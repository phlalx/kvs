open Core
open Async

let check cl p b n =
  let open View in 
  let%map {viewnum; primary; backup} = Clerk.get cl in
  if viewnum <> n then (
    Log.Global.info "Test: wanted viewnum %d, got %d" n viewnum;
    assert false
  ) else if primary <> p then (
    Log.Global.info "Test: wanted primary %s, got %s" p primary;
    assert false
  ) else if backup <> b then (
    Log.Global.info "Test: wanted backup %s, got %s" b backup;
    assert false
  )

let test_no_primary (cl1, _, _) = 
  Log.Global.info "Test: First primary 1 ..."; 
  match%map Clerk.primary cl1 with
  | "" -> Log.Global.info "Test:  ... Passed"
  | _ -> failwith "Test: there was a primary too soon"

let test_first_primary (cl1, _, _) =
  Log.Global.info "Test: First primary 2 ..."; 
  let action () =
    let%bind view = Clerk.ping cl1 0 in 
    if view.View.primary = cl1.Clerk.client then
      `Finished () |> return
    else 
      let%map () = Clock.after Const.ping_interval in
      `Repeat () 
  in
  let timeout = Time.Span.scale Const.ping_timeout 2.0 in
  let%bind _ = Clock.with_timeout timeout (Deferred.repeat_until_finished () action) in
  let%map () = check cl1 cl1.Clerk.client "" 1 in
  Log.Global.info "Test: ... Passed"

let test_first_primary (cl1, _, _) =
  Log.Global.info "Test: First primary 2 ..."; 
  let action () =
    let%bind view = Clerk.ping cl1 0 in 
    if view.View.primary = cl1.Clerk.client then
      `Finished () |> return
    else 
      let%map () = Clock.after Const.ping_interval in
      `Repeat () 
  in
  let timeout = Time.Span.scale Const.ping_timeout 2.0 in
  let%bind _ = Clock.with_timeout timeout (Deferred.repeat_until_finished () action) in
  let%map () = check cl1 cl1.Clerk.client "" 1 in
  Log.Global.info "Test: ... Passed"

let test_first_backup (cl1, cl2, _) : unit Deferred.t =
  Log.Global.info "Test: First backup ..."; 
  let%bind view = Clerk.get cl1 in 
  let action () =
    let%bind _ = Clerk.ping cl1 1 in 
    let%bind view = Clerk.ping cl2 0 in (
      if view.View.backup = cl2.Clerk.client then
        `Finished () |> return
      else 
        let%map () = Clock.after Const.ping_interval in
        `Repeat () )
  in
  let timeout = Time.Span.scale Const.ping_timeout 2.0 in
  let%bind _ = Clock.with_timeout timeout (Deferred.repeat_until_finished () action) in
  let%map () = check cl1 cl1.Clerk.client cl2.Clerk.client 2 in
  Log.Global.info "Test: ... Passed"

let test_primary_dies (cl1, cl2, _) : unit Deferred.t =
  Log.Global.info "Test: primary dies ..."; 
  let%bind _ = Clerk.ping cl1 2 in 
  let%bind view = Clerk.ping cl2 2 in 
  let action () =
    let%bind view = Clerk.ping cl2 view.viewnum in 
    if view.primary = cl2.Clerk.client 
    && view.backup = "" then
      `Finished () |> return
    else 
      let%map () = Clock.after Const.ping_interval in
      `Repeat () 
  in
  let timeout = Time.Span.scale Const.ping_timeout 2.0 in
  let%bind _ = Clock.with_timeout timeout (Deferred.repeat_until_finished () action) in
  let%map () = check cl2 cl2.Clerk.client "" (view.viewnum + 1) in
  Log.Global.info "Test: ... Passed"

let test_restarted_backup (cl1, cl2, _) : unit Deferred.t =
  Log.Global.info "Test: Restarted server becomes backup ..."; 
  let%bind view = Clerk.get cl2 in 
  let%bind _ : View.t = Clerk.ping cl2 view.viewnum in 
  let action () =
    let%bind _ = Clerk.ping cl1 0 in 
    let%bind v = Clerk.ping cl2 view.viewnum in 
    if v.primary = cl2.Clerk.client && v.backup = cl1.client then
      `Finished () |> return
    else 
      let%map () = Clock.after Const.ping_interval in
      `Repeat () 
  in
  let timeout = Time.Span.scale Const.ping_timeout 2.0 in
  let%bind _ = Clock.with_timeout timeout (Deferred.repeat_until_finished () action) in
  let%map () = check cl2 cl2.Clerk.client cl1.Clerk.client (view.viewnum + 1) in
  Log.Global.info "Test: ... Passed"

let test_idle_third_server (cl1, cl2, cl3) : unit Deferred.t =
  Log.Global.info "Test: Idle third server becomes backup if primary fails..."; 
  let%bind view = Clerk.get cl2 in 
  let%bind _ = Clerk.ping cl2 view.viewnum  in 
  let action () =
    let%bind _ = Clerk.ping cl3 0  in 
    let%bind v = Clerk.ping cl1 view.viewnum in 
    if v.primary = cl1.Clerk.client && v.backup = cl3.client then
      `Finished () |> return
    else 
      let%map () = Clock.after Const.ping_interval in
      `Repeat () 
  in
  let timeout = Time.Span.scale Const.ping_timeout 2.0 in
  let%bind _ = Clock.with_timeout timeout (Deferred.repeat_until_finished () action) in
  let%map () = check cl1 cl1.Clerk.client cl3.Clerk.client (view.viewnum + 2) in
  Log.Global.info "Test: ... Passed"

let test_restarted_primary (cl1, cl2, cl3) : unit Deferred.t =
  Log.Global.info "Test: Restarted primary treated as dead..."; 
  let%bind view = Clerk.get cl1 in 
  let%bind _ = Clerk.ping cl1 view.viewnum  in 
  let action () =
    let%bind _ = Clerk.ping cl1 0  in 
    let%bind _ = Clerk.ping cl3 view.viewnum in 
    let%bind v = Clerk.get cl3 in 
    if v.primary <> cl1.Clerk.client then
      `Finished () |> return
    else 
      let%map () = Clock.after Const.ping_interval in
      `Repeat () 
  in
  let timeout = Time.Span.scale Const.ping_timeout 2.0 in
  let%bind _ = Clock.with_timeout timeout (Deferred.repeat_until_finished () action) in
  let%map vy = Clerk.get cl3 in
  assert (vy.primary = cl3.Clerk.client); (* TODO *)
  Log.Global.info "Test: ... Passed"

let set_up_view_with_three_as_primary (cl1, cl2, cl3) =
  let action () =
    let%bind v = Clerk.get cl3  in 
    let%bind _ = Clerk.ping cl3 v.viewnum in 
    let%map () = Clock.after Const.ping_interval in
    `Repeat () 
  in
  let timeout = Time.Span.scale Const.ping_timeout 3.0 in
  let%bind _ = Clock.with_timeout timeout (Deferred.repeat_until_finished () action) in
  let%map vy = Clerk.get cl3 in
  assert (vy.primary = cl3.Clerk.client && vy.backup = "") (* TODO *)

let test_vs_waits_for_primary_to_ack_view_1 (cl1, cl2, cl3) : unit Deferred.t =
  Log.Global.info "Test: Viewserver waits for primary to ack view 1/2..."; 
  let%bind view = Clerk.get cl1 in 
  let action () =
    let%bind _ = Clerk.ping cl1 0  in 
    let%bind _ = Clerk.ping cl3 view.viewnum in 
    let%bind v = Clerk.get cl1 in 
    if v.viewnum > view.viewnum then
      `Finished () |> return
    else 
      let%map () = Clock.after Const.ping_interval in `Repeat () 
  in
  let timeout = Time.Span.scale Const.ping_timeout 3.0 in
  let%bind _ = Clock.with_timeout timeout (Deferred.repeat_until_finished () action) in
  let%map () = check cl1 cl3.Clerk.client cl1.Clerk.client (view.viewnum + 1) in
  Log.Global.info "Test: ... Passed"

let test_vs_waits_for_primary_to_ack_view_2 (cl1, cl2, cl3) : unit Deferred.t =
  Log.Global.info "Test: Viewserver waits for primary to ack view 2/2..."; 
  let%bind view = Clerk.get cl1 in 
  let action () =
      let%bind v = Clerk.ping cl1 view.viewnum in 
      if v.viewnum > view.viewnum then
        `Finished () |> return
      else 
        let%map () = Clock.after Const.ping_interval in `Repeat () 
  in
  let timeout = Time.Span.scale Const.ping_timeout 3.0 in
  let%bind _ = Clock.with_timeout timeout (Deferred.repeat_until_finished () action) in
  let%map () = check cl2 cl3.Clerk.client cl1.Clerk.client view.viewnum in
  Log.Global.info "Test: ... Passed"

let test_uninitialized_server (cl1, cl2, cl3) : unit Deferred.t =
  Log.Global.info "Test: uninitialized server can't become primary..."; 
  let action () =
    let%bind v = Clerk.get cl1 in 
    let%bind _ = Clerk.ping cl1 v.viewnum in 
    let%bind _ = Clerk.ping cl2 0 in 
    let%bind _ = Clerk.ping cl3 v.viewnum in 
    let%map () = Clock.after Const.ping_interval in
    `Repeat () 
  in let action2 () =
       let%bind _ = Clerk.ping cl2 0 in 
       let%map () = Clock.after Const.ping_interval in
       `Repeat () 
  in
  let timeout = Time.Span.scale Const.ping_timeout 2.0 in
  let%bind _ = Clock.with_timeout timeout (Deferred.repeat_until_finished () action) in
  let%bind _ = Clock.with_timeout timeout (Deferred.repeat_until_finished () action2) in
  let%map vz = Clerk.get cl2 in 
  assert (vz.primary <> cl2.client);
  Log.Global.info "Test: ... Passed"

let test_viewservice port =
  let cl1 = Clerk.create "1" port in
  let cl2 = Clerk.create "2" port in
  let cl3 = Clerk.create "3" port in
  let ctx = cl1, cl2, cl3 in
  let%bind () = test_no_primary ctx in
  let%bind () = test_first_primary ctx in
  let%bind () = test_first_backup ctx in
  let%bind () = test_primary_dies ctx  in
  let%bind () = test_restarted_backup ctx  in
  let%bind () = test_idle_third_server ctx  in
  let%bind () = test_restarted_primary ctx  in
  let%bind () = set_up_view_with_three_as_primary ctx in
  let%bind () = test_vs_waits_for_primary_to_ack_view_1 ctx in
  let%bind () = test_vs_waits_for_primary_to_ack_view_2 ctx in 
  test_uninitialized_server ctx

let process port () : unit Deferred.t = 
  Log.Global.set_level `Info;
  let%map () = test_viewservice port in
  ()

let () = 
  let spec =
    Command.Spec.(
      empty +> 
      flag "-p" (required int) ~doc:"port")
  in
  Command.async ~summary:"Viewservice test." spec process |> Command.run 
