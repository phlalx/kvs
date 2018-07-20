open Core
open Async

let check cl p b n =
  let open Types in 
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
  let action n =
    if n = 0 then
      `Finished () |> return
    else
      let%bind view = Clerk.ping cl1 0 in (
        if view.Types.primary = cl1.Clerk.client then
          `Finished () |> return
        else 
          let%map () = Clock.after Types.ping_interval in
          `Repeat (n-1) )
  in
  Log.Global.info "Test: First primary 2 ..."; 
  let n = Types.dead_ping * 2 in
  let%bind () = Deferred.repeat_until_finished n action in
  let%map () = check cl1 cl1.Clerk.client "" 1 in
  Log.Global.info "Test: ... Passed"

let test_first_backup (cl1, cl2, _) : unit Deferred.t =
  let%bind view = Clerk.get cl1 in 
  let action n =
    if n = 0 then
      `Finished () |> return
    else
      let%bind _ = Clerk.ping cl1 1 in 
      let%bind view = Clerk.ping cl2 0 in (
        if view.Types.backup = cl2.Clerk.client then
          `Finished () |> return
        else 
          let%map () = Clock.after Types.ping_interval in
          `Repeat (n-1) )
  in
  Log.Global.info "Test: First backup ..."; 
  let n = Types.dead_ping * 2 in
  let%bind () = Deferred.repeat_until_finished n action in
  let%map () = check cl1 cl1.Clerk.client cl2.Clerk.client 2 in
  Log.Global.info "Test: ... Passed"

let test_viewservice port =
  let cl1 = Clerk.create "1" port in
  let cl2 = Clerk.create "2" port in
  let cl3 = Clerk.create "3" port in
  let ctx = cl1, cl2, cl3 in
  test_no_primary ctx 
  >>= fun () ->
  test_first_primary ctx 
  >>= fun () ->
  test_first_backup ctx 

let timeout = sec 1.0

let process port () : unit Deferred.t = 
  Log.Global.set_level `Info;
  let%map res = test_viewservice port in
  ()

let () = 
  let spec =
    Command.Spec.(
      empty +> 
      flag "-p" (required int) ~doc:"port")
  in
  Command.async ~summary:"Viewservice test." spec process |> Command.run 
