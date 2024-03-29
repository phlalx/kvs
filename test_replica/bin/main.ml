open Core
open Async

module VSClerk = Viewservice.Clerk
module RClerk = Replica.Clerk

let test_peer port : bool Deferred.t =
  let cl = Replica.Clerk.create port in
  let%bind () = Replica.Clerk.put cl ("toto", "tutu") in
  match%map Replica.Clerk.get cl ("toto") with
  | Some "tutu" ->  true
  | _ -> false


let process _ () : unit Deferred.t = 
  Log.Global.set_level `Info;
  (* hardcode ports for the moment TODO *)
  let vs_port = 8000 in
  let replica_port = 8001  in

  don't_wait_for (Viewservice.Server.start ~port:vs_port);
  let%bind () = Clock.after (sec 0.5) in
  let replica = Replica.Server.create ~port:replica_port ~vs_port in
  don't_wait_for (Replica.Server.start replica);
  let%bind () = Clock.after (sec 0.5) in
  let vck = VSClerk.create "test_replica" vs_port in
  (* let ck = RClerk.create replica_port in *)
  let%bind () = Clock.after (sec 10.) in (* TODO magic constant *)
  let%bind primary = VSClerk.primary vck in
  assert (primary = Replica.Server.name replica);
  Deferred.never ()

let () = 
  let spec =
    Command.Spec.(
      empty +> 
      flag "-p" (required int) ~doc:"port")
  in
  Command.async ~summary:"Replica test." spec process |> Command.run 
