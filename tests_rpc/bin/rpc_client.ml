open Core
open Async

let test_peer port : bool Deferred.t =
  let%bind () = Wrapper.put ("toto", "tutu") port in
  match%map Wrapper.get ("toto") port with
  | Some "tutu" ->  true
  | _ -> false

let process port () : unit Deferred.t = 
  Log.Global.set_level `Info;
  let%map res = test_peer port in
  ()

let () = 
  let spec =
    Command.Spec.(
      empty +> 
      flag "-p" (required int) ~doc:"port")
  in
  Command.async ~summary:"KVS RPC client." spec process |> Command.run 
