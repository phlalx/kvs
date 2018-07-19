open Core
open Async

let test_viewservice port =
  let%map view = Wrapper.ping 0 "me" port in
  view

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
