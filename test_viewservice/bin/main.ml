open Core
open Async

let test_viewservice port =
  let cl = Clerk.create "me" port in
  let%map view = Clerk.ping cl port in
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
