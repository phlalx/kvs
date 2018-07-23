open Core
open Async

let () = 
  let spec =
    Command.Spec.(
    empty +> 
    flag "-p" (required int) ~doc:" set port")
  in
  let command =
    Command.async ~summary:"Viewservice." spec
      (fun port () -> Viewservice.Server.start ~port)
  in
  Command.run command

