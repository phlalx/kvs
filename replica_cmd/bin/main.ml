open Core
open Async

let () = 
  let spec =
    Command.Spec.(
    empty +> 
    flag "-p" (required int) ~doc:" set port" +>
    flag "-v" (required int) ~doc:" set viewservice port"
    )
  in
  let command =
    Command.async ~summary:"KVS RPC server." spec
      (fun port vs_port () -> Replica.Server.start ~port ~vs_port)
  in
  Command.run command
