open Core
open Async

let () = 
  let spec =
    Command.Spec.(
    empty +> 
    flag "-p" (required int) ~doc:" set RPC port")
  in
  let command =
    Command.async ~summary:"KVS RPC server." spec
      (fun rpc_port () -> Replica.process rpc_port)
  in
  Command.run command

