open Core
open Async


let put = Client.wrapper Protocol.put_rpc

let get = Client.wrapper Protocol.get_rpc

let terminate = Client.wrapper Protocol.terminate_rpc () 