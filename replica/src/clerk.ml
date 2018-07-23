open Core
open Async
open Rpc_common
 
type t = {
  port : int;
}

let create ~port = { port }

(* TODO use labels *)
let put t (key, value) = Client.wrapper Protocol.put_rpc (key, value) t.port

let get t key = Client.wrapper Protocol.get_rpc key t.port

let terminate t = Client.wrapper Protocol.terminate_rpc () t.port