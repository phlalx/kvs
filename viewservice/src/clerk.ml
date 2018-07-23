open Core
open Async
open Rpc_common

type t = {
  client : string;
  port : int;
}

let create ~client ~port = { client; port }

let terminate t = Rpc_common.Client.wrapper Protocol.terminate_rpc () t.port

let ping t ~viewnum = Client.wrapper Protocol.ping_rpc (viewnum, t.client) t.port

let get t = Client.wrapper Protocol.get_rpc () t.port

let primary t = 
  let%map v = get t in
  v.primary
