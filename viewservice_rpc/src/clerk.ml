open Core
open Async

type t = {
  client : string;
  port : int;
}

let create ~client ~port = { client; port }

let terminate t = Client.wrapper Protocol.terminate_rpc () t.port

let ping t ~viewnum = Client.wrapper Protocol.ping_rpc (viewnum, t.client) t.port