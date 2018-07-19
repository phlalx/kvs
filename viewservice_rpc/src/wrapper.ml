open Core
open Async

let terminate = Client.wrapper Protocol.terminate_rpc () 

let ping ~viewnum ~host = Client.wrapper Protocol.ping_rpc (viewnum, host)