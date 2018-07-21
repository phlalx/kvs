open Core
open Async

val terminate_rpc : (unit, unit) Rpc.Rpc.t

val ping_rpc : (int * string, View.t) Rpc.Rpc.t

val get_rpc : (unit, View.t) Rpc.Rpc.t