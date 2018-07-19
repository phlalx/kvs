open Core
open Async

val terminate_rpc : (unit, unit) Rpc.Rpc.t

val ping_rpc : (int * string, Types.view) Rpc.Rpc.t

val get_rpc : (unit, Types.view) Rpc.Rpc.t