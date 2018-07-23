open Core
open Async

val put_rpc : (string * string, unit) Rpc.Rpc.t

val get_rpc : (string, string option) Rpc.Rpc.t

val terminate_rpc : (unit, unit) Rpc.Rpc.t