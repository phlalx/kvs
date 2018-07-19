(* Functions used by the client. TODO Why no error case? *)

open Core
open Async

val put : string * string -> int -> unit Deferred.t

val get : string -> int -> string option Deferred.t

val terminate : int -> unit Deferred.t