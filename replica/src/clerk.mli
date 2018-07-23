(* Functions used by the client. TODO Why no error case? *)

open Core
open Async

type t = {
  port : int;
}

val create : port:int -> t

val put : t -> string * string -> unit Deferred.t

val get : t -> string -> string option Deferred.t

val terminate : t -> unit Deferred.t