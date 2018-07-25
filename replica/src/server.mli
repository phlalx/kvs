open Core
open Async

type t

val create : port:int -> vs_port:int -> t

val start : t -> unit Deferred.t 

val name : t -> string

val kill : t -> unit