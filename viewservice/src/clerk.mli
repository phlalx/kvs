open Core
open Async

type t = {
  client : string;
  port : int;
}

val create : client:string -> port:int -> t

val ping : t -> viewnum:int -> View.t Deferred.t

val get : t -> View.t Deferred.t

val primary : t -> string Deferred.t 
 
val terminate : t -> unit Deferred.t

