open Core
open Async

type t

val create : client:string -> port:int -> t

val ping : t -> viewnum:int -> Types.view Deferred.t
 
val terminate : t -> unit Deferred.t

