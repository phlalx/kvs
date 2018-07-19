open Core
open Async

val ping : viewnum:int -> host:string -> int -> Types.view Deferred.t
 
val terminate : int -> unit Deferred.t

