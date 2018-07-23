open Async

val start
  :  env:'a
  -> ?stop : unit Deferred.t
  -> implementations:'a Rpc.Implementation.t list
  -> port:int
  -> unit
  -> unit Deferred.t