open Core
open Async

(** The protocol for communicating between the hello client and server.

    The [bin_query] and [bin_response] arguments are values that contain logic
    for binary serialization of the query and response types. 

    The version number is used when you want to mint new versions of an RPC
    without disturbing older versions.
*)

let terminate_rpc = Rpc.Rpc.create
  ~name:"terminate"
  ~version:0
  ~bin_query:Unit.bin_t
  ~bin_response:Unit.bin_t

type int_string = int * string [@@deriving bin_io]

let ping_rpc = Rpc.Rpc.create 
  ~name:"ping"
  ~version:0
  ~bin_query:bin_int_string
  ~bin_response:Types.bin_view