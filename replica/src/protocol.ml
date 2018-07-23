open Core
open Async

(** The protocol for communicating between the hello client and server.

    The [bin_query] and [bin_response] arguments are values that contain logic
    for binary serialization of the query and response types. 

    The version number is used when you want to mint new versions of an RPC
    without disturbing older versions.
*)

type string_string = string * string [@@deriving bin_io]

let put_rpc = Rpc.Rpc.create
  ~name:"put"
  ~version:0
  ~bin_query:bin_string_string
  ~bin_response:Unit.bin_t

type string_option = string option[@@deriving bin_io]

let get_rpc = Rpc.Rpc.create
  ~name:"get"
  ~version:0
  ~bin_query:String.bin_t
  ~bin_response:bin_string_option

let terminate_rpc = Rpc.Rpc.create
  ~name:"terminate"
  ~version:0
  ~bin_query:Unit.bin_t
  ~bin_response:Unit.bin_t