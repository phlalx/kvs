open Core

type t = 
  | Leaf of int
  | Node of t * t

val parse : string -> t

val to_string : t -> string