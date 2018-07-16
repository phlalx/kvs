open Core

type t = 
  | Leaf of int
  | Node of t * t
  [@@deriving sexp_of, of_sexp]

let to_string t = t |> sexp_of_t |> Sexp.to_string 

let parse s = t_of_sexp (Sexp.of_string s)