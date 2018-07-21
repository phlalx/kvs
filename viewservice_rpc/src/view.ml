open Core

type t = 
  { viewnum : int; 
    mutable primary : string; 
    mutable backup : string } 
[@@deriving bin_io, sexp_of, of_sexp]

let to_string t = t |> sexp_of_t |> Sexp.to_string  