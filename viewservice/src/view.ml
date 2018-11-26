open Core

type t = 
  { viewnum : int; 
    primary : string; 
    backup : string } 
[@@deriving bin_io, sexp_of, of_sexp]

let init_view = { viewnum = 0; primary = ""; backup = ""}

let update_primary v primary = { v with viewnum = v.viewnum + 1; primary}

let update_backup v backup = { v with viewnum = v.viewnum + 1; backup}

let primary t = t.primary

let backup t = t.backup

let to_string t = t |> sexp_of_t |> Sexp.to_string  