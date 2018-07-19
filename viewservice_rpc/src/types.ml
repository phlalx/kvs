open Core

type view = 
  { viewnum : int; 
    primary : string; 
    backup : string } 
[@@deriving bin_io]