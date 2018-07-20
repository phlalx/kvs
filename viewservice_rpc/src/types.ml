open Core

let ping_interval = sec 1.0
let dead_ping = 5 

type view = 
  { viewnum : int; 
    mutable primary : string; 
    mutable backup : string } 
[@@deriving bin_io]