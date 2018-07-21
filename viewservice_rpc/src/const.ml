open Core

let ping_interval = sec 1.0
let dead_ping = 5 
let ping_timeout = Time.Span.scale ping_interval (Float.of_int dead_ping)
