open Core
open Async

let test_peer port : bool Deferred.t =
  let%bind () = Wrapper.put ("toto", "tutu") port in
  match%map Wrapper.get ("toto") port with
  | Some "tutu" ->  true
  | _ -> false

let timeout = sec 1.0

(* this function must be defined somewhere *)
let repeat_until_event 
    (event:'a Deferred.t) 
    (f: unit -> 'a Deferred.t) : 
    'a Deferred.t =
  let keepgoing = ref true in 
  upon event (fun () -> keepgoing := false);
  let f' () = 
    match !keepgoing with
    | true -> f () >>| fun () -> (`Repeat () )
    | false -> `Finished ()  |> return
  in 
  Deferred.repeat_until_finished () f' 

let test_many port = 
  let timeout_event = Clock.after timeout in
  repeat_until_event 
     timeout_event 
     (fun () -> Deferred.ignore (test_peer port))

let process port () : unit Deferred.t = 
  Log.Global.set_level `Info;
  let%map res = test_many port in
  ()

let () = 
  let spec =
    Command.Spec.(
      empty +> 
      flag "-p" (required int) ~doc:"port")
  in
  Command.async ~summary:"KVS RPC client." spec process |> Command.run 