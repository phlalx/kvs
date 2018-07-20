open Core
open Async

let bye () : unit =
   don't_wait_for (after (sec 1.0) >>= fun () -> exit 0)

let terminate () : unit Deferred.t = exit 0

type state = { mutable cur_view : int; views : Types.view Array.t }

let state =
  let f i = Types.{ viewnum = i; primary = ""; backup = "" } in
  {
    cur_view = 0;
    views = Array.init 100 ~f
  }

let ping (viewnum, host) : Types.view = 
  Log.Global.info "Received ping";
  if viewnum = 0 && state.cur_view = 0 then (
    state.cur_view <- state.cur_view + 1;
    state.views.(state.cur_view).primary <- host 
  ) else if viewnum = 0 && state.cur_view = 1 then (
    state.cur_view <- state.cur_view + 1;
    state.views.(state.cur_view).backup <- host 
  ) else if viewnum < state.cur_view then (
    
  ) else (
    assert false
  );
  state.views.(state.cur_view)

 
let get () : Types.view =
  Log.Global.info "Received get";
  state.views.(state.cur_view)

(* The list of RPC implementations supported by this server *)
let implementations =
  [
    Rpc.Rpc.implement Protocol.terminate_rpc (fun () -> terminate);
    Rpc.Rpc.implement' Protocol.get_rpc (fun () -> get);
    Rpc.Rpc.implement' Protocol.ping_rpc (fun () -> ping);
  ]

  let process (rpc_port : int) : unit Deferred.t = 
    Signal.handle Signal.terminating ~f:(fun _ -> terminate () |> don't_wait_for);
    Rpc_server.start_server ~env:() ~port:rpc_port ~implementations ()

let () = 
  let spec =
    Command.Spec.(
    empty +> 
    flag "-p" (required int) ~doc:" set RPC port")
  in
  let command =
    Command.async ~summary:"Viewservice." spec
      (fun rpc_port () -> process rpc_port)
  in
  Command.run command

