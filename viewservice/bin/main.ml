open Core
open Async

let bye () : unit =
   don't_wait_for (after (sec 1.0) >>= fun () -> exit 0)

let terminate () : unit Deferred.t = 
  Log.Global.info "VS: terminate";
  exit 0

let ping (viewnum, host) : View.t = 
  let open State in
  Log.Global.info "VS: Received ping %d from %s" viewnum host;
  State.ping ~viewnum ~host;
  let cur_view = State.current_view () in
  Log.Global.info !"VS: Ping return view %{View}" cur_view;
  cur_view
 
let get () : View.t =
  Log.Global.info "VS: Received get";
  let cur_view = State.current_view () in
  Log.Global.info !"VS: Get return view %{View}" cur_view;
  cur_view

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

