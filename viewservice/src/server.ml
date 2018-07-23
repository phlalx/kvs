open Core
open Async
open Rpc_common

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

let start ~port =
  (* TODO get rid of this *)
  Signal.handle Signal.terminating ~f:(fun _ -> terminate () |> don't_wait_for);
  Rpc_common.Server.start ~env:() ~port ~implementations ()

