open Core
open Async

let bye () : unit =
   don't_wait_for (after (sec 1.0) >>= fun () -> exit 0)

let terminate () : unit Deferred.t = exit 0

let table = Hashtbl.Poly.create ()

let my_put (x, y) = 
  Log.Global.info "Put: map[%s] <- %s" x y;
  Hashtbl.add table ~key:x ~data:y |> ignore

let my_get x =
  let y = Hashtbl.find table x in
  let y_str = 
    match y with
    | None -> "None"
    | Some t -> t
  in
  Log.Global.info "Get: map[%s] -> %s" x y_str;
  y

(* The list of RPC implementations supported by this server *)
let implementations =
  [ Rpc.Rpc.implement' Protocol.put_rpc (fun () -> my_put);
    Rpc.Rpc.implement' Protocol.get_rpc (fun () -> my_get);
    Rpc.Rpc.implement Protocol.terminate_rpc (fun () -> terminate);
  ]

  let start ~port ~vs_port =
    Signal.handle Signal.terminating ~f:(fun _ -> terminate () |> don't_wait_for);
    Rpc_common.Server.start ~env:() ~port ~implementations ()