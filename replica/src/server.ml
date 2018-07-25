open Core
open Async

module VSClerk = Viewservice.Clerk

type t = {
  port : int;
  name : string;
  vsclerk : VSClerk.t;
  stop : unit Ivar.t;
  table : (string, string) Hashtbl.t;
}

let kill t : unit = Ivar.fill t.stop ()

let terminate t : unit Deferred.t = 
    return (kill t)

let my_put t (x, y) = 
  Log.Global.info "Put: map[%s] <- %s" x y;
  Hashtbl.add t.table ~key:x ~data:y |> ignore

let my_get t x =
  let y = Hashtbl.find t.table x in
  let y_str = 
    match y with
    | None -> "None"
    | Some t -> t
  in
  Log.Global.info "Get: map[%s] -> %s" x y_str;
  y

  let ping vsclerk () = 
     VSClerk.ping vsclerk ~viewnum:0
     |> Deferred.ignore 
     |> don't_wait_for

  let create ~port ~vs_port =
    let stop = Ivar.create () in
    let name = Printf.sprintf "replica/%d" port in
    let vsclerk = VSClerk.create name vs_port in
    let table = Hashtbl.Poly.create () in
    { port; name; vsclerk; stop; table } 

let name t = t.name

  let start t =
    let implementations =
      [ Rpc.Rpc.implement' Protocol.put_rpc (fun () -> my_put t);
        Rpc.Rpc.implement' Protocol.get_rpc (fun () -> my_get t);
        Rpc.Rpc.implement Protocol.terminate_rpc (fun () () -> terminate t);
      ]
    in
    let stop = Ivar.read t.stop in
    Clock.run_at_intervals ~stop (sec 1.) (ping t.vsclerk);
    Rpc_common.Server.start ~stop ~env:() ~port:t.port ~implementations ()