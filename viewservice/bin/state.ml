open Core
open Async

type state = { 
  mutable cur_view : int; 
  views : View.t Array.t; (* TODO use dynamic structure *)
  last_heard : (string, Time.t) Hashtbl.Poly.t;
}

let state =
  let f i = View.{ viewnum = i; primary = ""; backup = "" } in
  {
    cur_view = 0;
    views = Array.init 100 ~f;
    last_heard = Hashtbl.Poly.create ();
  }

let current_view () = state.views.(state.cur_view)

let heard_replica (p:string) : unit = 
  Hashtbl.set state.last_heard ~key:p ~data:(Time.now ())

let fail replica =
  Log.Global.info "VS: fail %s" replica;
  let primary = state.views.(state.cur_view).primary in
  let backup = state.views.(state.cur_view).backup in
  state.cur_view <- state.cur_view + 1;
  if replica = primary then ( 
    Log.Global.info "VS: primary is dead";
    state.views.(state.cur_view).primary <- backup;
    state.views.(state.cur_view).backup <- ""
  ) else if replica = backup then (
    Log.Global.info "VS: backup is dead";
    state.views.(state.cur_view).primary <- primary;
    state.views.(state.cur_view).backup <- ""
  ) 

let setup_primary p =
  state.cur_view <- state.cur_view + 1;
  state.views.(state.cur_view).primary <- p;
  state.views.(state.cur_view).backup <- state.views.(state.cur_view-1).backup 

let setup_backup p =
  state.cur_view <- state.cur_view + 1;
  state.views.(state.cur_view).primary <- state.views.(state.cur_view-1).primary;
  state.views.(state.cur_view).backup <- p

let () = 
  let incr_tick () = 
    let replicas = 
      let v = state.views.(state.cur_view) in 
      [ v.primary; v.backup ] |> List.filter ~f:(fun x -> x <> "") 
    in 
    let f replica = 
      let last_heard = Hashtbl.find_exn state.last_heard replica in
      let t = Time.add last_heard Const.ping_timeout in
      if Time.(t < (Time.now ())) then
        fail replica
    in
    List.iter replicas ~f 
  in
  Clock.every Const.ping_interval incr_tick

let ping ~viewnum ~host =
  heard_replica host;
  if viewnum = 0 && state.cur_view = 0 then setup_primary host
  else if viewnum = 0 && state.cur_view = 1 then setup_backup host
  else if viewnum <= state.cur_view then () 
  else assert false