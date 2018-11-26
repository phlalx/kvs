open Core
open Async

type state = { 
  mutable cur_view : int; 
  views : View.t Array.t; (* TODO use dynamic structure *)
  last_heard : (string, Time.t) Hashtbl.Poly.t;
}

let state = {
    cur_view = 0;
    views = Array.create 100 View.init_view;
    last_heard = Hashtbl.Poly.create ();
  }

let current_view () = state.views.(state.cur_view)

let update_view_with_primary p = 
  state.views.(state.cur_view + 1) <- View.update_primary (current_view ()) p;
  state.cur_view <- state.cur_view + 1

let update_view_with_backup b = 
  state.views.(state.cur_view + 1) <- View.update_backup (current_view ()) b;
  state.cur_view <- state.cur_view + 1

let has_primary state = state.views.(state.cur_view).primary <> ""

let has_backup state = state.views.(state.cur_view).backup <> ""

let heard_replica (p:string) : unit = 
  Hashtbl.set state.last_heard ~key:p ~data:(Time.now ())

let fail replica =
  Log.Global.info "VS: fail %s" replica;
  let primary = View.primary (current_view ()) in
  let backup = View.backup (current_view ()) in
  if replica = primary then ( 
    Log.Global.info "VS: primary is dead";
    update_view_with_primary backup;
  ) else if replica = backup then (
    Log.Global.info "VS: backup is dead";
    update_view_with_backup ""
  ) 

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
  if viewnum = 0 && state.cur_view = 0 then update_view_with_primary host
  else if viewnum = 0 && View.primary (current_view ())  = host then fail host 
  else if viewnum = 0 && (has_primary state) && not (has_backup state) then update_view_with_backup host
  else if viewnum = 0 && state.views.(state.cur_view).backup = host then fail host 
  else if viewnum <= state.cur_view then () 
  else assert false