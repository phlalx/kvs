open Core
open Async

let start_server ~env ?(stop=Deferred.never ()) ~implementations ~port () =
  Log.Global.info "RPC_server: starting server on %d" port;
  let implementations =
    Rpc.Implementations.create_exn ~implementations
      ~on_unknown_rpc:(`Call (fun _ ~rpc_tag ~version ->
          Log.Global.info "Unexpected RPC, tag %s, version %d" rpc_tag version;
          `Continue
        ))
  in
  let%bind server = 
    Tcp.Server.create
      ~on_handler_error:(`Call (fun _ exn -> Log.Global.sexp [%sexp (exn : Exn.t)]))
      (Tcp.on_port port)
      (fun _addr r w ->
         Rpc.Connection.server_with_close r w
           ~connection_state:(fun _ -> env)
           ~on_handshake_error:(
             `Call (fun exn -> Log.Global.sexp [%sexp (exn : Exn.t)]; return ()))
           ~implementations
      )
  in
  Log.Global.info "Server started, waiting for close";
  Deferred.any
    [ (stop >>= fun () -> Tcp.Server.close server)
    ; Tcp.Server.close_finished server ]