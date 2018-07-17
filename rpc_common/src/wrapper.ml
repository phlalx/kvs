open Core
open Async

let host = "localhost"

let with_rpc_conn f ~host ~port =
  Tcp.with_connection
    (Tcp.to_host_and_port host port)
    ~timeout:(sec 1.)
    (fun _ r w ->
       match%bind Rpc.Connection.create r w ~connection_state:(fun _ -> ()) with
       | Error exn -> raise exn
       | Ok conn   -> f conn
    )

let wrapper f arg port =
  with_rpc_conn (fun conn -> Rpc.Rpc.dispatch_exn f conn arg)
    ~host ~port

let put = wrapper Protocol.put_rpc

let get = wrapper Protocol.get_rpc

let terminate = wrapper Protocol.terminate_rpc () 