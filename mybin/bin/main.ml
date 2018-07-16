open Core

let process _ s () =
   let ast = Mymodule.parse s in
   Printf.printf !"%{Mymodule}" ast

let () = 
  let spec =
    Command.Spec.(
    empty +> 
    flag "-v" (optional bool) ~doc:" Optional boolean." +> 
    anon ("expression" %: string))
  in
  Command.basic_spec ~summary:"Bla bla." spec process |> Command.run 