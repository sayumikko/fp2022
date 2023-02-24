open Bash_lib
open Parser
open Interpreter.Interpret (Interpreter.Result)
open Ast

let rec run ctx =
  let interpret_parsed ast =
    let inner_ast = function Declarations a -> a in
    match interpret_bash ctx (inner_ast ast) with
    | Error e ->
        print_endline ("INTERPRETER ERROR: " ^ e);
        run ctx
    | Ok new_ctx -> run new_ctx
  in
  let input = parse (ast_parser ()) (read_line ()) in
  match input with
  | Error e ->
      print_endline ("PARSER ERROR: " ^ e);
      run ctx
  | Ok b ->
      print_endline "";
      interpret_parsed b

let () =
  print_endline "bash: (Ctrl+D to enter) (Ctrl+C to stop)";
  run empty_ctx
