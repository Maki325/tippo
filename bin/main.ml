open Sexplib

let lexer = Tippo.Lexer.from_file "./examples/starting-point.tippo"
let program = Tippo.Parser.parse_program lexer

let _run_log () =
  let print_source ?(channel = stdout) sexps =
    let formatter = Format.formatter_of_out_channel channel in
    List.iter (fun s -> Sexp.pp_hum formatter s) sexps;
    Format.pp_print_flush formatter ();
    print_newline ()
  in

  print_source (List.map (fun ast -> Tippo.Ast.sexp_of_t ast) program.ast);
  Tippo.Typechecker.typecheck program program.ast;
  print_source (Tippo.Program.sexp_of_t program :: [])

let _run () =
  Tippo.Typechecker.typecheck program program.ast;
  Tippo.Compiler.compile program "./examples/starting-point"
;;

_run ()
