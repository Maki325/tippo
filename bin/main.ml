open Sexplib

let lexer = Tippo.Lexer.from_file "./examples/starting-point.tippo"
let program = Tippo.Parser.parse_program lexer

let print_source ?(channel = stdout) sexps =
  let formatter = Format.formatter_of_out_channel channel in
  List.iter (fun s -> Sexp.pp_hum formatter s) sexps;
  Format.pp_print_flush formatter ();
  print_newline ()
;;

print_source (List.map (fun ast -> Tippo.Ast.sexp_of_t ast) program)
