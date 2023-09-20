let lexer = Tippo.Lexer.from_file "./examples/4-arithmetic-parentheses.tippo"
let program = Tippo.Parser.parse_program lexer

let _run_log () =
  print_endline "Program source:";
  Tippo.Utils.print_source
    (List.map (fun ast -> Tippo.Ast.sexp_of_t ast) program.ast);
  Tippo.Typechecker.typecheck program program.ast;
  print_endline "Typechecked Program source:";
  Tippo.Utils.print_source (Tippo.Program.sexp_of_t program :: []);
  Tippo.Compiler.compile program "./examples/out/4-arithmetic-parentheses"

let _run () =
  Tippo.Typechecker.typecheck program program.ast;
  Tippo.Compiler.compile program "./examples/out/4-arithmetic-parentheses"
;;

_run ()
