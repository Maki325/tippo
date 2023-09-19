let lexer = Tippo.Lexer.from_file "./examples/3-arithmetics-multiple.tippo"
let program = Tippo.Parser.parse_program lexer

let _run_log () =
  Tippo.Utils.print_source
    (List.map (fun ast -> Tippo.Ast.sexp_of_t ast) program.ast);
  Tippo.Typechecker.typecheck program program.ast;
  Tippo.Utils.print_source (Tippo.Program.sexp_of_t program :: [])

let _run () =
  Tippo.Typechecker.typecheck program program.ast;
  Tippo.Compiler.compile program "./examples/out/3-arithmetics-multiple"
;;

_run ()
