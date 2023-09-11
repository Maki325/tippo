(* let lexer =
   Tippo.Lexer.from_string "const a = 5;\nmut b = 7;\nb = 8;\n\nalpha_print a;" *)

let lexer = Tippo.Lexer.from_file "./examples/starting-point.tippo"

let rec print_token_until_invalid lexer =
  let token = Tippo.Lexer.next_token lexer in
  print_endline ("Token: " ^ Tippo.Token.to_string token);
  match token with
  | Invalid _ -> print_endline "Invalid!!!"
  | EOF _ -> ()
  | _ -> print_token_until_invalid lexer
;;

print_token_until_invalid lexer
