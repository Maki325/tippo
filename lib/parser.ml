let parse_int token value : Ast.t = Ast.Lit { token; value = Ast.Int value }

let rec parse_program ?(list = []) (lexer : Lexer.t) =
  let token = Lexer.next_token lexer in
  match token with
  | Token.EOF _ -> List.rev list
  | _ ->
      let list = parse lexer ?token:(Some token) :: list in
      parse_program lexer ?list:(Some list)

and parse ?token lexer : Ast.t =
  let token = match token with Some t -> t | None -> Lexer.next_token lexer in
  (* let token = Lexer.next_token lexer in *)
  (* print_endline (Token.to_string token); *)
  match token with
  | Token.Const _ -> parse_declaration lexer token false
  | Token.Mut _ -> parse_declaration lexer token true
  | Token.Int (_, value) -> parse_int token value
  | Token.Ident (_, value) -> parse_ident lexer token value
  | Token.AlphaPrint _ ->
      let ident = Lexer.expect_token lexer TokenType.Ident in
      let name =
        match ident with Token.Ident (_, value) -> value | _ -> assert false
      in
      Ast.AlphaPrint
        {
          token;
          ident = { token = ident; name };
          semicolon = Lexer.expect_token lexer TokenType.Semicolon;
        }
  | _ -> assert false

and parse_declaration lexer declaration is_mutable =
  let ident = Lexer.expect_token lexer TokenType.Ident in
  let name =
    match ident with Token.Ident (_, value) -> value | _ -> assert false
  in
  let next = Lexer.next_token lexer in
  match next with
  | Token.Semicolon _ ->
      Ast.Declare
        {
          ident = { token = ident; name };
          declaration = { token = declaration; is_mutable };
          semicolon = next;
        }
  | Token.Assign _ ->
      let value = parse lexer in
      let semicolon = Lexer.expect_token lexer TokenType.Semicolon in
      Ast.DeclareAssign
        {
          ident = { token = ident; name };
          declaration = { token = declaration; is_mutable };
          eq = next;
          value;
          semicolon;
        }
  | _ -> raise (Exceptions.UnexpectedToken (next, TokenType.Semicolon))

and parse_ident lexer ident name =
  let next = Lexer.next_token lexer in
  match next with
  | Token.Semicolon _ ->
      (* NOP - we can just return the next token *)
      parse lexer
  | Token.Assign _ ->
      let value = parse lexer in
      let semicolon = Lexer.expect_token lexer TokenType.Semicolon in
      Ast.Assing
        { ident = { token = ident; name }; eq = next; value; semicolon }
  | _ -> raise (Exceptions.UnexpectedToken (next, TokenType.Semicolon))
