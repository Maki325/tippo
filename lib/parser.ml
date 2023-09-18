let parse_int token value : Ast.t = Ast.Lit { token; value = Ast.Int value }

let rec parse_program ?(list = []) lexer =
  let token = Lexer.next_token lexer in
  match token with
  | Token.EOF _ -> Program.from_ast (List.rev list)
  | _ ->
      let list = parse lexer ?token:(Some token) :: list in
      parse_program lexer ?list:(Some list)

and parse ?token lexer : Ast.t =
  let token = match token with Some t -> t | None -> Lexer.next_token lexer in
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
  | Token.Semicolon _ -> parse lexer
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
  | _ ->
      raise
        (Exceptions.UnexpectedToken
           { expected = TokenType.Semicolon; got = next })

and parse_ident lexer ident name =
  let next = Lexer.peek_token lexer in
  match next with
  | Token.Semicolon _ -> Ast.Ident { token = ident; name }
  | Token.Assign _ ->
      ignore (Lexer.next_token lexer);
      let value = parse lexer in
      let semicolon = Lexer.expect_token lexer TokenType.Semicolon in
      Ast.Assign
        { ident = { token = ident; name }; eq = next; value; semicolon }
  | _ ->
      raise
        (Exceptions.UnexpectedToken
           { expected = TokenType.Semicolon; got = next })
