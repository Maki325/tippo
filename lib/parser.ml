exception ReachedEOF [@@deriving show { with_path = false }, sexp]

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
  | Token.Int (_, value) -> parse_int lexer token value
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
      let semicolon =
        match Lexer.is_last_token_of_type lexer TokenType.Semicolon with
        | `Same tok -> tok
        | `Different ->
            raise
              (Exceptions.UnexpectedToken
                 { expected = TokenType.Semicolon; got = next })
      in
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

and parse_binary_operation lexer left (binary_operation : Ast.binary_operation)
    =
  let right = parse lexer in
  match right with
  | Ast.BinaryOperation bo
    when BinaryOperation.priority bo.op.value
         < BinaryOperation.priority binary_operation.value ->
      Ast.BinaryOperation
        {
          left =
            Ast.BinaryOperation { left; op = binary_operation; right = bo.left };
          right = bo.right;
          op = bo.op;
        }
  | _ -> Ast.BinaryOperation { left; right; op = binary_operation }

and parse_int lexer token value : Ast.t =
  let next = Lexer.next_token lexer in
  let lit = Ast.Lit { token; value = Ast.Int value } in
  match next with
  | Token.Semicolon _ -> lit
  | Token.Plus _ ->
      let ast =
        parse_binary_operation lexer lit
          { token = next; value = BinaryOperation.Add }
      in
      Lexer.assert_last_token_of_type lexer TokenType.Semicolon;
      ast
  | Token.Minus _ ->
      let ast =
        parse_binary_operation lexer lit
          { token = next; value = BinaryOperation.Subtract }
      in
      Lexer.assert_last_token_of_type lexer TokenType.Semicolon;
      ast
  | Token.Star _ ->
      let ast =
        parse_binary_operation lexer lit
          { token = next; value = BinaryOperation.Multiply }
      in
      Lexer.assert_last_token_of_type lexer TokenType.Semicolon;
      ast
  | Token.Slash _ ->
      let ast =
        parse_binary_operation lexer lit
          { token = next; value = BinaryOperation.Divide }
      in
      Lexer.assert_last_token_of_type lexer TokenType.Semicolon;
      ast
  | _ ->
      raise
        (Exceptions.UnexpectedToken
           { expected = TokenType.Semicolon; got = next })

and parse_ident lexer ident name =
  let next = Lexer.next_token lexer in
  match next with
  | Token.Semicolon _ -> Ast.Ident { token = ident; name }
  | Token.Assign _ ->
      let value = parse lexer in
      let semicolon =
        match Lexer.is_last_token_of_type lexer TokenType.Semicolon with
        | `Same tok -> tok
        | `Different ->
            raise
              (Exceptions.UnexpectedToken
                 { expected = TokenType.Semicolon; got = next })
      in
      Ast.Assign
        { ident = { token = ident; name }; eq = next; value; semicolon }
  | Token.Plus _ ->
      let ast =
        parse_binary_operation lexer
          (Ast.Ident { token = ident; name })
          { token = next; value = BinaryOperation.Add }
      in
      Lexer.assert_last_token_of_type lexer TokenType.Semicolon;
      ast
  | Token.Minus _ ->
      let ast =
        parse_binary_operation lexer
          (Ast.Ident { token = ident; name })
          { token = next; value = BinaryOperation.Subtract }
      in

      Lexer.assert_last_token_of_type lexer TokenType.Semicolon;
      ast
  | Token.Star _ ->
      let ast =
        parse_binary_operation lexer
          (Ast.Ident { token = ident; name })
          { token = next; value = BinaryOperation.Multiply }
      in
      Lexer.assert_last_token_of_type lexer TokenType.Semicolon;
      ast
  | Token.Slash _ ->
      let ast =
        parse_binary_operation lexer
          (Ast.Ident { token = ident; name })
          { token = next; value = BinaryOperation.Divide }
      in
      Lexer.assert_last_token_of_type lexer TokenType.Semicolon;
      ast
  | _ ->
      raise
        (Exceptions.UnexpectedToken
           { expected = TokenType.Semicolon; got = next })
