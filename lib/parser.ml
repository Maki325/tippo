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
  let ast, check_for_more =
    match token with
    | Token.Const _ -> (parse_declaration lexer token false, false)
    | Token.Mut _ -> (parse_declaration lexer token true, false)
    | Token.Int (_, value) -> (Ast.Lit { token; value = Ast.Int value }, true)
    | Token.Ident ident_inner -> (Ast.create_ident_ast ident_inner, true)
    | Token.AlphaPrint _ ->
        let ident = Lexer.expect_token lexer TokenType.Ident in
        let ident_inner =
          match ident with Token.Ident value -> value | _ -> assert false
        in
        ( Ast.AlphaPrint
            {
              token;
              ident = Ast.create_ident ident_inner;
              semicolon = Lexer.expect_token lexer TokenType.Semicolon;
            },
          false )
    | Token.OpenParen _ -> (parse_priority lexer token, true)
    | _ ->
        print_endline (Sexplib0.Sexp.to_string_hum (Token.sexp_of_t token));
        assert false
  in

  if check_for_more then
    let next = Lexer.peek_token lexer in
    match next with
    | Token.Semicolon _ -> ast
    | Token.Plus _ | Token.Minus _ | Token.Star _ | Token.Slash _ ->
        parse_binary_operation lexer ast (Lexer.next_token lexer)
    | _ -> (
        match ast with
        | Ast.Ident ident when TokenType.is_token_type next TokenType.Assign ->
            (* Ignore the `=` token *)
            ignore (Lexer.next_token lexer);
            let value = parse lexer in
            let semicolon = Lexer.expect_token lexer TokenType.Semicolon in
            Ast.Assign { ident; eq = next; value; semicolon }
        | v -> v)
  else ast

and parse_priority lexer open_paren =
  let inner = parse lexer in

  Ast.Priority
    {
      open_paren;
      close_paren = Lexer.expect_token lexer TokenType.CloseParen;
      inner;
    }

and parse_declaration lexer declaration is_mutable =
  let ident = Lexer.expect_token lexer TokenType.Ident in
  let ident_inner =
    match ident with Token.Ident value -> value | _ -> assert false
  in
  let next = Lexer.next_token lexer in
  match next with
  | Token.Semicolon _ ->
      Ast.Declare
        {
          ident = Ast.create_ident ident_inner;
          declaration = { token = declaration; is_mutable };
          semicolon = next;
        }
  | Token.Assign _ ->
      let value = parse lexer in
      let semicolon = Lexer.expect_token lexer TokenType.Semicolon in
      Ast.DeclareAssign
        {
          ident = Ast.create_ident ident_inner;
          declaration = { token = declaration; is_mutable };
          eq = next;
          value;
          semicolon;
        }
  | _ ->
      raise
        (Exceptions.UnexpectedToken
           { expected = TokenType.Semicolon; got = next })

and parse_binary_operation lexer left token =
  let value =
    match token with
    | Token.Plus _ -> BinaryOperation.Add
    | Token.Minus _ -> BinaryOperation.Subtract
    | Token.Star _ -> BinaryOperation.Multiply
    | Token.Slash _ -> BinaryOperation.Divide
    | _ -> assert false
  in
  let binary_operation : Ast.binary_operation = { token; value } in
  let right = parse lexer in

  let rec fix_order left (binary_operation : Ast.binary_operation) right =
    match right with
    | Ast.BinaryOperation bo
      when BinaryOperation.priority bo.op.value
           <= BinaryOperation.priority binary_operation.value -> (
        let fix = fix_order left binary_operation bo.left in
        match fix with
        | Ast.BinaryOperation fix ->
            Ast.BinaryOperation
              {
                left =
                  Ast.BinaryOperation
                    { left = fix.left; op = fix.op; right = fix.right };
                right = bo.right;
                op = bo.op;
              }
        | _ -> assert false)
    | _ -> Ast.BinaryOperation { left; right; op = binary_operation }
  in

  fix_order left binary_operation right
