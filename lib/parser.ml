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
  | Token.OpenParen _ -> parse_priority_group lexer token
  | _ -> assert false

and parse_priority_group lexer open_paren =
  let inner = parse lexer in

  let close_paren = Lexer.get_last_token lexer in
  if not (TokenType.is_token_type close_paren TokenType.CloseParen) then
    raise
      (Exceptions.UnexpectedToken
         { expected = TokenType.CloseParen; got = close_paren });

  let next = Lexer.next_token lexer in
  let priority = Ast.Priority { open_paren; close_paren; inner } in
  match next with
  | Token.Semicolon _ -> priority
  | Token.Plus _ | Token.Minus _ | Token.Star _ | Token.Slash _ ->
      let ast = parse_binary_operation lexer priority next in
      Lexer.assert_last_token_of_type lexer TokenType.Semicolon;
      ast
  | _ ->
      raise
        (Exceptions.UnexpectedToken
           { expected = TokenType.Semicolon; got = next })

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

and parse_int lexer token value : Ast.t =
  let next = Lexer.next_token lexer in
  let lit = Ast.Lit { token; value = Ast.Int value } in
  match next with
  | Token.Semicolon _ | Token.CloseParen _ -> lit
  | Token.Plus _ | Token.Minus _ | Token.Star _ | Token.Slash _ ->
      let ast = parse_binary_operation lexer lit next in
      (* Sooooo *)
      (* Last Token can be either Semicolon *)
      (* Or CloseParen *)
      (* But that WILL make my life harder *)
      (* As I now have to stop the group by using Last Token *)
      (* Instead of just next in `parse_priority_group` *)
      let last_token = Lexer.get_last_token lexer in
      (match last_token with
      | Semicolon _ | CloseParen _ -> ()
      | _ ->
          raise
            (Exceptions.UnexpectedToken
               { expected = TokenType.Semicolon; got = last_token }));
      ast
  | Token.OpenParen _ -> parse_priority_group lexer next
  | _ ->
      raise
        (Exceptions.UnexpectedToken
           { expected = TokenType.Semicolon; got = next })

and parse_ident lexer ident name =
  let next = Lexer.next_token lexer in
  let indet = Ast.Ident { token = ident; name } in
  match next with
  | Token.Semicolon _ | Token.CloseParen _ -> indet
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
  | Token.Plus _ | Token.Minus _ | Token.Star _ | Token.Slash _ ->
      let ast = parse_binary_operation lexer indet next in

      let last_token = Lexer.get_last_token lexer in
      (match last_token with
      | Token.Semicolon _ | Token.CloseParen _ -> ()
      | _ ->
          raise
            (Exceptions.UnexpectedToken
               { expected = Semicolon; got = last_token }));

      ast
  | _ ->
      raise
        (Exceptions.UnexpectedToken
           { expected = TokenType.Semicolon; got = next })
