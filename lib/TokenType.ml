type t =
  | EOF
  | Invalid
  | Const
  | Mut
  | Ident
  | Int
  | Assign
  | Eq
  | Semicolon
  | AlphaPrint
[@@deriving show { with_path = false }, sexp]

let is_token_type (token : Token.t) token_type =
  match token with
  | EOF _ -> if token_type = EOF then true else false
  | Invalid _ -> if token_type = Invalid then true else false
  | Const _ -> if token_type = Const then true else false
  | Mut _ -> if token_type = Mut then true else false
  | Ident _ -> if token_type = Ident then true else false
  | Int _ -> if token_type = Int then true else false
  | Assign _ -> if token_type = Assign then true else false
  | Eq _ -> if token_type = Eq then true else false
  | Semicolon _ -> if token_type = Semicolon then true else false
  | AlphaPrint _ -> if token_type = AlphaPrint then true else false
