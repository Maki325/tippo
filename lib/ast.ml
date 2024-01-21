open Sexplib.Std

type lit_value = Int of int [@@deriving show { with_path = false }, sexp]

type ident = { token : Token.t; name : string }
[@@deriving show { with_path = false }, sexp]

type declaration = { token : Token.t; is_mutable : bool }
[@@deriving show { with_path = false }, sexp]

type binary_operation = { token : Token.t; value : BinaryOperation.t }
[@@deriving show { with_path = false }, sexp]

type t =
  | Declare of { declaration : declaration; ident : ident; semicolon : Token.t }
  | Assign of { ident : ident; eq : Token.t; value : t; semicolon : Token.t }
  | DeclareAssign of {
      declaration : declaration;
      ident : ident;
      eq : Token.t;
      value : t;
      semicolon : Token.t;
    }
  | Lit of { token : Token.t; value : lit_value }
  | AlphaPrint of { token : Token.t; ident : ident; semicolon : Token.t }
  | Ident of ident
  | BinaryOperation of { left : t; op : binary_operation; right : t }
  | Priority of { open_paren : Token.t; close_paren : Token.t; inner : t }
[@@deriving show { with_path = false }, sexp]
