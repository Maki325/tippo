open Sexplib.Std

type t =
  | EOF of Location.t
  | Invalid of Location.t
  | Const of Location.t
  | Mut of Location.t
  | Ident of Location.t * string
  | Int of Location.t * int
  | Assign of Location.t
  | Eq of Location.t
  | Semicolon of Location.t
  | AlphaPrint of Location.t
[@@deriving show { with_path = false }, sexp]
