open Sexplib.Std

type t = { file_path : string option; row : int; col : int }
[@@deriving show { with_path = false }, sexp]
