open Sexplib.Std

type t = { file_path : string option; row : int; col : int }
[@@deriving show { with_path = false }, sexp]

let to_string loc =
  "["
  ^ (match loc.file_path with None -> "" | Some path -> path ^ ":")
  ^ Utils.to_string_padded loc.row 3
  ^ ":"
  ^ Utils.to_string_padded loc.col 3
  ^ "]"
