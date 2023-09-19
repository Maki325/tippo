type t = Add | Subtract | Multiply | Divide
[@@deriving show { with_path = false }, sexp]

let priority t =
  match t with Add -> 0 | Subtract -> 0 | Multiply -> 1 | Divide -> 1
