type t = Int | None [@@deriving show { with_path = false }, sexp]

exception UnsupportedType of t

let get_byte_size ty =
  match ty with Int -> 8 | None -> raise (UnsupportedType ty)
