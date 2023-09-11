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

let to_string t =
  match t with
  | EOF loc -> Location.to_string loc ^ ": " ^ "EOF"
  | Invalid loc -> Location.to_string loc ^ ": " ^ "Invalid"
  | Const loc -> Location.to_string loc ^ ": " ^ "Const"
  | Mut loc -> Location.to_string loc ^ ": " ^ "Mut"
  | Ident (loc, s) -> Location.to_string loc ^ ": " ^ "Ident " ^ "'" ^ s ^ "'"
  | Int (loc, i) ->
      Location.to_string loc ^ ": " ^ "Int " ^ "'" ^ Int.to_string i ^ "'"
  | Assign loc -> Location.to_string loc ^ ": " ^ "Assign"
  | Eq loc -> Location.to_string loc ^ ": " ^ "Eq"
  | Semicolon loc -> Location.to_string loc ^ ": " ^ "Semicolon"
  | AlphaPrint loc -> Location.to_string loc ^ ": " ^ "AlphaPrint"
