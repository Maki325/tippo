type t

val from_string : ?file_path:string -> string -> t
val from_file : string -> t
val next_token : t -> Token.t
