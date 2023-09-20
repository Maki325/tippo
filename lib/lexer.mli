type t

val from_string : ?file_path:string -> string -> t
val from_file : string -> t
val next_token : t -> Token.t
val peek_token : t -> Token.t

val is_last_token_of_type :
  t -> TokenType.t -> [> `Different | `Same of Token.t ]

val assert_last_token_of_type : t -> TokenType.t -> unit
(** [expect_token]
    @raise Exceptions.UnexpectedToken if [Token] isn't of type [TokenType]
*)

val get_last_token : t -> Token.t

val expect_token : t -> TokenType.t -> Token.t
(** [expect_token]
    @raise Exceptions.UnexpectedToken if [Token] isn't of type [TokenType]
*)

val sexp_of_t : t -> Sexplib.Sexp.t
