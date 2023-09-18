open Sexplib.Std

type t = {
  file_path : string option;
  content : string;
  len : int;
  mutable position : int;
  mutable ch : char option;
  mutable row : int;
  mutable col : int;
  mutable last_position : int;
}
[@@deriving sexp]

let peek_char lexer =
  if lexer.position >= lexer.len then None
  else Some (String.unsafe_get lexer.content lexer.position)

let read_char lexer =
  let ch = peek_char lexer in
  lexer.ch <- ch;
  lexer.position <- lexer.position + 1;
  lexer.col <- lexer.col + lexer.position - lexer.last_position;
  lexer.last_position <- lexer.position

let from_string ?file_path content =
  let lexer =
    {
      file_path;
      content;
      len = String.length content;
      position = 0;
      ch = None;
      row = 1;
      col = 1;
      last_position = 0;
    }
  in
  read_char lexer;
  lexer

let read_file filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true do
      lines := input_line chan :: !lines
    done;
    String.concat "" !lines
  with End_of_file ->
    close_in chan;
    String.concat "\n" (List.rev !lines)

let from_file file_path2 =
  let content = read_file file_path2 in
  from_string content ?file_path:(Some file_path2)

let is_whitespace c =
  match c with '\t' | '\n' | '\x0C' | '\r' | ' ' -> true | _ -> false

let rec skip_whitespace lexer =
  match lexer.ch with
  | Some c when is_whitespace c ->
      if c == '\n' then (
        lexer.row <- lexer.row + 1;
        lexer.col <- 1;
        lexer.last_position <- lexer.position);
      read_char lexer;
      skip_whitespace lexer
  | _ -> ()

let create_location lexer len : Location.t =
  { file_path = lexer.file_path; row = lexer.row; col = lexer.col - len }

let rec read_ident lexer start =
  let start =
    match start with Some start -> start | None -> lexer.position - 1
  in
  match peek_char lexer with
  | Some 'a' .. 'z' | Some 'A' .. 'Z' | Some '0' .. '9' | Some '_' ->
      read_char lexer;
      read_ident lexer (Some start)
  | _ -> (
      let len = lexer.position - start in
      let value = String.sub lexer.content start len in

      match value with
      | "const" -> Token.Const (create_location lexer len)
      | "mut" -> Token.Mut (create_location lexer len)
      | "alpha_print" -> Token.AlphaPrint (create_location lexer len)
      | _ -> Token.Ident (create_location lexer len, value))

let rec read_int lexer start =
  let start =
    match start with Some start -> start | None -> lexer.position - 1
  in
  match peek_char lexer with
  | Some '0' .. '9' | Some '_' ->
      read_char lexer;
      read_int lexer (Some start)
  | _ ->
      let len = lexer.position - start in
      let value = String.sub lexer.content start len in
      Token.Int (create_location lexer len, int_of_string value)

let next_token lexer =
  skip_whitespace lexer;
  let token =
    match lexer.ch with
    | None ->
        Token.EOF
          { file_path = lexer.file_path; col = lexer.col; row = lexer.row }
    | Some 'a' .. 'z' | Some 'A' .. 'Z' | Some '_' -> read_ident lexer None
    | Some '0' .. '9' -> read_int lexer None
    | Some '=' ->
        if peek_char lexer = Some '=' then (
          read_char lexer;
          Token.Eq (create_location lexer 2))
        else Token.Assign (create_location lexer 1)
    | Some ';' -> Token.Semicolon (create_location lexer 1)
    | _ -> Token.Invalid (create_location lexer 1)
  in
  read_char lexer;
  token

(** @raise Exceptions.UnexpectedToken *)
let expect_token lexer token_type =
  let token = next_token lexer in
  if TokenType.is_token_type token token_type then token
  else raise (Exceptions.UnexpectedToken { expected = token_type; got = token })

let peek_token lexer =
  let position = lexer.position in
  let ch = lexer.ch in
  let row = lexer.row in
  let col = lexer.col in
  let last_position = lexer.last_position in
  skip_whitespace lexer;
  let token =
    match lexer.ch with
    | None ->
        Token.EOF
          { file_path = lexer.file_path; col = lexer.col; row = lexer.row }
    | Some 'a' .. 'z' | Some 'A' .. 'Z' | Some '_' -> read_ident lexer None
    | Some '0' .. '9' -> read_int lexer None
    | Some '=' ->
        if peek_char lexer = Some '=' then (
          read_char lexer;
          Token.Eq (create_location lexer 2))
        else Token.Assign (create_location lexer 1)
    | Some ';' -> Token.Semicolon (create_location lexer 1)
    | _ -> Token.Invalid (create_location lexer 1)
  in

  lexer.position <- position;
  lexer.ch <- ch;
  lexer.row <- row;
  lexer.col <- col;
  lexer.last_position <- last_position;

  token
