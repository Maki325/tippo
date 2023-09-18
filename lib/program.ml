open Sexplib.Std

type variable = { ident : Ast.ident; ty : Type.t; mutable offset : int }
[@@deriving show { with_path = false }, sexp]

type variables_map = variable Base.Hashtbl.M(Base.String).t [@@deriving sexp]

type t = {
  mutable variables : variable list;
  mutable variablesMap : variables_map;
  mutable offset : int;
  ast : Ast.t list;
}
[@@deriving sexp]

let empty () =
  {
    variables = [];
    variablesMap = Base.Hashtbl.create (module Base.String);
    ast = [];
    offset = 0;
  }

let from_ast ast =
  {
    variables = [];
    variablesMap = Base.Hashtbl.create (module Base.String);
    ast;
    offset = 0;
  }
