exception UnexpectedToken of { expected : TokenType.t; got : Token.t }
[@@deriving show { with_path = false }, sexp]

exception VariableAlreadyExists of Ast.t
[@@deriving show { with_path = false }, sexp]

exception UnexpectedAst of Ast.t [@@deriving show { with_path = false }, sexp]

exception UnexpectedType of { expected : Type.t; got : Type.t }
[@@deriving show { with_path = false }, sexp]

exception UndeclaredVariable of Ast.t
[@@deriving show { with_path = false }, sexp]

exception UntypedVariable of Ast.ident
[@@deriving show { with_path = false }, sexp]

exception ArithmeticPriorityGroupMoreThanOneChild of Ast.t
[@@deriving show { with_path = false }, sexp]
