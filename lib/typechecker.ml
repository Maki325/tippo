let extract_type ast =
  match ast with
  | Ast.Lit lit -> ( match lit.value with Ast.Int _ -> Type.Int)
  | _ -> raise (Exceptions.UnexpectedAst ast)

let rec typecheck (program : Program.t) list =
  match list with
  | [] ->
      Base.Hashtbl.iter program.variablesMap ~f:(fun v ->
          if v.ty == Type.None then raise (Exceptions.UntypedVariable v.ident));
      List.iter
        (fun (variable : Program.variable) ->
          let size = Type.get_byte_size variable.ty in
          variable.offset <- program.offset - size;
          program.offset <- program.offset - size;
          ())
        program.variables;
      ()
  | ast :: rest ->
      (match ast with
      | Ast.DeclareAssign value ->
          let name = value.ident.name in
          let exists =
            Base.Hashtbl.existsi program.variablesMap ~f:(fun ~key:k ~data:_ ->
                k == name)
          in
          (* If it exists, we will throw an error *)
          (* But in the future, when we add scopes, we could allow it to shadow the *)
          (* last variable with the same name, but for now, we won't *)
          if exists then raise (Exceptions.VariableAlreadyExists ast);

          let ty = extract_type value.value in
          let variable : Program.variable =
            { ident = value.ident; ty; offset = 0 }
          in
          ignore
            (Base.Hashtbl.add program.variablesMap ~key:name ~data:variable);
          program.variables <- program.variables @ [ variable ];
          ()
      | Ast.Declare value ->
          let name = value.ident.name in
          let exists =
            Base.Hashtbl.existsi program.variablesMap ~f:(fun ~key:k ~data:_ ->
                k == name)
          in
          (* If it exists, we will throw an error *)
          (* But in the future, when we add scopes, we could allow it to shadow the *)
          (* last variable with the same name, but for now, we won't *)
          if exists then raise (Exceptions.VariableAlreadyExists ast);

          let variable : Program.variable =
            { ident = value.ident; ty = Type.None; offset = 0 }
          in
          ignore
            (Base.Hashtbl.add program.variablesMap ~key:name ~data:variable);
          program.variables <- program.variables @ [ variable ];
          ()
      | Ast.Assign value -> (
          let name = value.ident.name in
          let variable =
            match Base.Hashtbl.find program.variablesMap name with
            | None -> raise (Exceptions.UndeclaredVariable ast)
            | Some v -> v
          in

          let ty = extract_type value.value in
          match variable.ty with
          | Type.None ->
              ignore
                (Base.Hashtbl.add program.variablesMap ~key:name
                   ~data:{ ident = value.ident; ty; offset = 0 });
              ()
          | _ ->
              if ty != variable.ty then
                raise
                  (Exceptions.UnexpectedType
                     { expected = variable.ty; got = ty });
              ())
      | _ -> ());
      typecheck program rest
