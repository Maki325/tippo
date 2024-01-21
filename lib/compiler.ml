let add_alpha_print file =
  Printf.fprintf file "alpha_print:\n";
  Printf.fprintf file "  mov   r9, -3689348814741910323\n";
  Printf.fprintf file "  sub   rsp, 40\n";
  Printf.fprintf file "  mov   BYTE [rsp+31], 10\n";
  Printf.fprintf file "  lea   rcx, [rsp+30]\n";
  Printf.fprintf file ".alpha_print_l2:\n";
  Printf.fprintf file "  mov   rax, rdi\n";
  Printf.fprintf file "  lea   r8, [rsp+32]\n";
  Printf.fprintf file "  mul   r9\n";
  Printf.fprintf file "  mov   rax, rdi\n";
  Printf.fprintf file "  sub   r8, rcx\n";
  Printf.fprintf file "  shr   rdx, 3\n";
  Printf.fprintf file "  lea   rsi, [rdx+rdx*4]\n";
  Printf.fprintf file "  add   rsi, rsi\n";
  Printf.fprintf file "  sub   rax, rsi\n";
  Printf.fprintf file "  add   eax, 48\n";
  Printf.fprintf file "  mov   BYTE [rcx], al\n";
  Printf.fprintf file "  mov   rax, rdi\n";
  Printf.fprintf file "  mov   rdi, rdx\n";
  Printf.fprintf file "  mov   rdx, rcx\n";
  Printf.fprintf file "  sub   rcx, 1\n";
  Printf.fprintf file "  cmp   rax, 9\n";
  Printf.fprintf file "  ja  .alpha_print_l2\n";
  Printf.fprintf file "  lea   rax, [rsp+32]\n";
  Printf.fprintf file "  mov   edi, 1\n";
  Printf.fprintf file "  sub   rdx, rax\n";
  Printf.fprintf file "  xor   eax, eax\n";
  Printf.fprintf file "  lea   rsi, [rsp+32+rdx]\n";
  Printf.fprintf file "  mov   rdx, r8\n";
  Printf.fprintf file "  mov   rax, 1\n";
  Printf.fprintf file "  syscall\n";
  Printf.fprintf file "  add   rsp, 40\n";
  Printf.fprintf file "ret\n"

let prepare file =
  Printf.fprintf file "BITS 64\n";
  Printf.fprintf file "section .text\n";
  add_alpha_print file;
  Printf.fprintf file "global _start\n";
  Printf.fprintf file "_start:\n"

let write_exit file =
  (* RDI is the return code of the program *)
  (* Which is returned from the main function in the RAX register *)
  (* Printf.fprintf file "mov rdi, rax\n"; *)
  Printf.fprintf file "mov rdi, 0\n";
  (* RAX is the syscall code for exiting the program *)
  Printf.fprintf file "mov rax, 60\n";
  Printf.fprintf file "syscall\n"

let stringify_binary_operation_value op =
  match op with
  | BinaryOperation.Add -> " + "
  | BinaryOperation.Subtract -> " - "
  | BinaryOperation.Multiply -> " * "
  | BinaryOperation.Divide -> " / "

let rec stringify_binary_operation ast : string =
  match ast with
  | Ast.BinaryOperation { left; right; op } ->
      "["
      ^ stringify_binary_operation left
      ^ "]"
      ^ stringify_binary_operation_value op.value
      ^ "["
      ^ stringify_binary_operation right
      ^ "]"
  | Ast.Ident ident -> ident.name
  | Ast.Lit value -> (
      match value.value with Ast.Int value -> Int.to_string value)
  | Ast.Priority priority ->
      "(" ^ stringify_binary_operation priority.inner ^ ")"
  | _ ->
      Utils.print_source (Ast.sexp_of_t ast :: []);
      assert false

let rec compile ?(silent = false) (program : Program.t) file_path =
  let build_path = Filename.concat (Filename.dirname file_path) "./build" in
  if not (Sys.file_exists build_path) then Sys.mkdir build_path 0o700;
  let build_path = Unix.realpath build_path in

  let file_name = Filename.basename file_path in
  let asm_file_path = Printf.sprintf "%s/%s.asm" build_path file_name in
  let asm_file = open_out asm_file_path in
  prepare asm_file;
  compole_program program asm_file;
  write_exit asm_file;
  close_out asm_file;

  let object_file_path = Printf.sprintf "%s/%s.o" build_path file_name in
  let cmd =
    Printf.sprintf "nasm -g -felf64 %s -o %s" asm_file_path object_file_path
  in
  if not silent then (
    Printf.printf "[CMD]: %s\n" cmd;
    flush stdout);
  ignore (Unix.system cmd);
  let cmd = Printf.sprintf "ld -o %s %s" file_path object_file_path in
  if not silent then (
    Printf.printf "[CMD]: %s\n" cmd;
    flush stdout);
  ignore (Unix.system cmd);
  ()

and compole_program program file =
  Printf.fprintf file "push rbp\nmov rbp, rsp\nsub rsp, %n\n"
    (Int.abs program.offset);
  compile_ast_list program program.ast file;
  Printf.fprintf file "mov rsp, rbp\npop rbp\n"

and compile_ident ?(reg = Register.REG_A) (ident : Ast.ident)
    (program : Program.t) file =
  let from =
    match Base.Hashtbl.find program.variablesMap ident.name with
    | None -> assert false
    | Some variable -> variable
  in

  let sign_from = if from.offset < 0 then '-' else '+' in

  Printf.fprintf file "mov %s, [rbp %c %u]\n" (Register.get_64bit reg) sign_from
    (abs from.offset)

and compile_lit ?(reg = Register.REG_A) (lit : Ast.lit_value) file =
  match lit with
  | Ast.Int value ->
      Printf.fprintf file "mov %s, %u\n" (Register.get_64bit reg) value

and compile_binary_operation ?(reg = Register.REG_A) left op right program file
    =
  Printf.fprintf file "; Binary Operation Start: %s\n"
    (stringify_binary_operation (Ast.BinaryOperation { left; op; right }));
  let write str = Printf.fprintf file "%s" str in

  compile_ast left program file ?reg:(Some Register.REG_A);
  write "push rbx\npush rax\n";
  compile_ast right program file ?reg:(Some Register.REG_B);
  write "pop rax\n";

  (match op.value with
  | BinaryOperation.Add -> write "add rax, rbx\n"
  | BinaryOperation.Subtract -> write "sub rax, rbx\n"
  (* RAX*RBX -> Ends up in RDX and RAX; So we don't need to move them *)
  | BinaryOperation.Multiply -> write "push rdx\nmul rbx\npop rdx\n"
  (* [RDX:RAX]/RBX -> Quotient in RAX, Remainder in RDX *)
  (* We need to set RDX to 0 before dividing. XOR-ing itself is the fastest *)
  (* In 64-bit mode, still use xor r32, r32, because writing a 32-bit reg zeros the upper 32. *)
  | BinaryOperation.Divide -> write "push rdx\nxor edx, edx\ndiv rbx\npop rdx\n");

  write "pop rbx\n";

  (match reg with
  | REG_A -> ()
  | _ ->
      let reg = Register.get_64bit reg in
      Printf.fprintf file "mov %s, rax\n" reg);

  Printf.fprintf file "; Binary Operation End: %s\n"
    (stringify_binary_operation (Ast.BinaryOperation { left; op; right }));
  ()

and compile_assign ?reg (ident : Ast.ident) value (program : Program.t) file =
  let name = ident.name in
  let variable =
    match Base.Hashtbl.find program.variablesMap name with
    | None -> assert false
    | Some variable -> variable
  in

  let sign = if variable.offset < 0 then '-' else '+' in
  let offset = abs variable.offset in

  Printf.fprintf file "; START %s =\n" name;

  (match value with
  | Ast.Lit lit_value -> (
      match lit_value.value with
      | Ast.Int value ->
          Printf.fprintf file "mov QWORD [rbp %c %u], %u\n" sign offset value)
  | Ast.Ident ident ->
      let from =
        match Base.Hashtbl.find program.variablesMap ident.name with
        | None -> assert false
        | Some variable -> variable
      in

      let sign_from = if from.offset < 0 then '-' else '+' in

      Printf.fprintf file
        "push rax\nmov rax, [rbp %c %u]\nmov [rbp %c %u], rax\npop rax\n"
        sign_from (abs from.offset) sign offset
  | Ast.BinaryOperation bo ->
      Printf.fprintf file "push rax\n";
      compile_binary_operation bo.left bo.op bo.right program file;
      Printf.fprintf file "mov [rbp %c %u], rax\npop rax\n" sign offset
  | Ast.Priority priority ->
      Printf.fprintf file "push rax\n";
      compile_ast priority.inner program file ?reg:(Some Register.REG_A);
      Printf.fprintf file "mov [rbp %c %u], rax\npop rax\n" sign offset
  | _ -> assert false);

  (match reg with
  | Some reg ->
      Printf.fprintf file "mov %s, [rbp %c %u]\n" (Register.get_64bit reg) sign
        offset
  | None -> ());

  Printf.fprintf file "; END %s =\n" name

and compile_ast ?reg ast program file =
  let write str = Printf.fprintf file "%s" str in
  match ast with
  | Ast.DeclareAssign value ->
      compile_assign value.ident value.value program file ?reg
  | Ast.Assign value -> compile_assign value.ident value.value program file ?reg
  | Ast.AlphaPrint value ->
      let name = value.ident.name in
      let variable =
        match Base.Hashtbl.find program.variablesMap name with
        | None -> assert false
        | Some variable -> variable
      in

      let sign = if variable.offset < 0 then '-' else '+' in
      let offset =
        if variable.offset < 0 then -variable.offset else variable.offset
      in

      Printf.fprintf file "mov rdi, [rbp %c %u]\n" sign offset;
      write "call alpha_print\n"
  | Ast.Ident ident -> compile_ident ident program file ?reg
  | Ast.Lit lit -> compile_lit lit.value file ?reg
  | Ast.BinaryOperation bo ->
      compile_binary_operation bo.left bo.op bo.right program file ?reg
  | Ast.Priority priority -> compile_ast priority.inner program file ?reg
  | _ ->
      Utils.print_source (Ast.sexp_of_t ast :: []);
      assert false

and compile_ast_list program ast_list file =
  match ast_list with
  | [] -> ()
  | ast :: rest ->
      compile_ast ast program file;
      compile_ast_list program rest file
