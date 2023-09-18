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

let rec compile ?(silent = false) (program : Program.t) file_path =
  let build_path =
    Unix.realpath (Filename.concat (Filename.dirname file_path) "./build")
  in
  if not (Sys.file_exists build_path) then Sys.mkdir build_path 0o700;

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
  compile_ast program program.ast file;
  Printf.fprintf file "mov rsp, rbp\npop rbp\n"

and compile_ast program ast_list file =
  let write str = Printf.fprintf file "%s" str in
  match ast_list with
  | [] -> ()
  | ast :: rest ->
      (match ast with
      | Ast.DeclareAssign value -> (
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

          match value.value with
          | Ast.Lit lit_value -> (
              match lit_value.value with
              | Ast.Int value ->
                  Printf.fprintf file "mov QWORD [rbp %c %u], %u\n" sign offset
                    value)
          | _ -> assert false)
      | Ast.Assign value -> (
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

          match value.value with
          | Ast.Lit lit_value -> (
              match lit_value.value with
              | Ast.Int value ->
                  Printf.fprintf file "mov QWORD [rbp %c %u], %u\n" sign offset
                    value)
          | _ -> assert false)
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
      | _ -> ());
      compile_ast program rest file
