let print_source ?(channel = stdout) sexps =
  let formatter = Format.formatter_of_out_channel channel in
  List.iter (fun s -> Sexplib.Sexp.pp_hum formatter s) sexps;
  Format.pp_print_flush formatter ();
  print_newline ()
