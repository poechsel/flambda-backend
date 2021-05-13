let () =
  (match Sys.backend_type with
   | Native -> Memtrace.trace_if_requested ~context:"ocamlopt" ()
   | Bytecode | Other _ -> ());
  let status = Optmaindriver.main Sys.argv Format.err_formatter in
  Profile.print Format.std_formatter !Clflags.profile_columns;
  exit status
