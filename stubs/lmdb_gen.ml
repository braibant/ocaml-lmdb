let () =
  let fmt file = Format.formatter_of_out_channel (open_out file) in
  let fmt_c = fmt "lmdb_stubs.c" in
  Format.fprintf fmt_c "#include \"lmdb.h\"@.";
  Cstubs.write_c fmt_c ~prefix:"caml_" (module Lmdb_bindings.C);
  let fmt_ml = fmt "lmdb_generated.ml" in
  Cstubs.write_ml fmt_ml ~prefix:"caml_" (module Lmdb_bindings.C);
  flush_all ()
;;
