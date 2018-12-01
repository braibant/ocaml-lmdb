let () =
  let fmt file = Format.formatter_of_out_channel (open_out file) in
  let fmt_c = fmt "lmdb_types_stubs.c" in
  Format.fprintf fmt_c "#include \"lmdb.h\"@." ;
  Cstubs.Types.write_c c_fmt (module Lmdb_types.T)
  flush_all ()
