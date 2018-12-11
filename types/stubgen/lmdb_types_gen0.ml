let () =
  let fmt file = Format.formatter_of_out_channel (open_out file) in
  let fmt = fmt "lmdb_types_gen.c" in
  Format.fprintf fmt "#include \"lmdb.h\"@." ;
  Cstubs.Types.write_c fmt (module Lmdb_bindings_types.C) ;
  flush_all ()
