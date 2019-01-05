module C = Configurator.V1

let _flags_for_dir lib_dir =
  [
     Printf.sprintf "-Wl,-R%s" lib_dir
    ; Printf.sprintf "-L%s" lib_dir
  ]


let () =
  C.main ~name:"lmdb-config" (fun _c ->
      let flags =
          [
            "-W -Wall -Wno-unused-parameter -Wbad-function-cast -Wuninitialized";
            "-O2"
          ]
      in
      C.Flags.write_sexp "c_library_flags.sexp" flags)
