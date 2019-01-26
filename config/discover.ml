open Base
module C = Configurator.V1

let _flags_for_dir lib_dir =
  [Printf.sprintf "-Wl,-R%s" lib_dir; Printf.sprintf "-L%s" lib_dir]

let () =
  C.main ~name:"lmdb-config" (fun _c ->
      let conf : C.Pkg_config.package_conf =
        { libs= ["-llmdb"]
        ; cflags=
            [ "-O2"
            ; "-Wall"
            ; "-Wextra"
            ; "-Wno-unused-parameter"
            ; "-pthread"
            ; "-Wbad-function-cast"
            ; "-Wuninitialized" ] }
      in
      C.Flags.write_sexp "c_flags.sexp" conf.cflags ;
      C.Flags.write_sexp "c_library_flags.sexp" conf.libs )
