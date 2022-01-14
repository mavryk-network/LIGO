(* open Api_helpers
open Simple_utils
module Helpers   = Ligo_compile.Helpers
module Run = Ligo_run.Of_michelson *)

open Mini_c

let compile_contract ~raise : options:Compiler_options.t -> expression -> WasmObjectFile.Ast.module_  = fun ~options e ->
  ignore raise; ignore options; ignore e;
  failwith "TODO this"