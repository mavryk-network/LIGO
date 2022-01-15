[@@@warning "-33-27"]

open Main_errors
open Mini_c
open Trace

module Wasm = Wasm_pass.Compiler


let compile_contract ~raise : options:Compiler_options.t -> expression -> WasmObjectFile.Ast.module_'  = fun ~options e ->
  (* give warnings / errors on certain command line options... *)
  (* ignore options; *)
  let (_, _) = trace ~raise wasm_tracer @@ Wasm.compile e in
  failwith "uhh"

  