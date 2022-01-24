[@@@warning "-33-27"]

open Main_errors
open Mini_c
open Trace

module Lift = Wasm_pass.Lift
module Wasm = Wasm_pass.Compiler


let compile_contract ~raise : options:Compiler_options.t -> expression -> WasmObjectFile.Ast.module_'  = fun ~options e ->
  (* give warnings / errors on certain command line options... *)
  (* ignore options; *)
  print_endline "before:";
  Mini_c.PP.expression Format.std_formatter e;
  Format.pp_print_flush Format.std_formatter ();
  let e = Lift.toplevel e in
  print_endline "after:";
  Mini_c.PP.expression Format.std_formatter e;
  Simple_utils.Trace.trace ~raise wasm_tracer @@ Wasm.compile e
  
  