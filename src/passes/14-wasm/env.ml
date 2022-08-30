module I = Mini_c.Types
module W = WasmObjectFile
module A = W.Ast
module T = W.Types
module S = W.Source
module Z = Z
module ValueVar = Stage_common.Types.ValueVar
module Location = Simple_utils.Location

type locals = (string * T.value_type) list

type operand_stack = (T.value_type * operand_stack_next) option
 and operand_stack_next = Next of operand_stack

type t = {
  locals: locals;
  blocks: A.block_type list;  
  operand_stack: operand_stack;
  linear_memory_offset: int32;
}

let make_env () = {
  locals               = [];
  blocks               = [];
  operand_stack        = None;
  linear_memory_offset = 0l;
}

let add_local (env: t) (local: string * T.value_type) = 
  { env with locals = env.locals @ [local]}

let add_locals (env: t) (locals: locals) = 
  {env with locals = env.locals @ locals}