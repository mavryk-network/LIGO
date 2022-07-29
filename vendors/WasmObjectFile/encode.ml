(* Binary format version *)

let version = 1l


(* Errors *)

module Code = Error.Make ()
exception Code = Code.Error

let error = Code.error


(* Encoding stream *)

type stream =
{
  buf : Buffer.t;
  patches : (int * char) list ref
}

let stream () = {buf = Buffer.create 8192; patches = ref []}
let pos s = Buffer.length s.buf
let put s b = Buffer.add_char s.buf b
let put_string s bs = Buffer.add_string s.buf bs
let patch s pos b = s.patches := (pos, b) :: !(s.patches)

let to_string s =
  let bs = Buffer.to_bytes s.buf in
  List.iter (fun (pos, b) -> Bytes.set bs pos b) !(s.patches);
  Bytes.to_string bs


(* move to linking *)
type code_relocation =
| R_WASM_FUNCTION_INDEX_LEB of int32 * string
(* | R_WASM_MEMORY_ADDR_LEB of int32 * Ast.var   *)
| R_WASM_TYPE_INDEX_LEB of int32 * Ast.var
| R_WASM_GLOBAL_INDEX_LEB of int32 * string
| R_WASM_MEMORY_ADDR_SLEB of int32 * string
| R_WASM_TABLE_INDEX_SLEB  of int32 * string

type data_relocation =
| R_WASM_TABLE_INDEX_I32 of int32 * string
| R_WASM_MEMORY_ADDR_I32 of int32 * string



(* Encoding *)

let encode (m: Ast.module_) =
  let s = stream () in

  let code_relocations:code_relocation list ref = ref [] in

  let data_relocations:data_relocation list ref = ref [] in
  
  let module E = struct
  (* Generic values *)

  let byte i = put s (Char.chr (i land 0xff))
  let word16 i = byte (i land 0xff); byte (i lsr 8)
  let word32 i =
    Int32.(word16 (to_int (logand i 0xffffl));
            word16 (to_int (shift_right i 16)))
  let word64 i =
    Int64.(word32 (to_int32 (logand i 0xffffffffL));
            word32 (to_int32 (shift_right i 32)))

  let rec u64 i =
    let b = Int64.(to_int (logand i 0x7fL)) in
    if 0L <= i && i < 128L then byte b
    else (byte (b lor 0x80); u64 (Int64.shift_right_logical i 7))

  let rec s64 i =
    let b = Int64.(to_int (logand i 0x7fL)) in
    if -64L <= i && i < 64L then byte b
    else (byte (b lor 0x80); s64 (Int64.shift_right i 7))

  let u1 i = u64 Int64.(logand (of_int i) 1L)
  let u32 i = u64 Int64.(logand (of_int32 i) 0xffffffffL)
  let s7 i = s64 (Int64.of_int i)
  let s32 i = s64 (Int64.of_int32 i)
  let s33 i = s64 (I64_convert.extend_i32_s i)
  let f32 x = word32 (F32.to_bits x)
  let f64 x = word64 (F64.to_bits x)
  let v128 v = String.iter (put s) (V128.to_bits v)

  (* Needed for linking *)
  let vu32_fixed i = (
    let i = Int64.of_int32 i in
    Int64.(
      byte ((to_int (logand i 0x7fL)) lor 0x80);
      byte ((to_int  (logand (shift_right_logical i 7) 0x7fL) lor 0x80));
      byte ((to_int  (logand (shift_right_logical i 14) 0x7fL) lor 0x80)); 
      byte ((to_int  (logand (shift_right_logical i 21) 0x7fL) lor 0x80));
      byte (to_int (logand (shift_right_logical i 28) 0x0fL))
    )
  )

  (* Needed for linking *)
  let vs32_fixed i = (
    let i = Int64.of_int32 i in
    let rec aux i p = 
      let b = Int64.(to_int (logand i 0x7fL)) in
      if -64L <= i && i < 64L && p = 0 then byte b
      else (byte (b lor 0x80); aux (Int64.shift_right i 7) (p - 1))
    in aux i 4
  )

  let len i =
    if Int32.to_int (Int32.of_int i) <> i then
      Code.error Source.no_region "length out of bounds";
    u32 (Int32.of_int i)

  let bool b = u1 (if b then 1 else 0)
  let string bs = len (String.length bs); put_string s bs
  let name n = string (Utf8.encode n)
  let list f xs = List.iter f xs
  let opt f xo = Lib.Option.app f xo
  let vec f xs = len (List.length xs); list f xs

  let gap32 () = let p = pos s in word32 0l; byte 0; p
  let patch_gap32 p n =
    assert (n <= 0x0fff_ffff); (* Strings cannot excess 2G anyway *)
    let lsb i = Char.chr (i land 0xff) in
    patch s p (lsb (n lor 0x80));
    patch s (p + 1) (lsb ((n lsr 7) lor 0x80));
    patch s (p + 2) (lsb ((n lsr 14) lor 0x80));
    patch s (p + 3) (lsb ((n lsr 21) lor 0x80));
    patch s (p + 4) (lsb (n lsr 28))


  (* Types *)

  open Types

  let num_type = function
    | I32Type -> s7 (-0x01)
    | I64Type -> s7 (-0x02)
    | F32Type -> s7 (-0x03)
    | F64Type -> s7 (-0x04)

  let vec_type = function
    | V128Type -> s7 (-0x05)

  let ref_type = function
    | FuncRefType -> s7 (-0x10)
    | ExternRefType -> s7 (-0x11)
    
  let value_type = function
  | NumType t -> num_type t
  | VecType t -> vec_type t
  | RefType t -> ref_type t

  let func_type = function
  | FuncType (ts1, ts2) ->
    s7 (-0x20); vec value_type ts1; vec value_type ts2
  
  let type_symbol = function 
    Ast.TypeSymbol {tdetails; _} -> func_type tdetails
  | TypeNoSymbol ft -> func_type ft

  let limits vu {min; max} =
    bool (max <> None); vu min; opt vu max

  let table_type = function
    | TableType (lim, t) -> ref_type t; limits u32 lim

  let memory_type = function
    | MemoryType lim -> limits u32 lim

  let mutability = function
    | Immutable -> byte 0
    | Mutable -> byte 1

  let global_type = function
    | GlobalType (t, mut) -> value_type t; mutability mut


  (* Instructions *)

  open Source
  open Ast
  open Values
  open V128

  let op n = byte n
  let vecop n = op 0xfd; u32 n
  let end_ () = op 0x0b

  let code_pos = ref (-1l)
  let data_pos = ref (-1l)

  let memop {align; offset; _} = u32 (Int32.of_int align); u32 offset

  let var x = u32 x.it

  let reloc_index x = 
    vu32_fixed x

  let block_type = function
    | ValBlockType None -> s33 (-0x40l)
    | ValBlockType (Some t) -> value_type t 
    | VarBlockType x -> s33 x.it
    
  let rec instr locals e =
    let get_local_position name = (
      let rec find counter = function
        | (local_, _) :: _ when local_ = name -> 
          {
            it = Int32.of_int counter;
            at = e.at;
          }
        | _ ::  rest -> find (counter + 1) rest
        | [] -> failwith ("did not find:" ^ name)
      in
      find 0 locals )
    in
    match e.it with
    | Unreachable -> op 0x00
    | Nop -> op 0x01

    | Block (bt, es) -> op 0x02; block_type bt; list (instr locals) es; end_ ()
    | Loop (bt, es) -> op 0x03; block_type bt; list (instr locals) es; end_ ()
    | If (bt, es1, es2) ->
      op 0x04; block_type bt; list (instr locals) es1;
      if es2 <> [] then op 0x05;
      list (instr locals) es2; end_ ()

    | Br x -> op 0x0c; var x
    | BrIf x -> op 0x0d; var x
    | BrTable (xs, x) -> op 0x0e; vec var xs; var x
    | Return -> op 0x0f
    | Call x -> op 0x10; var x
    | CallIndirect (x, y) -> op 0x11; var y; var x
    | Drop -> op 0x1a
    | Select None -> op 0x1b
    | Select (Some ts) -> op 0x1c; vec value_type ts
    | LocalGet x -> op 0x20; var x
    | LocalSet x -> op 0x21; var x
    | LocalTee x -> op 0x22; var x
    | GlobalGet x -> op 0x23; var x
    | GlobalSet x -> op 0x24; var x

    | Call_symbol symbol -> 
      op 0x10; 
      let p = pos s in
      let import_funcs = List.filter (fun i -> match i.it.idesc.it with FuncImport _ -> true | _ -> false) m.it.imports in
      let index = Linking.func_index m.it.funcs import_funcs symbol in
      code_relocations := !code_relocations @ [R_WASM_FUNCTION_INDEX_LEB (Int32.of_int p, symbol)];          
      reloc_index index
    | CallIndirect_symbol symbol ->
      
      op 0x11;        
      let p = pos s in
      let index = Linking.find_type m.it.types symbol in
      let index = {
        it = index;
        at = e.at
      } in 
      code_relocations := !code_relocations @ [R_WASM_TYPE_INDEX_LEB (Int32.of_int p, index)];
      reloc_index index.it;
      byte 0x00
    | LocalGet_symbol x -> 
      let x = get_local_position x in
      op 0x20; 
      var x
    | LocalSet_symbol x -> 
      let x = get_local_position x in
      op 0x21; 
      var x
    | LocalTee_symbol x -> 
      let x = get_local_position x in
      op 0x22; 
      var x
    | GlobalGet_symbol x -> 
      op 0x23; 
      let p = pos s in
      code_relocations := !code_relocations @ [R_WASM_GLOBAL_INDEX_LEB (Int32.of_int p, x)];
      let x = Linking.find_global_index m.it.symbols e.at x in
      var x
    | GlobalSet_symbol x -> 
      op 0x24; 
      let p = pos s in
      code_relocations := !code_relocations @ [R_WASM_GLOBAL_INDEX_LEB (Int32.of_int p, x)];
      let x = Linking.find_global_index m.it.symbols e.at x in
      var x

    | TableGet x -> op 0x25; var x
    | TableSet x -> op 0x26; var x
    | TableSize x -> op 0xfc; u32 0x10l; var x
    | TableGrow x -> op 0xfc; u32 0x0fl; var x
    | TableFill x -> op 0xfc; u32 0x11l; var x
    | TableCopy (x, y) -> op 0xfc; u32 0x0el; var x; var y
    | TableInit (x, y) -> op 0xfc; u32 0x0cl; var y; var x
    | ElemDrop x -> op 0xfc; u32 0x0dl; var x

    | Load ({ty = I32Type; pack = None; _} as mo) -> op 0x28; memop mo
    | Load ({ty = I64Type; pack = None; _} as mo) -> op 0x29; memop mo
    | Load ({ty = F32Type; pack = None; _} as mo) -> op 0x2a; memop mo
    | Load ({ty = F64Type; pack = None; _} as mo) -> op 0x2b; memop mo
    | Load ({ty = I32Type; pack = Some (Pack8, SX); _} as mo) ->
      op 0x2c; memop mo
    | Load ({ty = I32Type; pack = Some (Pack8, ZX); _} as mo) ->
      op 0x2d; memop mo
    | Load ({ty = I32Type; pack = Some (Pack16, SX); _} as mo) ->
      op 0x2e; memop mo
    | Load ({ty = I32Type; pack = Some (Pack16, ZX); _} as mo) ->
      op 0x2f; memop mo
    | Load {ty = I32Type; pack = Some (Pack32, _); _} ->
      error e.at "illegal instruction i32.load32"
    | Load ({ty = I64Type; pack = Some (Pack8, SX); _} as mo) ->
      op 0x30; memop mo
    | Load ({ty = I64Type; pack = Some (Pack8, ZX); _} as mo) ->
      op 0x31; memop mo
    | Load ({ty = I64Type; pack = Some (Pack16, SX); _} as mo) ->
      op 0x32; memop mo
    | Load ({ty = I64Type; pack = Some (Pack16, ZX); _} as mo) ->
      op 0x33; memop mo
    | Load ({ty = I64Type; pack = Some (Pack32, SX); _} as mo) ->
      op 0x34; memop mo
    | Load ({ty = I64Type; pack = Some (Pack32, ZX); _} as mo) ->
      op 0x35; memop mo
    | Load {ty = F32Type | F64Type; pack = Some _; _} ->
      error e.at "illegal instruction fxx.loadN"
    | Load {ty = I32Type | I64Type; pack = Some (Pack64, _); _} ->
      error e.at "illegal instruction ixx.load64"

    | Store ({ty = I32Type; pack = None; _} as mo) -> op 0x36; memop mo
    | Store ({ty = I64Type; pack = None; _} as mo) -> op 0x37; memop mo
    | Store ({ty = F32Type; pack = None; _} as mo) -> op 0x38; memop mo
    | Store ({ty = F64Type; pack = None; _} as mo) -> op 0x39; memop mo
    | Store ({ty = I32Type; pack = Some Pack8; _} as mo) -> op 0x3a; memop mo
    | Store ({ty = I32Type; pack = Some Pack16; _} as mo) -> op 0x3b; memop mo
    | Store {ty = I32Type; pack = Some Pack32; _} ->
      error e.at "illegal instruction i32.store32"
    | Store ({ty = I64Type; pack = Some Pack8; _} as mo) -> op 0x3c; memop mo
    | Store ({ty = I64Type; pack = Some Pack16; _} as mo) -> op 0x3d; memop mo
    | Store ({ty = I64Type; pack = Some Pack32; _} as mo) -> op 0x3e; memop mo
    | Store {ty = F32Type | F64Type; pack = Some _; _} ->
      error e.at "illegal instruction fxx.storeN"
    | Store {ty = (I32Type | I64Type); pack = Some Pack64; _} ->
      error e.at "illegal instruction ixx.store64"

    | VecLoad ({ty = V128Type; pack = None; _} as mo) ->
      vecop 0x00l; memop mo
    | VecLoad ({ty = V128Type; pack = Some (Pack64, ExtLane (Pack8x8, SX)); _} as mo) ->
      vecop 0x01l; memop mo
    | VecLoad ({ty = V128Type; pack = Some (Pack64, ExtLane (Pack8x8, ZX)); _} as mo) ->
      vecop 0x02l; memop mo
    | VecLoad ({ty = V128Type; pack = Some (Pack64, ExtLane (Pack16x4, SX)); _} as mo) ->
      vecop 0x03l; memop mo
    | VecLoad ({ty = V128Type; pack = Some (Pack64, ExtLane (Pack16x4, ZX)); _} as mo) ->
      vecop 0x04l; memop mo
    | VecLoad ({ty = V128Type; pack = Some (Pack64, ExtLane (Pack32x2, SX)); _} as mo) ->
      vecop 0x05l; memop mo
    | VecLoad ({ty = V128Type; pack = Some (Pack64, ExtLane (Pack32x2, ZX)); _} as mo) ->
      vecop 0x06l; memop mo
    | VecLoad ({ty = V128Type; pack = Some (Pack8, ExtSplat); _} as mo) ->
      vecop 0x07l; memop mo
    | VecLoad ({ty = V128Type; pack = Some (Pack16, ExtSplat); _} as mo) ->
      vecop 0x08l; memop mo
    | VecLoad ({ty = V128Type; pack = Some (Pack32, ExtSplat); _} as mo) ->
      vecop 0x09l; memop mo
    | VecLoad ({ty = V128Type; pack = Some (Pack64, ExtSplat); _} as mo) ->
      vecop 0x0al; memop mo
    | VecLoad ({ty = V128Type; pack = Some (Pack32, ExtZero); _} as mo) ->
      vecop 0x5cl; memop mo
    | VecLoad ({ty = V128Type; pack = Some (Pack64, ExtZero); _} as mo) ->
      vecop 0x5dl; memop mo
    | VecLoad _ ->
      error e.at "illegal instruction v128.loadNxM_<ext>"

    | VecLoadLane ({ty = V128Type; pack = Pack8; _} as mo, i) ->
      vecop 0x54l; memop mo; byte i;
    | VecLoadLane ({ty = V128Type; pack = Pack16; _} as mo, i) ->
      vecop 0x55l; memop mo; byte i;
    | VecLoadLane ({ty = V128Type; pack = Pack32; _} as mo, i) ->
      vecop 0x56l; memop mo; byte i;
    | VecLoadLane ({ty = V128Type; pack = Pack64; _} as mo, i) ->
      vecop 0x57l; memop mo; byte i;

    | VecStore ({ty = V128Type; _} as mo) -> vecop 0x0bl; memop mo

    | VecStoreLane ({ty = V128Type; pack = Pack8; _} as mo, i) ->
      vecop 0x58l; memop mo; byte i;
    | VecStoreLane ({ty = V128Type; pack = Pack16; _} as mo, i) ->
      vecop 0x59l; memop mo; byte i;
    | VecStoreLane ({ty = V128Type; pack = Pack32; _} as mo, i) ->
      vecop 0x5al; memop mo; byte i;
    | VecStoreLane ({ty = V128Type; pack = Pack64; _} as mo, i) ->
      vecop 0x5bl; memop mo; byte i;

    | MemorySize -> op 0x3f; byte 0x00
    | MemoryGrow -> op 0x40; byte 0x00
    | MemoryFill -> op 0xfc; u32 0x0bl; byte 0x00
    | MemoryCopy -> op 0xfc; u32 0x0al; byte 0x00; byte 0x00
    | MemoryInit x -> op 0xfc; u32 0x08l; var x; byte 0x00
    | DataDrop x -> op 0xfc; u32 0x09l; var x

    | RefNull t -> op 0xd0; ref_type t
    | RefIsNull -> op 0xd1
    | RefFunc x -> op 0xd2; var x

    | Const {it = I32 c; _} -> op 0x41; s32 c
    | Const {it = I64 c; _} -> op 0x42; s64 c
    | Const {it = F32 c; _} -> op 0x43; f32 c
    | Const {it = F64 c; _} -> op 0x44; f64 c

    | Test (I32 I32Op.Eqz) -> op 0x45
    | Test (I64 I64Op.Eqz) -> op 0x50
    | Test (F32 _ | F64 _) -> .

    | Compare (I32 I32Op.Eq) -> op 0x46
    | Compare (I32 I32Op.Ne) -> op 0x47
    | Compare (I32 I32Op.LtS) -> op 0x48
    | Compare (I32 I32Op.LtU) -> op 0x49
    | Compare (I32 I32Op.GtS) -> op 0x4a
    | Compare (I32 I32Op.GtU) -> op 0x4b
    | Compare (I32 I32Op.LeS) -> op 0x4c
    | Compare (I32 I32Op.LeU) -> op 0x4d
    | Compare (I32 I32Op.GeS) -> op 0x4e
    | Compare (I32 I32Op.GeU) -> op 0x4f

    | Compare (I64 I64Op.Eq) -> op 0x51
    | Compare (I64 I64Op.Ne) -> op 0x52
    | Compare (I64 I64Op.LtS) -> op 0x53
    | Compare (I64 I64Op.LtU) -> op 0x54
    | Compare (I64 I64Op.GtS) -> op 0x55
    | Compare (I64 I64Op.GtU) -> op 0x56
    | Compare (I64 I64Op.LeS) -> op 0x57
    | Compare (I64 I64Op.LeU) -> op 0x58
    | Compare (I64 I64Op.GeS) -> op 0x59
    | Compare (I64 I64Op.GeU) -> op 0x5a

    | Compare (F32 F32Op.Eq) -> op 0x5b
    | Compare (F32 F32Op.Ne) -> op 0x5c
    | Compare (F32 F32Op.Lt) -> op 0x5d
    | Compare (F32 F32Op.Gt) -> op 0x5e
    | Compare (F32 F32Op.Le) -> op 0x5f
    | Compare (F32 F32Op.Ge) -> op 0x60

    | Compare (F64 F64Op.Eq) -> op 0x61
    | Compare (F64 F64Op.Ne) -> op 0x62
    | Compare (F64 F64Op.Lt) -> op 0x63
    | Compare (F64 F64Op.Gt) -> op 0x64
    | Compare (F64 F64Op.Le) -> op 0x65
    | Compare (F64 F64Op.Ge) -> op 0x66

    | Unary (I32 I32Op.Clz) -> op 0x67
    | Unary (I32 I32Op.Ctz) -> op 0x68
    | Unary (I32 I32Op.Popcnt) -> op 0x69
    | Unary (I32 (I32Op.ExtendS Pack8)) -> op 0xc0
    | Unary (I32 (I32Op.ExtendS Pack16)) -> op 0xc1
    | Unary (I32 (I32Op.ExtendS (Pack32 | Pack64))) ->
      error e.at "illegal instruction i32.extendN_s"

    | Unary (I64 I64Op.Clz) -> op 0x79
    | Unary (I64 I64Op.Ctz) -> op 0x7a
    | Unary (I64 I64Op.Popcnt) -> op 0x7b
    | Unary (I64 (I64Op.ExtendS Pack8)) -> op 0xc2
    | Unary (I64 (I64Op.ExtendS Pack16)) -> op 0xc3
    | Unary (I64 (I64Op.ExtendS Pack32)) -> op 0xc4
    | Unary (I64 (I64Op.ExtendS Pack64)) ->
      error e.at "illegal instruction i64.extend64_s"

    | Unary (F32 F32Op.Abs) -> op 0x8b
    | Unary (F32 F32Op.Neg) -> op 0x8c
    | Unary (F32 F32Op.Ceil) -> op 0x8d
    | Unary (F32 F32Op.Floor) -> op 0x8e
    | Unary (F32 F32Op.Trunc) -> op 0x8f
    | Unary (F32 F32Op.Nearest) -> op 0x90
    | Unary (F32 F32Op.Sqrt) -> op 0x91

    | Unary (F64 F64Op.Abs) -> op 0x99
    | Unary (F64 F64Op.Neg) -> op 0x9a
    | Unary (F64 F64Op.Ceil) -> op 0x9b
    | Unary (F64 F64Op.Floor) -> op 0x9c
    | Unary (F64 F64Op.Trunc) -> op 0x9d
    | Unary (F64 F64Op.Nearest) -> op 0x9e
    | Unary (F64 F64Op.Sqrt) -> op 0x9f

    | Binary (I32 I32Op.Add) -> op 0x6a
    | Binary (I32 I32Op.Sub) -> op 0x6b
    | Binary (I32 I32Op.Mul) -> op 0x6c
    | Binary (I32 I32Op.DivS) -> op 0x6d
    | Binary (I32 I32Op.DivU) -> op 0x6e
    | Binary (I32 I32Op.RemS) -> op 0x6f
    | Binary (I32 I32Op.RemU) -> op 0x70
    | Binary (I32 I32Op.And) -> op 0x71
    | Binary (I32 I32Op.Or) -> op 0x72
    | Binary (I32 I32Op.Xor) -> op 0x73
    | Binary (I32 I32Op.Shl) -> op 0x74
    | Binary (I32 I32Op.ShrS) -> op 0x75
    | Binary (I32 I32Op.ShrU) -> op 0x76
    | Binary (I32 I32Op.Rotl) -> op 0x77
    | Binary (I32 I32Op.Rotr) -> op 0x78

    | Binary (I64 I64Op.Add) -> op 0x7c
    | Binary (I64 I64Op.Sub) -> op 0x7d
    | Binary (I64 I64Op.Mul) -> op 0x7e
    | Binary (I64 I64Op.DivS) -> op 0x7f
    | Binary (I64 I64Op.DivU) -> op 0x80
    | Binary (I64 I64Op.RemS) -> op 0x81
    | Binary (I64 I64Op.RemU) -> op 0x82
    | Binary (I64 I64Op.And) -> op 0x83
    | Binary (I64 I64Op.Or) -> op 0x84
    | Binary (I64 I64Op.Xor) -> op 0x85
    | Binary (I64 I64Op.Shl) -> op 0x86
    | Binary (I64 I64Op.ShrS) -> op 0x87
    | Binary (I64 I64Op.ShrU) -> op 0x88
    | Binary (I64 I64Op.Rotl) -> op 0x89
    | Binary (I64 I64Op.Rotr) -> op 0x8a

    | Binary (F32 F32Op.Add) -> op 0x92
    | Binary (F32 F32Op.Sub) -> op 0x93
    | Binary (F32 F32Op.Mul) -> op 0x94
    | Binary (F32 F32Op.Div) -> op 0x95
    | Binary (F32 F32Op.Min) -> op 0x96
    | Binary (F32 F32Op.Max) -> op 0x97
    | Binary (F32 F32Op.CopySign) -> op 0x98

    | Binary (F64 F64Op.Add) -> op 0xa0
    | Binary (F64 F64Op.Sub) -> op 0xa1
    | Binary (F64 F64Op.Mul) -> op 0xa2
    | Binary (F64 F64Op.Div) -> op 0xa3
    | Binary (F64 F64Op.Min) -> op 0xa4
    | Binary (F64 F64Op.Max) -> op 0xa5
    | Binary (F64 F64Op.CopySign) -> op 0xa6

    | Convert (I32 I32Op.ExtendSI32) ->
      error e.at "illegal instruction i32.extend_i32_s"
    | Convert (I32 I32Op.ExtendUI32) ->
      error e.at "illegal instruction i32.extend_i32_u"
    | Convert (I32 I32Op.WrapI64) -> op 0xa7
    | Convert (I32 I32Op.TruncSF32) -> op 0xa8
    | Convert (I32 I32Op.TruncUF32) -> op 0xa9
    | Convert (I32 I32Op.TruncSF64) -> op 0xaa
    | Convert (I32 I32Op.TruncUF64) -> op 0xab
    | Convert (I32 I32Op.TruncSatSF32) -> op 0xfc; u32 0x00l
    | Convert (I32 I32Op.TruncSatUF32) -> op 0xfc; u32 0x01l
    | Convert (I32 I32Op.TruncSatSF64) -> op 0xfc; u32 0x02l
    | Convert (I32 I32Op.TruncSatUF64) -> op 0xfc; u32 0x03l
    | Convert (I32 I32Op.ReinterpretFloat) -> op 0xbc

    | Convert (I64 I64Op.ExtendSI32) -> op 0xac
    | Convert (I64 I64Op.ExtendUI32) -> op 0xad
    | Convert (I64 I64Op.WrapI64) ->
      error e.at "illegal instruction i64.wrap_i64"
    | Convert (I64 I64Op.TruncSF32) -> op 0xae
    | Convert (I64 I64Op.TruncUF32) -> op 0xaf
    | Convert (I64 I64Op.TruncSF64) -> op 0xb0
    | Convert (I64 I64Op.TruncUF64) -> op 0xb1
    | Convert (I64 I64Op.TruncSatSF32) -> op 0xfc; u32 0x04l
    | Convert (I64 I64Op.TruncSatUF32) -> op 0xfc; u32 0x05l
    | Convert (I64 I64Op.TruncSatSF64) -> op 0xfc; u32 0x06l
    | Convert (I64 I64Op.TruncSatUF64) -> op 0xfc; u32 0x07l
    | Convert (I64 I64Op.ReinterpretFloat) -> op 0xbd

    | Convert (F32 F32Op.ConvertSI32) -> op 0xb2
    | Convert (F32 F32Op.ConvertUI32) -> op 0xb3
    | Convert (F32 F32Op.ConvertSI64) -> op 0xb4
    | Convert (F32 F32Op.ConvertUI64) -> op 0xb5
    | Convert (F32 F32Op.PromoteF32) ->
      error e.at "illegal instruction f32.promote_f32"
    | Convert (F32 F32Op.DemoteF64) -> op 0xb6
    | Convert (F32 F32Op.ReinterpretInt) -> op 0xbe

    | Convert (F64 F64Op.ConvertSI32) -> op 0xb7
    | Convert (F64 F64Op.ConvertUI32) -> op 0xb8
    | Convert (F64 F64Op.ConvertSI64) -> op 0xb9
    | Convert (F64 F64Op.ConvertUI64) -> op 0xba
    | Convert (F64 F64Op.PromoteF32) -> op 0xbb
    | Convert (F64 F64Op.DemoteF64) ->
      error e.at "illegal instruction f64.demote_f64"
    | Convert (F64 F64Op.ReinterpretInt) -> op 0xbf

    | VecConst {it = V128 c; _} -> vecop 0x0cl; v128 c

    | VecTest (V128 (I8x16 V128Op.AllTrue)) -> vecop 0x63l
    | VecTest (V128 (I16x8 V128Op.AllTrue)) -> vecop 0x83l
    | VecTest (V128 (I32x4 V128Op.AllTrue)) -> vecop 0xa3l
    | VecTest (V128 (I64x2 V128Op.AllTrue)) -> vecop 0xc3l
    | VecTest (V128 _) -> .

    | VecUnary (V128 (I8x16 V128Op.Abs)) -> vecop 0x60l
    | VecUnary (V128 (I8x16 V128Op.Neg)) -> vecop 0x61l
    | VecUnary (V128 (I8x16 V128Op.Popcnt)) -> vecop 0x62l
    | VecUnary (V128 (I16x8 V128Op.Abs)) -> vecop 0x80l
    | VecUnary (V128 (I16x8 V128Op.Neg)) -> vecop 0x81l
    | VecUnary (V128 (I16x8 V128Op.Popcnt)) ->
      error e.at "illegal instruction i16x8.popcnt"
    | VecUnary (V128 (I32x4 V128Op.Abs)) -> vecop 0xa0l
    | VecUnary (V128 (I32x4 V128Op.Neg)) -> vecop 0xa1l
    | VecUnary (V128 (I32x4 V128Op.Popcnt)) ->
      error e.at "illegal instruction i32x4.popcnt"
    | VecUnary (V128 (I64x2 V128Op.Abs)) -> vecop 0xc0l
    | VecUnary (V128 (I64x2 V128Op.Neg)) -> vecop 0xc1l
    | VecUnary (V128 (I64x2 V128Op.Popcnt)) ->
      error e.at "illegal instruction i64x2.popcnt"
    | VecUnary (V128 (F32x4 V128Op.Ceil)) -> vecop 0x67l
    | VecUnary (V128 (F32x4 V128Op.Floor)) -> vecop 0x68l
    | VecUnary (V128 (F32x4 V128Op.Trunc)) -> vecop 0x69l
    | VecUnary (V128 (F32x4 V128Op.Nearest)) -> vecop 0x6al
    | VecUnary (V128 (F64x2 V128Op.Ceil)) -> vecop 0x74l
    | VecUnary (V128 (F64x2 V128Op.Floor)) -> vecop 0x75l
    | VecUnary (V128 (F64x2 V128Op.Trunc)) -> vecop 0x7al
    | VecUnary (V128 (F64x2 V128Op.Nearest)) -> vecop 0x94l
    | VecUnary (V128 (F32x4 V128Op.Abs)) -> vecop 0xe0l
    | VecUnary (V128 (F32x4 V128Op.Neg)) -> vecop 0xe1l
    | VecUnary (V128 (F32x4 V128Op.Sqrt)) -> vecop 0xe3l
    | VecUnary (V128 (F64x2 V128Op.Abs)) -> vecop 0xecl
    | VecUnary (V128 (F64x2 V128Op.Neg)) -> vecop 0xedl
    | VecUnary (V128 (F64x2 V128Op.Sqrt)) -> vecop 0xefl

    | VecCompare (V128 (I8x16 V128Op.Eq)) -> vecop 0x23l
    | VecCompare (V128 (I8x16 V128Op.Ne)) -> vecop 0x24l
    | VecCompare (V128 (I8x16 V128Op.LtS)) -> vecop 0x25l
    | VecCompare (V128 (I8x16 V128Op.LtU)) -> vecop 0x26l
    | VecCompare (V128 (I8x16 V128Op.GtS)) -> vecop 0x27l
    | VecCompare (V128 (I8x16 V128Op.GtU)) -> vecop 0x28l
    | VecCompare (V128 (I8x16 V128Op.LeS)) -> vecop 0x29l
    | VecCompare (V128 (I8x16 V128Op.LeU)) -> vecop 0x2al
    | VecCompare (V128 (I8x16 V128Op.GeS)) -> vecop 0x2bl
    | VecCompare (V128 (I8x16 V128Op.GeU)) -> vecop 0x2cl
    | VecCompare (V128 (I16x8 V128Op.Eq)) -> vecop 0x2dl
    | VecCompare (V128 (I16x8 V128Op.Ne)) -> vecop 0x2el
    | VecCompare (V128 (I16x8 V128Op.LtS)) -> vecop 0x2fl
    | VecCompare (V128 (I16x8 V128Op.LtU)) -> vecop 0x30l
    | VecCompare (V128 (I16x8 V128Op.GtS)) -> vecop 0x31l
    | VecCompare (V128 (I16x8 V128Op.GtU)) -> vecop 0x32l
    | VecCompare (V128 (I16x8 V128Op.LeS)) -> vecop 0x33l
    | VecCompare (V128 (I16x8 V128Op.LeU)) -> vecop 0x34l
    | VecCompare (V128 (I16x8 V128Op.GeS)) -> vecop 0x35l
    | VecCompare (V128 (I16x8 V128Op.GeU)) -> vecop 0x36l
    | VecCompare (V128 (I32x4 V128Op.Eq)) -> vecop 0x37l
    | VecCompare (V128 (I32x4 V128Op.Ne)) -> vecop 0x38l
    | VecCompare (V128 (I32x4 V128Op.LtS)) -> vecop 0x39l
    | VecCompare (V128 (I32x4 V128Op.LtU)) -> vecop 0x3al
    | VecCompare (V128 (I32x4 V128Op.GtS)) -> vecop 0x3bl
    | VecCompare (V128 (I32x4 V128Op.GtU)) -> vecop 0x3cl
    | VecCompare (V128 (I32x4 V128Op.LeS)) -> vecop 0x3dl
    | VecCompare (V128 (I32x4 V128Op.LeU)) -> vecop 0x3el
    | VecCompare (V128 (I32x4 V128Op.GeS)) -> vecop 0x3fl
    | VecCompare (V128 (I32x4 V128Op.GeU)) -> vecop 0x40l
    | VecCompare (V128 (I64x2 V128Op.Eq)) -> vecop 0xd6l
    | VecCompare (V128 (I64x2 V128Op.Ne)) -> vecop 0xd7l
    | VecCompare (V128 (I64x2 V128Op.LtS)) -> vecop 0xd8l
    | VecCompare (V128 (I64x2 V128Op.LtU)) ->
      error e.at "illegal instruction i64x2.lt_u"
    | VecCompare (V128 (I64x2 V128Op.GtS)) -> vecop 0xd9l
    | VecCompare (V128 (I64x2 V128Op.GtU)) ->
      error e.at "illegal instruction i64x2.gt_u"
    | VecCompare (V128 (I64x2 V128Op.LeS)) -> vecop 0xdal
    | VecCompare (V128 (I64x2 V128Op.LeU)) ->
      error e.at "illegal instruction i64x2.le_u"
    | VecCompare (V128 (I64x2 V128Op.GeS)) -> vecop 0xdbl
    | VecCompare (V128 (I64x2 V128Op.GeU)) ->
      error e.at "illegal instruction i64x2.ge_u"
    | VecCompare (V128 (F32x4 V128Op.Eq)) -> vecop 0x41l
    | VecCompare (V128 (F32x4 V128Op.Ne)) -> vecop 0x42l
    | VecCompare (V128 (F32x4 V128Op.Lt)) -> vecop 0x43l
    | VecCompare (V128 (F32x4 V128Op.Gt)) -> vecop 0x44l
    | VecCompare (V128 (F32x4 V128Op.Le)) -> vecop 0x45l
    | VecCompare (V128 (F32x4 V128Op.Ge)) -> vecop 0x46l
    | VecCompare (V128 (F64x2 V128Op.Eq)) -> vecop 0x47l
    | VecCompare (V128 (F64x2 V128Op.Ne)) -> vecop 0x48l
    | VecCompare (V128 (F64x2 V128Op.Lt)) -> vecop 0x49l
    | VecCompare (V128 (F64x2 V128Op.Gt)) -> vecop 0x4al
    | VecCompare (V128 (F64x2 V128Op.Le)) -> vecop 0x4bl
    | VecCompare (V128 (F64x2 V128Op.Ge)) -> vecop 0x4cl

    | VecBinary (V128 (I8x16 (V128Op.Shuffle is))) -> vecop 0x0dl; List.iter byte is
    | VecBinary (V128 (I8x16 V128Op.Swizzle)) -> vecop 0x0el
    | VecBinary (V128 (I8x16 V128Op.NarrowS)) -> vecop 0x65l
    | VecBinary (V128 (I8x16 V128Op.NarrowU)) -> vecop 0x66l
    | VecBinary (V128 (I8x16 V128Op.Add)) -> vecop 0x6el
    | VecBinary (V128 (I8x16 V128Op.AddSatS)) -> vecop 0x6fl
    | VecBinary (V128 (I8x16 V128Op.AddSatU)) -> vecop 0x70l
    | VecBinary (V128 (I8x16 V128Op.Sub)) -> vecop 0x71l
    | VecBinary (V128 (I8x16 V128Op.SubSatS)) -> vecop 0x72l
    | VecBinary (V128 (I8x16 V128Op.SubSatU)) -> vecop 0x73l
    | VecBinary (V128 (I8x16 V128Op.MinS)) -> vecop 0x76l
    | VecBinary (V128 (I8x16 V128Op.MinU)) -> vecop 0x77l
    | VecBinary (V128 (I8x16 V128Op.MaxS)) -> vecop 0x78l
    | VecBinary (V128 (I8x16 V128Op.MaxU)) -> vecop 0x79l
    | VecBinary (V128 (I8x16 V128Op.AvgrU)) -> vecop 0x7bl
    | VecBinary (V128 (I16x8 V128Op.NarrowS)) -> vecop 0x85l
    | VecBinary (V128 (I16x8 V128Op.NarrowU)) -> vecop 0x86l
    | VecBinary (V128 (I16x8 V128Op.Add)) -> vecop 0x8el
    | VecBinary (V128 (I16x8 V128Op.AddSatS)) -> vecop 0x8fl
    | VecBinary (V128 (I16x8 V128Op.AddSatU)) -> vecop 0x90l
    | VecBinary (V128 (I16x8 V128Op.Sub)) -> vecop 0x91l
    | VecBinary (V128 (I16x8 V128Op.SubSatS)) -> vecop 0x92l
    | VecBinary (V128 (I16x8 V128Op.SubSatU)) -> vecop 0x93l
    | VecBinary (V128 (I16x8 V128Op.Mul)) -> vecop 0x95l
    | VecBinary (V128 (I16x8 V128Op.MinS)) -> vecop 0x96l
    | VecBinary (V128 (I16x8 V128Op.MinU)) -> vecop 0x97l
    | VecBinary (V128 (I16x8 V128Op.MaxS)) -> vecop 0x98l
    | VecBinary (V128 (I16x8 V128Op.MaxU)) -> vecop 0x99l
    | VecBinary (V128 (I16x8 V128Op.AvgrU)) -> vecop 0x9bl
    | VecBinary (V128 (I16x8 V128Op.ExtMulLowS)) -> vecop 0x9cl
    | VecBinary (V128 (I16x8 V128Op.ExtMulHighS)) -> vecop 0x9dl
    | VecBinary (V128 (I16x8 V128Op.ExtMulLowU)) -> vecop 0x9el
    | VecBinary (V128 (I16x8 V128Op.ExtMulHighU)) -> vecop 0x9fl
    | VecBinary (V128 (I16x8 V128Op.Q15MulRSatS)) -> vecop 0x82l
    | VecBinary (V128 (I32x4 V128Op.Add)) -> vecop 0xael
    | VecBinary (V128 (I32x4 V128Op.Sub)) -> vecop 0xb1l
    | VecBinary (V128 (I32x4 V128Op.MinS)) -> vecop 0xb6l
    | VecBinary (V128 (I32x4 V128Op.MinU)) -> vecop 0xb7l
    | VecBinary (V128 (I32x4 V128Op.MaxS)) -> vecop 0xb8l
    | VecBinary (V128 (I32x4 V128Op.MaxU)) -> vecop 0xb9l
    | VecBinary (V128 (I32x4 V128Op.DotS)) -> vecop 0xbal
    | VecBinary (V128 (I32x4 V128Op.Mul)) -> vecop 0xb5l
    | VecBinary (V128 (I32x4 V128Op.ExtMulLowS)) -> vecop 0xbcl
    | VecBinary (V128 (I32x4 V128Op.ExtMulHighS)) -> vecop 0xbdl
    | VecBinary (V128 (I32x4 V128Op.ExtMulLowU)) -> vecop 0xbel
    | VecBinary (V128 (I32x4 V128Op.ExtMulHighU)) -> vecop 0xbfl
    | VecBinary (V128 (I64x2 V128Op.Add)) -> vecop 0xcel
    | VecBinary (V128 (I64x2 V128Op.Sub)) -> vecop 0xd1l
    | VecBinary (V128 (I64x2 V128Op.Mul)) -> vecop 0xd5l
    | VecBinary (V128 (I64x2 V128Op.ExtMulLowS)) -> vecop 0xdcl
    | VecBinary (V128 (I64x2 V128Op.ExtMulHighS)) -> vecop 0xddl
    | VecBinary (V128 (I64x2 V128Op.ExtMulLowU)) -> vecop 0xdel
    | VecBinary (V128 (I64x2 V128Op.ExtMulHighU)) -> vecop 0xdfl
    | VecBinary (V128 (F32x4 V128Op.Add)) -> vecop 0xe4l
    | VecBinary (V128 (F32x4 V128Op.Sub)) -> vecop 0xe5l
    | VecBinary (V128 (F32x4 V128Op.Mul)) -> vecop 0xe6l
    | VecBinary (V128 (F32x4 V128Op.Div)) -> vecop 0xe7l
    | VecBinary (V128 (F32x4 V128Op.Min)) -> vecop 0xe8l
    | VecBinary (V128 (F32x4 V128Op.Max)) -> vecop 0xe9l
    | VecBinary (V128 (F32x4 V128Op.Pmin)) -> vecop 0xeal
    | VecBinary (V128 (F32x4 V128Op.Pmax)) -> vecop 0xebl
    | VecBinary (V128 (F64x2 V128Op.Add)) -> vecop 0xf0l
    | VecBinary (V128 (F64x2 V128Op.Sub)) -> vecop 0xf1l
    | VecBinary (V128 (F64x2 V128Op.Mul)) -> vecop 0xf2l
    | VecBinary (V128 (F64x2 V128Op.Div)) -> vecop 0xf3l
    | VecBinary (V128 (F64x2 V128Op.Min)) -> vecop 0xf4l
    | VecBinary (V128 (F64x2 V128Op.Max)) -> vecop 0xf5l
    | VecBinary (V128 (F64x2 V128Op.Pmin)) -> vecop 0xf6l
    | VecBinary (V128 (F64x2 V128Op.Pmax)) -> vecop 0xf7l
    | VecBinary (V128 _) ->
      error e.at "illegal binary vector instruction"

    | VecConvert (V128 (I8x16 _)) ->
      error e.at "illegal i8x16 conversion instruction"
    | VecConvert (V128 (I16x8 V128Op.ExtendLowS)) -> vecop 0x87l
    | VecConvert (V128 (I16x8 V128Op.ExtendHighS)) -> vecop 0x88l
    | VecConvert (V128 (I16x8 V128Op.ExtendLowU)) -> vecop 0x89l
    | VecConvert (V128 (I16x8 V128Op.ExtendHighU)) -> vecop 0x8al
    | VecConvert (V128 (I16x8 V128Op.ExtAddPairwiseS)) -> vecop 0x7cl
    | VecConvert (V128 (I16x8 V128Op.ExtAddPairwiseU)) -> vecop 0x7dl
    | VecConvert (V128 (I16x8 _)) ->
      error e.at "illegal i16x8 conversion instruction"
    | VecConvert (V128 (I32x4 V128Op.ExtendLowS)) -> vecop 0xa7l
    | VecConvert (V128 (I32x4 V128Op.ExtendHighS)) -> vecop 0xa8l
    | VecConvert (V128 (I32x4 V128Op.ExtendLowU)) -> vecop 0xa9l
    | VecConvert (V128 (I32x4 V128Op.ExtendHighU)) -> vecop 0xaal
    | VecConvert (V128 (I32x4 V128Op.ExtAddPairwiseS)) -> vecop 0x7el
    | VecConvert (V128 (I32x4 V128Op.ExtAddPairwiseU)) -> vecop 0x7fl
    | VecConvert (V128 (I32x4 V128Op.TruncSatSF32x4)) -> vecop 0xf8l
    | VecConvert (V128 (I32x4 V128Op.TruncSatUF32x4)) -> vecop 0xf9l
    | VecConvert (V128 (I32x4 V128Op.TruncSatSZeroF64x2)) -> vecop 0xfcl
    | VecConvert (V128 (I32x4 V128Op.TruncSatUZeroF64x2)) -> vecop 0xfdl
    | VecConvert (V128 (I64x2 V128Op.ExtendLowS)) -> vecop 0xc7l
    | VecConvert (V128 (I64x2 V128Op.ExtendHighS)) -> vecop 0xc8l
    | VecConvert (V128 (I64x2 V128Op.ExtendLowU)) -> vecop 0xc9l
    | VecConvert (V128 (I64x2 V128Op.ExtendHighU)) -> vecop 0xcal
    | VecConvert (V128 (I64x2 _)) ->
      error e.at "illegal i64x2 conversion instruction"
    | VecConvert (V128 (F32x4 V128Op.DemoteZeroF64x2)) -> vecop 0x5el
    | VecConvert (V128 (F32x4 V128Op.PromoteLowF32x4)) ->
      error e.at "illegal instruction f32x4.promote_low_f32x4"
    | VecConvert (V128 (F32x4 V128Op.ConvertSI32x4)) -> vecop 0xfal
    | VecConvert (V128 (F32x4 V128Op.ConvertUI32x4)) -> vecop 0xfbl
    | VecConvert (V128 (F64x2 V128Op.DemoteZeroF64x2)) ->
      error e.at "illegal instruction f64x2.demote_zero_f64x2"
    | VecConvert (V128 (F64x2 V128Op.PromoteLowF32x4)) -> vecop 0x5fl
    | VecConvert (V128 (F64x2 V128Op.ConvertSI32x4)) -> vecop 0xfel
    | VecConvert (V128 (F64x2 V128Op.ConvertUI32x4)) -> vecop 0xffl

    | VecShift (V128 (I8x16 V128Op.Shl)) -> vecop 0x6bl
    | VecShift (V128 (I8x16 V128Op.ShrS)) -> vecop 0x6cl
    | VecShift (V128 (I8x16 V128Op.ShrU)) -> vecop 0x6dl
    | VecShift (V128 (I16x8 V128Op.Shl)) -> vecop 0x8bl
    | VecShift (V128 (I16x8 V128Op.ShrS)) -> vecop 0x8cl
    | VecShift (V128 (I16x8 V128Op.ShrU)) -> vecop 0x8dl
    | VecShift (V128 (I32x4 V128Op.Shl)) -> vecop 0xabl
    | VecShift (V128 (I32x4 V128Op.ShrS)) -> vecop 0xacl
    | VecShift (V128 (I32x4 V128Op.ShrU)) -> vecop 0xadl
    | VecShift (V128 (I64x2 V128Op.Shl)) -> vecop 0xcbl
    | VecShift (V128 (I64x2 V128Op.ShrS)) -> vecop 0xccl
    | VecShift (V128 (I64x2 V128Op.ShrU)) -> vecop 0xcdl
    | VecShift (V128 _) -> .

    | VecBitmask (V128 (I8x16 V128Op.Bitmask)) -> vecop 0x64l
    | VecBitmask (V128 (I16x8 V128Op.Bitmask)) -> vecop 0x84l
    | VecBitmask (V128 (I32x4 V128Op.Bitmask)) -> vecop 0xa4l
    | VecBitmask (V128 (I64x2 V128Op.Bitmask)) -> vecop 0xc4l
    | VecBitmask (V128 _) -> .

    | VecTestBits (V128 V128Op.AnyTrue) -> vecop 0x53l
    | VecUnaryBits (V128 V128Op.Not) -> vecop 0x4dl
    | VecBinaryBits (V128 V128Op.And) -> vecop 0x4el
    | VecBinaryBits (V128 V128Op.AndNot) -> vecop 0x4fl
    | VecBinaryBits (V128 V128Op.Or) -> vecop 0x50l
    | VecBinaryBits (V128 V128Op.Xor) -> vecop 0x51l
    | VecTernaryBits (V128 V128Op.Bitselect) -> vecop 0x52l

    | VecSplat (V128 ((I8x16 V128Op.Splat))) -> vecop 0x0fl
    | VecSplat (V128 ((I16x8 V128Op.Splat))) -> vecop 0x10l
    | VecSplat (V128 ((I32x4 V128Op.Splat))) -> vecop 0x11l
    | VecSplat (V128 ((I64x2 V128Op.Splat))) -> vecop 0x12l
    | VecSplat (V128 ((F32x4 V128Op.Splat))) -> vecop 0x13l
    | VecSplat (V128 ((F64x2 V128Op.Splat))) -> vecop 0x14l

    | VecExtract (V128 (I8x16 (V128Op.Extract (i, SX)))) -> vecop 0x15l; byte i
    | VecExtract (V128 (I8x16 (V128Op.Extract (i, ZX)))) -> vecop 0x16l; byte i
    | VecExtract (V128 (I16x8 (V128Op.Extract (i, SX)))) -> vecop 0x18l; byte i
    | VecExtract (V128 (I16x8 (V128Op.Extract (i, ZX)))) -> vecop 0x19l; byte i
    | VecExtract (V128 (I32x4 (V128Op.Extract (i, ())))) -> vecop 0x1bl; byte i
    | VecExtract (V128 (I64x2 (V128Op.Extract (i, ())))) -> vecop 0x1dl; byte i
    | VecExtract (V128 (F32x4 (V128Op.Extract (i, ())))) -> vecop 0x1fl; byte i
    | VecExtract (V128 (F64x2 (V128Op.Extract (i, ())))) -> vecop 0x21l; byte i

    | VecReplace (V128 (I8x16 (V128Op.Replace i))) -> vecop 0x17l; byte i
    | VecReplace (V128 (I16x8 (V128Op.Replace i))) -> vecop 0x1al; byte i
    | VecReplace (V128 (I32x4 (V128Op.Replace i))) -> vecop 0x1cl; byte i
    | VecReplace (V128 (I64x2 (V128Op.Replace i))) -> vecop 0x1el; byte i
    | VecReplace (V128 (F32x4 (V128Op.Replace i))) -> vecop 0x20l; byte i
    | VecReplace (V128 (F64x2 (V128Op.Replace i))) -> vecop 0x22l; byte i
    
    | FuncSymbol symbol ->
      op 0x41;
      let p = pos s in
      (* let _, index = Linking.find_symbol_index m.it.symbols (fun s -> match s.it.details with Function when s.it.name = symbol -> true | _ -> false) symbol in *)
      code_relocations := !code_relocations @ [R_WASM_TABLE_INDEX_SLEB (Int32.of_int p, symbol)];
      let import_funcs = List.filter (fun i -> match i.it.idesc.it with FuncImport _ -> true | _ -> false) m.it.imports in
      vs32_fixed (Linking.func_index m.it.funcs import_funcs symbol)
    | DataSymbol symbol ->
      op 0x41;
      let p = pos s in
      let s, _ = Linking.find_symbol_index m.it.symbols (fun s -> match s.it.details with Function when s.it.name = symbol -> true |  Data _ when s.it.name = symbol -> true | _ -> false) in
      match s.it.details with 
        Data {offset; _} ->
          code_relocations := !code_relocations @ [R_WASM_MEMORY_ADDR_SLEB (Int32.of_int p, symbol)];
          vs32_fixed offset.it
      | Function ->
          code_relocations := !code_relocations @ [R_WASM_TABLE_INDEX_SLEB (Int32.of_int p, symbol)];
          let import_funcs = List.filter (fun i -> match i.it.idesc.it with FuncImport _ -> true | _ -> false) m.it.imports in
          vs32_fixed (Linking.func_index m.it.funcs import_funcs symbol)
      | _ -> ()

  let const c =
    list (instr []) c.it; end_ ()

  (* Sections *)
  let code_section_index = ref 0
  let data_section_index = ref 0
  let section_counter = ref 0
  let section id f x needed =
    if needed then begin
      section_counter := !section_counter + 1;
      byte id;
      let g = gap32 () in
      let p = pos s in
      f x;
      patch_gap32 g (pos s - p)
    end
  

  (* Type section *)

  let type_ t = type_symbol t.it

  let type_section ts =
    section 1 (vec type_) ts (ts <> [])


  (* Import section *)

  let import_desc d =
    match d.it with
    | FuncImport x -> byte 0x00; var x
    | FuncImport_symbol x -> byte 0x00; u32 (Linking.find_type m.it.types x)
    | TableImport t -> byte 0x01; table_type t
    | MemoryImport t -> byte 0x02; memory_type t
    | GlobalImport t -> byte 0x03; global_type t

  let import im =
    let {module_name; item_name; idesc} = im.it in
    name module_name; name item_name; import_desc idesc

  let import_section ims =
    section 2 (vec import) ims (ims <> [])


  (* Function section *)

  let func (f: func' Source.phrase) = 
    match f.it with 
      FuncSymbol {ftype; _} ->
        u32 (Linking.find_type m.it.types ftype)
    | FuncNoSymbol f ->
        var f.ftype
    
  let func_section fs =
    section 3 (vec func) fs (fs <> [])


  (* Table section *)

  let table tab =
    let {ttype} = tab.it in
    table_type ttype

  let table_section tabs =
    section 4 (vec table) tabs (tabs <> [])


  (* Memory section *)

  let memory mem =
    let {mtype} = mem.it in
    memory_type mtype

  let memory_section mems =
    section 5 (vec memory) mems (mems <> [])


  (* Global section *)
  
  let global g =
    let {gtype; ginit; _} = g.it in
    global_type gtype; const ginit

  let global_section gs =
    section 6 (vec global) gs (gs <> [])


  (* Export section *)

  let export_desc d =
    match d.it with
    | FuncExport x -> byte 0; var x
    | TableExport x -> byte 1; var x
    | MemoryExport x -> byte 2; var x
    | GlobalExport x -> byte 3; var x

  let export ex =
    let {name = n; edesc} = ex.it in
    name n; export_desc edesc

  let export_section exs =
    section 7 (vec export) exs (exs <> [])


  (* Start section *)

  let start st =
    let {sfunc} = st.it in
    var sfunc

  let start_section xo =
    section 8 (opt start) xo (xo <> None)


  (* Code section *)

  let local (t, n) = len n; value_type t

  let local_symbol ((_, t), n) = local (t, n)

  let locals locs l =
    let combine t = function
      | (t', n) :: ts when t = t' -> (t, n + 1) :: ts
      | ts -> (t, 1) :: ts
    in vec l (List.fold_right combine locs [])

  let code f =
    match f.it with 
      FuncSymbol {locals = locs; body;  _} -> 
        let g = gap32 () in
        let p = pos s in
        locals locs local_symbol;
        list (instr locs) body;
        end_ ();
        patch_gap32 g (pos s - p)
    | FuncNoSymbol {locals = locs ; body; _} ->
      let g = gap32 () in
      let p = pos s in
      locals locs local_symbol;
      list (instr locs) body;
      end_ ();
      patch_gap32 g (pos s - p)

  let code_section fs =
    code_section_index := !section_counter;
    code_pos := Int32.of_int (pos s + 6);
    section 10 (vec code) fs (fs <> [])


(* Element section *)

  let is_elem_kind = function
    | FuncRefType -> true
    | _ -> false

  let elem_kind = function
    | FuncRefType -> byte 0x00
    | _ -> assert false

  let is_elem_index e =
    match e.it with
    | [{it = RefFunc _; _}] -> true
    | _ -> false

  let elem_index e =
    match e.it with
    | [{it = RefFunc x; _}] -> var x
    | _ -> assert false

  let elem seg =
    let {etype; einit; emode} = seg.it in
    if is_elem_kind etype && List.for_all is_elem_index einit then
      match emode.it with
      | Passive ->
        u32 0x01l; elem_kind etype; vec elem_index einit
      | Active {index; offset} when index.it = 0l && is_elem_kind etype ->
        u32 0x00l; const offset; vec elem_index einit
      | Active {index; offset} ->
        u32 0x02l;
        var index; const offset; elem_kind etype; vec elem_index einit
      | Declarative ->
        u32 0x03l; elem_kind etype; vec elem_index einit
    else
      match emode.it with
      | Passive ->
        u32 0x05l; ref_type etype; vec const einit
      | Active {index; offset} when index.it = 0l && is_elem_kind etype ->
        u32 0x04l; const offset; vec const einit
      | Active {index; offset} ->
        u32 0x06l; var index; const offset; ref_type etype; vec const einit
      | Declarative ->
        u32 0x07l; ref_type etype; vec const einit

  let elem_section elems =
    section 9 (vec elem) elems (elems <> [])


  (* Data section *)

  let data_part_list (data_part_list: data_part) =      
    let g = gap32 () in      
    let start = pos s in
    List.iter (fun f ->
      match f with
      | String bs -> 
        put_string s bs
      | Float32 f -> 
        f32 f
      | Float64 f -> 
        f64 f
      | Symbol symbol ->
        let p = pos s in          
        let found = ref false in
        List.iteri (fun symbol_index (s: Ast.sym_info) -> 
        match s.it.details with
          | Data { index; offset; _ } when s.it.name = symbol -> 
          found := true;
          data_relocations := !data_relocations @ [R_WASM_MEMORY_ADDR_I32 (Int32.of_int p, symbol)];
          if offset.it = (-1l) then
            word32 0l
          else
            word32 offset.it
          | Function
          | Import _ when s.it.name = symbol ->
          found := true;
          let import_funcs = List.filter (fun i -> match i.it.idesc.it with FuncImport _ -> true | _ -> false) m.it.imports in
          let symbol_index = Linking.func_index m.it.funcs import_funcs symbol in
          data_relocations := !data_relocations @ [R_WASM_TABLE_INDEX_I32 (Int32.of_int p, symbol)]; 
          word32 symbol_index
          | Global _ when s.it.name = symbol ->
          failwith "Not handling a global here..."
          | _ -> ()
        ) m.it.symbols;
        if not !found then (
          failwith ("Not found symbol: " ^ symbol)
        )
      | FunctionLoc symbol -> 
        let p = pos s in
        let import_funcs = List.filter (fun i -> match i.it.idesc.it with FuncImport _ -> true | _ -> false) m.it.imports in
        let symbol_index = Linking.func_index m.it.funcs import_funcs symbol in
        data_relocations := !data_relocations @ [R_WASM_TABLE_INDEX_I32 (Int32.of_int p, symbol)];
        word32 symbol_index        
      | Int32 i32 -> 
        word32 i32
      | Int64 i64 -> 
        word64 i64
      | Nativeint ni -> 
        word32 (Nativeint.to_int32 ni)
      | Int16 i -> 
        word16 i
      | Int8 i -> 
        byte i
    ) data_part_list.detail;
    let l = pos s - start in
    patch_gap32 g l

  let data_segment seg = (* https://github.com/WebAssembly/design/blob/master/BinaryEncoding.md#data-section *)    
    let {dinit; dmode} = seg.it in
    match dmode.it with
    | Passive ->
      u32 0x01l; data_part_list dinit
    | Active {index; offset} when index.it = 0l ->
      u32 0x00l; const offset; data_part_list dinit
    | Active {index; offset} ->
      u32 0x02l; var index; const offset; data_part_list dinit
    | Declarative ->
      error dmode.at "illegal declarative data segment"

  let data_section datas =
    data_section_index := !section_counter;      
    data_pos := Int32.of_int (pos s + 6);
    section 11 (vec data_segment) datas (datas <> [])

  (* Custom section *)

  let custom_section id f x needed =
    if needed then begin
      byte 0;
      let g = gap32 () in        
      let p = pos s in
      string id;
      f x;
      patch_gap32 g (pos s - p);        
    end  

  (* Name section *)

  let name_section_impl m =       
    byte 1; (* functions *)
    let g = gap32 () in
    let p = pos s in
    let import_funcs = List.filter (fun i -> match i.it.idesc.it with FuncImport _ -> true | _ -> false) m.it.imports in
    u32 (Int32.of_int (List.length import_funcs + List.length m.it.funcs));
    
    List.iteri (fun i import ->
      u32 (Int32.of_int i);
      string (Ast.string_of_name import.it.item_name)
    ) import_funcs;
    List.iteri (fun i (f: Ast.func) ->
      u32 (Int32.of_int (List.length import_funcs + i));
      (match f.it with 
        FuncSymbol f -> string f.name;
      | FuncNoSymbol f -> string (Int32.to_string f.ftype.it) )
    ) m.it.funcs;
    patch_gap32 g (pos s - p);

    byte 2; (* locals *)
    let g = gap32 () in
    let p = pos s in
    u32 (Int32.of_int (List.length m.it.funcs));
    List.iteri(fun i (f: Ast.func) ->
      u32 (Int32.of_int (List.length import_funcs + i));
      match f.it with 
        FuncSymbol f ->
          u32 (Int32.of_int (List.length f.locals));
          List.iteri(fun i (name, _) ->
            u32 (Int32.of_int i);
            string name;
          ) f.locals
      | FuncNoSymbol f ->
        u32 (Int32.of_int (List.length f.locals));
        List.iteri(fun i (_, _) ->
          u32 (Int32.of_int i);
          string (string_of_int i);
        ) f.locals
    ) m.it.funcs;
    patch_gap32 g (pos s - p)

  let name_section m =
    custom_section "name" name_section_impl m (m.it.datas <> [] && m.it.imports <> [] && m.it.funcs <> [] && m.it.funcs <> [])

  (* Linking *)

  let reloc_code data =
    u32 (Int32.of_int !code_section_index);
    u32 (Int32.of_int (List.length !code_relocations));      
    List.iteri (fun i r -> 
    (        
      match r with        
      | R_WASM_TABLE_INDEX_SLEB (offset, symbol_) 
      | R_WASM_MEMORY_ADDR_SLEB (offset, symbol_) -> ( 
        let exists = ref false in
        List.iteri (fun symbol_index s -> match s.it.details with          
        | Data {index; _}  when s.it.name = symbol_ ->
          exists := true;
          byte 4;
          u32 (Int32.sub offset !code_pos);
          vs32_fixed (Int32.of_int symbol_index); 
          s32 0l
        | Import _
        | Function when s.it.name = symbol_ -> 
          exists := true;            
          byte 1;            
          u32 (Int32.sub offset !code_pos);
          vs32_fixed (Int32.of_int symbol_index); 
          | _ -> ()  
        ) m.it.symbols;
        if not !exists then (
          failwith ("Could not write relocation for:" ^ symbol_)
        )
      )
      | R_WASM_FUNCTION_INDEX_LEB (offset, symbol) ->
        let _, symbol_index = 
          Linking.find_symbol_index 
            m.it.symbols 
            (fun f -> 
              match f.it.details with 
                Function | Import _ when f.it.name = symbol -> true 
              | _ -> false) 
        in
        byte 0;
        u32 (Int32.sub offset !code_pos);
        vu32_fixed symbol_index;

      | R_WASM_TYPE_INDEX_LEB (offset, index) ->
        byte 6;
        u32 (Int32.sub offset !code_pos); 
        vu32_fixed index.it
      | R_WASM_GLOBAL_INDEX_LEB (offset, symbol) ->
        byte 7;
        u32 (Int32.sub offset !code_pos);
        let pos = {file = "dummy"; line = -1; column = -1} in
        let dummy_at = {left = pos; right = pos} in
        let symbol_index = Linking.find_global_index m.it.symbols dummy_at symbol in
        vu32_fixed symbol_index.it
    )
    ) !code_relocations
    

  let relocate_code_section data =
    custom_section "reloc.CODE" reloc_code data (data <> [])

  let reloc_data data =
    u32 (Int32.of_int !data_section_index);
    u32 (Int32.of_int (List.length !data_relocations));    
    List.iter (fun r ->
      match r with      
      | R_WASM_TABLE_INDEX_I32 (offset, symbol) -> ( 
        let _, symbol_index = 
          Linking.find_symbol_index 
            m.it.symbols 
            (fun f -> 
              match f.it.details with 
                Function | Import _ when f.it.name = symbol -> true 
              | _ -> false) 
        in  
        byte 2;
        u32 (Int32.sub offset !data_pos);          
        u32 symbol_index;
      )
      | R_WASM_MEMORY_ADDR_I32 (offset, symbol_) -> (
        let _, symbol_index = 
          Linking.find_symbol_index 
            m.it.symbols 
            (fun f -> 
              match f.it.details with 
                Data _ when f.it.name = symbol_ -> true 
              | _ -> false) 
          in  
          byte 5;
          u32 (Int32.sub offset !data_pos);
          u32 symbol_index;
          s32 0l
      )
    ) !data_relocations

  let relocate_data_section data =
    custom_section "reloc.DATA" reloc_data data (data <> [])

  let segment (sym: sym_info) = 
    let sym = sym.it in
    string sym.name;
    u32 4l;
    u32 0l

  let symbol (sym: sym_info) =
    let sym = sym.it in   
    (match sym.details with
    | Import _
    | Function -> byte 0
    | Data _ ->  byte 1
    | Global _ -> byte 2
    );

    let flags = ref (
      match sym.details with 
      | Import _ -> 0l
      | _ -> 1l
    )
    in
    let exists = (match sym.details with      
    | Function -> 
      (if not (List.exists (fun (f:Ast.func) -> 
        match f.it with 
          FuncSymbol f when f.name = sym.name -> true
        | _ -> false  
        ) m.it.funcs) then ( 
        failwith ("BUG: symbol " ^ sym.name ^ " appears to refer to a nonexisting function, perhaps it should refer to an import instead?"));            
      );
      (List.exists (fun (f:Ast.func) -> match f.it with FuncSymbol f when f.name = sym.name -> true | _ -> false ) m.it.funcs)
    | Import _ -> false
    | Data _ -> (Linking.data_index m.it.datas sym.name) <> -1l && sym.name <> "__heap_base"
    | Global _ -> true
    )
    in
    (if not exists then 
    (      
      flags := Int32.logor !flags 16l
    ));
    (match sym.details with 
    | Global _ -> flags := Int32.logor !flags 4l 
    | _ -> ());
    u32 !flags;  
    let import_funcs = List.filter (fun i -> match i.it.idesc.it with FuncImport _ -> true | _ -> false) m.it.imports in
    (match sym.details with
    | Global f ->         
      u32 f.index.it;
      if exists then (
        string sym.name
      )
    | Function ->
      u32 (Linking.func_index m.it.funcs import_funcs sym.name);
      if exists then (
        string sym.name
      )

    | Import _ ->        
      u32 (Linking.func_index m.it.funcs import_funcs sym.name);
    | Data d -> (    
      (if sym.name <> "" then (              
        string sym.name
      )
      else         
      string "_"); 
      (* probably an initial block ?!malformed uleb128 *)
      if exists then (
        u32 (Linking.data_index m.it.datas sym.name);
        u32 d.relocation_offset.it;
        u32 d.size.it
      )
      )
    )        
    


  let symbol_table (data2:data_segment list) =
    u32 2l;
    byte 5;
    let g = gap32 () in
    let p = pos s in
    (* improve: should be both in data and in the symbols *)
    let segments = List.filter (fun (f : sym_info) -> f.it.name <> "__heap_base" && match f.it.details with Data _ -> true  | _ -> false) m.it.symbols in
    vec segment segments;
    patch_gap32 g (pos s - p);
    byte 8; (* WASM_SYMBOL_TABLE *)
    let g = gap32 () in
    let p = pos s in
    vec symbol m.it.symbols;
    patch_gap32 g (pos s - p)

  let linking_section data =
    custom_section "linking" symbol_table data true

  (* Module *)

  let module_ (m: Ast.module_) =
    word32 0x6d736100l;
    word32 version;
    type_section m.it.types;
    import_section m.it.imports;
    func_section m.it.funcs;
    table_section m.it.tables;
    memory_section m.it.memories;
    global_section m.it.globals;
    export_section m.it.exports;
    start_section m.it.start;
    elem_section m.it.elems;
    code_section m.it.funcs;
    data_section m.it.datas;
    linking_section m.it.datas;
    if List.length !code_relocations > 0 then
      relocate_code_section m.it.funcs;
    if List.length !data_relocations > 0 then
      relocate_data_section m.it.funcs;
    name_section m;
end
in E.module_ m; to_string s
