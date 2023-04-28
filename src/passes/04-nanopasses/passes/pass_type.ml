open Ast_unified
open Simple_utils.Trace

(* it's preferable to use the defined recursion schemes for code transformations
   but  *)
type 'a code_transformation = 'a -> 'a

(* to dynamically check if reduction happened.
  , the second rhs of the pair allows for combination of reductions checks
*)
type 'a dyn_reduction_check = Iter.iter * (Iter.iter -> 'a -> unit)

type 'a sub_pass =
  { forward : 'a code_transformation
  ; forward_check : 'a dyn_reduction_check
  ; backward : 'a code_transformation
  }

type pass =
  { name : string
  ; expression : expr sub_pass
  ; program : program sub_pass
  ; pattern : pattern sub_pass
  ; ty_expr : ty_expr sub_pass
  ; block : block sub_pass
  (* TODO: include all sorts here ? shoudln't be that costly *)
  }

type raise_t = (Errors.t, Main_warnings.all) raise
type syntax_t = Syntax_types.t

module type T = sig
  val pass : raise:(Errors.t, Main_warnings.all) raise -> syntax:Syntax_types.t -> pass
end

module Selector : sig
  (* select a specific sort transformation from a pass *)
  type 'a t

  val select : 'a t -> pass -> 'a sub_pass
  val expr : expr t
  val program : program t
  val block : block t
  val pattern : pattern t
  val ty_expr : ty_expr t
end = struct
  type 'a t = pass -> 'a sub_pass

  let select (selector : 'a t) (p : pass) = selector p
  let block x = x.block
  let expr x = x.expression
  let program x = x.program
  let pattern x = x.pattern
  let ty_expr x = x.ty_expr
end

let pass_of_module : raise:raise_t -> syntax:syntax_t -> (module T) -> pass =
 fun ~raise ~syntax (module P) -> P.pass ~raise ~syntax


let rec select_passes included name passes =
  match passes with
  | [] -> []
  | (pass, true) :: tl ->
    if String.equal name pass.name
    then if included then [ pass ] else []
    else pass :: select_passes included name tl
  | _ :: tl -> select_passes included name tl


let compile_with_passes : type a. a sub_pass list -> a -> a =
 fun passes prg ->
  let combine_checks : a dyn_reduction_check list -> a -> unit =
   fun checks ->
    let iters = List.map ~f:fst checks in
    let combined_iter = Iter.combine_iteration iters in
    match checks with
    | [] -> fun _ -> ()
    | (_, f) :: _ -> f combined_iter
  in
  let f : a * a dyn_reduction_check list -> a sub_pass -> a * a dyn_reduction_check list =
   fun (prg, checks) pass ->
    let prg = pass.forward prg in
    (* checking all the reductions so far *)
    let checks = pass.forward_check :: checks in
    (combine_checks checks) prg;
    prg, checks
  in
  let prg, _ = List.fold passes ~init:(prg, []) ~f in
  prg


let decompile_with_passes
    : type a.
      raise:raise_t
      -> syntax:syntax_t
      -> sort:a Selector.t
      -> ((module T) * bool) list
      -> a
      -> a
  =
 fun ~raise ~syntax ~sort passes prg ->
  let passes =
    passes
    |> List.filter_map ~f:(fun (p, enabled) -> if enabled then Some p else None)
    |> List.map ~f:(pass_of_module ~raise ~syntax)
    |> List.map ~f:(Selector.select sort)
  in
  List.fold passes ~init:prg ~f:(fun prg pass -> pass.backward prg)


let nanopasses_until
    : type a.
      raise:raise_t
      -> syntax:syntax_t
      -> ((module T) * bool) list
      -> ?stop_before:_
      -> sort:a Selector.t
      -> a
      -> a
  =
 fun ~raise ~syntax passes ?stop_before ~sort prg ->
  let passes =
    List.map ~f:(Simple_utils.Pair.map_fst ~f:(pass_of_module ~raise ~syntax)) passes
  in
  let passes =
    let default =
      List.filter_map ~f:(fun (p, enabled) -> if enabled then Some p else None) passes
    in
    Option.value_map stop_before ~default ~f:(fun stop ->
        let included, stop =
          match String.lsplit2 stop ~on:'+' with
          | Some (name, "") -> true, name
          | _ -> false, stop
        in
        let stop = String.lowercase stop in
        if not (List.exists passes ~f:(fun (p, _) -> String.equal stop p.name))
        then failwith "No pass with the specified name";
        if List.exists passes ~f:(fun (p, enabled) ->
               String.equal stop p.name && not enabled)
        then failwith "A pass exist with the specified name but isn't enabled";
        select_passes included stop passes)
  in
  compile_with_passes (List.map ~f:(Selector.select sort) passes) prg


let idle_cata_pass = Catamorphism.idle

type 'a pass_unfold =
  ( expr * 'a
  , ty_expr * 'a
  , pattern * 'a
  , statement * 'a
  , block * 'a
  , mod_expr * 'a
  , instruction * 'a
  , declaration * 'a
  , program_entry * 'a
  , program * 'a )
  Ast_unified.Anamorphism.unfold

type 'a pass_fold =
  ( expr * 'a
  , ty_expr * 'a
  , pattern * 'a
  , statement * 'a
  , block * 'a
  , mod_expr * 'a
  , instruction * 'a
  , declaration * 'a
  , program_entry * 'a
  , program * 'a )
  Ast_unified.Catamorphism.fold

let default_unfold : 'a pass_unfold =
  let prop acc x = x, acc in
  { expr =
      (fun (x, acc) ->
        map_expr_ (prop acc) (prop acc) (prop acc) (prop acc) (prop acc) x.fp)
  ; ty_expr = (fun (x, acc) -> map_ty_expr_ (prop acc) x.fp)
  ; pattern = (fun (x, acc) -> map_pattern_ (prop acc) (prop acc) x.fp)
  ; statement = (fun (x, acc) -> map_statement_ (prop acc) (prop acc) (prop acc) x.fp)
  ; block = (fun (x, acc) -> map_block_ (prop acc) (prop acc) x.fp)
  ; mod_expr = (fun (x, acc) -> map_mod_expr_ (prop acc) (prop acc) x.fp)
  ; instruction =
      (fun (x, acc) ->
        map_instruction_ (prop acc) (prop acc) (prop acc) (prop acc) (prop acc) x.fp)
  ; declaration =
      (fun (x, acc) ->
        map_declaration_ (prop acc) (prop acc) (prop acc) (prop acc) (prop acc) x.fp)
  ; program_entry =
      (fun (x, acc) -> map_program_entry_ (prop acc) (prop acc) (prop acc) x.fp)
  ; program = (fun (x, acc) -> map_program_ (prop acc) (prop acc) x.fp)
  }


let default_fold plus init : 'a pass_fold =
  let p acc (_, el) = plus acc el in
  { expr =
      (fun x -> { fp = map_expr_ fst fst fst fst fst x }, fold_expr_ p p p p p init x)
  ; ty_expr = (fun x -> { fp = map_ty_expr_ fst x }, fold_ty_expr_ p init x)
  ; pattern = (fun x -> { fp = map_pattern_ fst fst x }, fold_pattern_ p p init x)
  ; statement =
      (fun x -> { fp = map_statement_ fst fst fst x }, fold_statement_ p p p init x)
  ; block = (fun x -> { fp = map_block_ fst fst x }, fold_block_ p p init x)
  ; mod_expr = (fun x -> { fp = map_mod_expr_ fst fst x }, fold_mod_expr_ p p init x)
  ; instruction =
      (fun x ->
        ( { fp = map_instruction_ fst fst fst fst fst x }
        , fold_instruction_ p p p p p init x ))
  ; declaration =
      (fun x ->
        ( { fp = map_declaration_ fst fst fst fst fst x }
        , fold_declaration_ p p p p p init x ))
  ; program_entry =
      (fun x ->
        { fp = map_program_entry_ fst fst fst x }, fold_program_entry_ p p p init x)
  ; program = (fun x -> { fp = map_program_ fst fst x }, fold_program_ p p init x)
  }


type 'a pass_kind =
  [ `Cata of Catamorphism.idle_fold
  | `Hylo of 'a pass_fold * 'a pass_unfold
  | `Check of Iter.iter
  | `None
  ]

let morph
    ~name
    ~(compile : 'a pass_kind)
    ~(decompile : 'a pass_kind)
    ~(reduction_check : Ast_unified.Iter.iter)
    : pass
  =
  let mk_morph pass_kind (catapass, (cata, ana), check) value =
    match pass_kind with
    | `Cata pass -> catapass ~f:pass value
    | `Hylo (fold, unfold) -> ana ~f:unfold (cata ~f:fold value)
    | `Check pass ->
      check ~f:pass value;
      value
    | `None -> value
  in
  let mk_sub_pass
      : type v.
        (f:Catamorphism.idle_fold -> v -> v)
        * ((f:'a pass_fold -> v -> v * 'a) * (f:'a pass_unfold -> v * 'a -> v))
        * (f:Iter.iter -> v -> unit)
        -> v sub_pass
    =
   fun (cata, ana, check) ->
    { forward = mk_morph compile (cata, ana, check)
    ; forward_check = (reduction_check, fun f v -> check ~f v)
    ; backward = mk_morph decompile (cata, ana, check)
    }
  in
  let expression =
    mk_sub_pass
      ( Catamorphism.cata_expr
      , (Catamorphism.cata_expr, Anamorphism.ana_expr)
      , Iter.iter_expr )
  in
  let program =
    mk_sub_pass
      ( Catamorphism.cata_program
      , (Catamorphism.cata_program, Anamorphism.ana_program)
      , Iter.iter_program )
  in
  let pattern =
    mk_sub_pass
      ( Catamorphism.cata_pattern
      , (Catamorphism.cata_pattern, Anamorphism.ana_pattern)
      , Iter.iter_pattern )
  in
  let ty_expr =
    mk_sub_pass
      ( Catamorphism.cata_ty_expr
      , (Catamorphism.cata_ty_expr, Anamorphism.ana_ty_expr)
      , Iter.iter_ty_expr )
  in
  let block =
    mk_sub_pass
      ( Catamorphism.cata_block
      , (Catamorphism.cata_block, Anamorphism.ana_block)
      , Iter.iter_block )
  in
  let process_name =
    (* we use __MODULE__ .. can be a bit incovenient as a name to use with CLI so we process it a bit *)
    let open Simple_utils.Function in
    String.lowercase <@ String.substr_replace_all ~pattern:"Passes__" ~with_:""
  in
  { name = process_name name; expression; program; pattern; ty_expr ; block}
