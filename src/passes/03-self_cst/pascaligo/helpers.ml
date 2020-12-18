open Cst.Pascaligo
open Trace

let nseq_to_list (hd, tl) = hd :: tl

let npseq_to_list (hd, tl) = hd :: (List.map snd tl)

let npseq_to_ne_list (hd, tl) = hd, (List.map snd tl)
let bind_map_npseq f (hd,tl) =
  let%bind hd = f hd in
  let%bind tl = bind_map_list (fun (a,b) -> let%bind b = f b in ok @@ (a,b)) tl in
  ok @@ (hd,tl)
let bind_fold_npseq f init (hd,tl) =
  let%bind res = f init hd in
  let%bind res = bind_fold_list (fun init (_,b) -> f init b) res tl in
  ok @@ res

let pseq_to_list = function
  | None -> []
  | Some lst -> npseq_to_list lst
let bind_map_pseq f = bind_map_option @@ bind_map_npseq f
let bind_fold_pseq f init seq =
  let%bind res = bind_map_option (bind_fold_npseq f init) seq in
  ok @@ Option.unopt ~default:(init) res


type ('a, 'err) folder = {
  e : 'a -> expr -> ('a, 'err) result ;
  s : 'a -> statement -> ('a, 'err) result ;
  t : 'a -> type_expr -> ('a, 'err) result ;
  d : 'a -> declaration -> ('a, 'err) result ;
}

let rec fold_type_expression : ('a, 'err) folder -> 'a -> type_expr -> ('a, 'err) result = fun f init t ->
  let self = fold_type_expression f in
  let%bind init = f.t init t in
  match t with
    TProd   {value;region=_} ->
    bind_fold_ne_list self init @@ npseq_to_ne_list value
  | TSum    {value;region=_} ->
    let {lead_vbar=_;variants;attributes=_} = value in
    let aux init ({value;region=_} : _ reg) =
      let {constr=_;arg;attributes=_} = value in
      match arg with
        Some (_,t) -> self init t
      | None -> ok @@ init
    in
    bind_fold_ne_list aux init @@ npseq_to_ne_list variants
  | TRecord {value;region=_} ->
    let aux init ({value;region=_} : _ reg) =
      let {field_name=_;colon=_;field_type;attributes=_} = value in
      self init field_type
    in
    bind_fold_ne_list aux init @@ npseq_to_ne_list value.ne_elements
  | TApp    {value;region=_} ->
    let (_, tuple) = value in
    bind_fold_ne_list self init @@ npseq_to_ne_list tuple.value.inside
  | TFun    {value;region=_} ->
    let (ty1, _, ty2) = value in
    let%bind res = self init ty1 in
    let%bind res = self res  ty2 in
    ok @@ res
  | TPar    {value;region=_} ->
    self init value.inside
  | TModA {value;region=_} ->
    self init value.field
  | TVar    _
  | TWild   _
  | TInt    _
  | TString _ -> ok @@ init

let rec fold_expression : ('a, 'err) folder -> 'a -> expr -> ('a, 'err) result = fun f init e  ->
  let self = fold_expression f in
  let self_type = fold_type_expression f in
  let%bind init = f.e init e in
  let bin_op value =
    let {op=_;arg1;arg2} = value in
    let%bind res = fold_expression f init arg1 in
    let%bind res = fold_expression f res  arg2 in
    ok @@ res
  in
  match e with
    ECase    {value;region=_} ->
    let {kwd_case=_;expr;kwd_of=_;lead_vbar=_;cases} = value in
    let%bind res = self init expr in
    let%bind res = matching_cases self res cases in
    ok @@ res
  | ECond    {value;region=_} ->
    let ({kwd_if=_;test;kwd_then=_;ifso;ifnot} : cond_expr) = value in
    let%bind res = self init test in
    let%bind res = self res ifso in
    let%bind res = self res ifnot in
    ok @@ res
  | EAnnot   {value;region=_} ->
    let (expr, _, type_expr) = value.inside in
    let%bind res = self init expr in
    let%bind res = self_type res type_expr in
    ok res
  | ELogic BoolExpr Or  {value;region=_} -> bin_op value
  | ELogic BoolExpr And {value;region=_} -> bin_op value
  | ELogic BoolExpr Not {value;region=_} ->
    let {op=_;arg} = value in
    let%bind res = fold_expression f init arg in
    ok @@ res
  | ELogic BoolExpr True _ -> ok @@ init
  | ELogic BoolExpr False _ -> ok @@ init
  | ELogic CompExpr Lt    {value;region=_}
  | ELogic CompExpr Leq   {value;region=_}
  | ELogic CompExpr Gt    {value;region=_}
  | ELogic CompExpr Geq   {value;region=_}
  | ELogic CompExpr Equal {value;region=_}
  | ELogic CompExpr Neq   {value;region=_} ->
    bin_op value
  | EArith Add   {value;region=_}
  | EArith Sub   {value;region=_}
  | EArith Mult  {value;region=_}
  | EArith Div   {value;region=_}
  | EArith Mod   {value;region=_} ->
    bin_op value
  | EArith Neg   {value;region=_} ->
    let {op=_;arg} = value in
    let%bind res = fold_expression f init arg in
    ok @@ res
  | EArith Int   _
  | EArith Nat   _
  | EArith Mutez _ -> ok @@ init
  | EString Cat {value;region=_} -> bin_op value
  | EString String   _
  | EString Verbatim _ -> ok init
  | EList ECons {value;region=_} -> bin_op value
  | EList EListComp {value;region=_} ->
    bind_fold_list self init @@ pseq_to_list value.elements
  | EList ENil _ -> ok @@ init
  | EConstr NoneExpr _ -> ok @@ init
  | EConstr SomeApp {value;region=_} ->
    let _, expr = value in
    bind_fold_ne_list self init @@ npseq_to_ne_list expr.value.inside
  | EConstr ConstrApp {value;region=_} ->
    let _, expr = value in
    (match expr with
      None -> ok @@ init
    | Some e ->
      bind_fold_ne_list self init @@ npseq_to_ne_list e.value.inside
    )
  | ERecord  {value;region=_} ->
    let aux init ({value;region=_} : _ reg) =
      let {field_name=_;assignment=_;field_expr} = value in
      let%bind res = self init field_expr in
      ok res
    in
    bind_fold_ne_list aux init @@ npseq_to_ne_list value.ne_elements
  | EProj    _ -> ok @@ init
  | EUpdate  {value;region=_} ->
    let aux init ({value;region=_} : _ reg) =
      let {field_path=_;assignment=_;field_expr} = value in
      let%bind res = self init field_expr in
      ok res
    in
    bind_fold_ne_list aux init @@ npseq_to_ne_list value.updates.value.ne_elements
  | EModA    {value;region=_} -> self init value.field
  | EVar     _ -> ok init
  | ECall    {value;region=_} ->
    let (lam, args) = value in
    let%bind res = self init lam in
    bind_fold_ne_list self res @@ npseq_to_ne_list args.value.inside
  | EBytes   _ -> ok @@ init
  | EUnit    _ -> ok @@ init
  | ETuple   {value;region=_} ->
    bind_fold_ne_list self init @@ npseq_to_ne_list value.inside
  | EPar     {value;region=_} ->
    self init value.inside
  | EFun     {value;region=_} ->
    let ({kwd_function=_; param=_; ret_type; kwd_is=_; return}: fun_expr) = value in
    let%bind res = self init return in
    (match ret_type with
      Some (_, ty) -> self_type res ty
    | None ->    ok @@ res
    )
  | ECodeInj {value;region=_} ->
    let {language=_;code;rbracket=_} = value in
    self init code
  | ESet SetInj {value;region=_} ->
    bind_fold_list self init @@ pseq_to_list value.elements
  | ESet SetMem {value;region=_} ->
    let {set;kwd_contains=_;element} = value in
    let%bind res = self init set in
    let%bind res = self res element in
    ok @@ res
  | EMap MapLookUp {value;region=_} ->
    let {path=_;index} = value in
    self init @@ index.value.inside
  | EMap MapInj {value;region=_}
  | EMap BigMapInj {value;region=_} ->
    let aux init ({value;region=_}: _ reg) =
      let {source;arrow=_;image} = value in
      let%bind res = self init source in
      let%bind res = self res image in
      ok @@ res
    in
    bind_fold_list aux init @@ pseq_to_list value.elements
  | EBlock {value;region=_} ->
    let {block=b;kwd_with=_;expr} = value in
    let%bind res = fold_block f init b in
    let%bind res = self res expr in
    ok res

and fold_block f init ({value;region=_}: block reg) =
  let {enclosing=_;statements;terminator=_} = value in
  let%bind res = bind_fold_ne_list (fold_statement f) init @@ npseq_to_ne_list statements in
  ok @@ res

and fold_statement : ('a, 'err) folder -> 'a -> statement -> ('a, 'err) result = fun f init s  ->
  let self = fold_statement f in
  let self_expr = fold_expression f in
  let self_type = fold_type_expression f in
  let%bind init = f.s init s in
  let if_clause res = function
      ClauseInstr inst -> self res @@ Instr inst
    | ClauseBlock LongBlock block -> fold_block f res block
    | ClauseBlock ShortBlock {value;region=_} -> bind_fold_ne_list self res @@ npseq_to_ne_list @@ fst value.inside
  in
  let fold_selection init = function
    FieldName _ -> ok @@ init
  | Component _ -> ok @@ init
  in
  let fold_path init = function
    Name _ -> ok @@ init
  | Path {value;region=_} ->
    let {struct_name=_;selector=_;field_path} = value in
    bind_fold_ne_list fold_selection init @@ npseq_to_ne_list field_path
  in
  match s with
    Instr Cond        {value;region=_} ->
    let {kwd_if=_;test;kwd_then=_;ifso;terminator=_;kwd_else=_;ifnot} : conditional = value in
    let%bind res = self_expr init test in
    let%bind res = if_clause res ifso in
    let%bind res = if_clause res ifnot in
    ok @@ res
  | Instr CaseInstr   {value;region=_} ->
    let {kwd_case=_;expr;kwd_of=_;enclosing=_;lead_vbar=_;cases} = value in
    let%bind res = self_expr init expr in
    let%bind res = matching_cases if_clause res cases in
    ok @@ res
  | Instr Assign      {value;region=_} ->
    let {lhs; assign=_;rhs} = value in
    let fold_lhs res (lhs:lhs) = match lhs with
      Path path -> fold_path res path
    | MapPath {value;region=_} ->
      let {path;index} = value in
      let%bind res = fold_path res path in
      let%bind res = self_expr res index.value.inside in
      ok @@ res
    in
    let%bind res = fold_lhs init lhs in
    let%bind res = self_expr res rhs in
    ok @@ res
  | Instr Loop While  {value;region=_} ->
    let {kwd_while=_;cond;block} = value in
    let%bind res = self_expr init cond in
    let%bind res = fold_block f res block in
    ok @@ res
  | Instr Loop For ForInt  {value;region=_} ->
    let {kwd_for=_;binder=_;assign=_;init=i;kwd_to=_;bound;step;block} = value in
    let%bind res = self_expr init i in
    let%bind res = self_expr res bound in
    let%bind res = match step with
      Some (_,expr) -> self_expr res expr | None -> ok @@ res in
    let%bind res = fold_block f res block in
    ok @@ res
  | Instr Loop For ForCollect  {value;region=_} ->
    let {kwd_for=_;var=_;bind_to=_;kwd_in=_;collection=_;expr;block} = value in
    let%bind res = self_expr init expr in
    let%bind res = fold_block f res block in
    ok @@ res
  | Instr ProcCall    {value;region=_} ->
    let (expr, arguments) = value in
    let%bind res = self_expr init expr in
    let%bind res = bind_fold_ne_list self_expr res @@ npseq_to_ne_list arguments.value.inside in
    ok @@ res
  | Instr Skip        _ -> ok @@ init
  | Instr RecordPatch {value;region=_} ->
    let {kwd_patch=_;path;kwd_with=_;record_inj} = value in
    let%bind res = fold_path init path in
    let aux init ({value;region=_} : _ reg) =
      let {field_name=_;assignment=_;field_expr} = value in
      let%bind res = self_expr init field_expr in
      ok res
    in
    let%bind res = bind_fold_ne_list aux res @@ npseq_to_ne_list record_inj.value.ne_elements in
    ok @@ res
  | Instr MapPatch    {value;region=_} ->
    let {kwd_patch=_;path;kwd_with=_;map_inj} = value in
    let%bind res = fold_path init path in
    let aux init ({value;region=_} : _ reg) =
      let {source;arrow=_;image} = value in
      let%bind res = self_expr init source in
      let%bind res = self_expr res  image in
      ok res
    in
    let%bind res = bind_fold_ne_list aux res @@ npseq_to_ne_list map_inj.value.ne_elements in
    ok @@ res
  | Instr SetPatch    {value;region=_} ->
    let {kwd_patch=_;path;kwd_with=_;set_inj} = value in
    let%bind res = fold_path init path in
    let%bind res = bind_fold_ne_list self_expr res @@ npseq_to_ne_list set_inj.value.ne_elements in
    ok @@ res
  | Instr MapRemove   {value;region=_} ->
    let {kwd_remove=_;key;kwd_from=_;kwd_map=_;map} = value in
    let%bind res = self_expr init key in
    let%bind res = fold_path res map in
    ok @@ res
  | Instr SetRemove   {value;region=_} ->
    let {kwd_remove=_;element;kwd_from=_;kwd_set=_;set} = value in
    let%bind res = self_expr init element in
    let%bind res = fold_path res set in
    ok @@ res
  | Data LocalConst   {value;region=_} ->
    let {kwd_const=_;name=_;const_type;equal=_;init=expr;terminator=_;attributes=_} = value in
    let%bind res = self_expr init expr in
    (match const_type with
      Some (_, ty) -> self_type res ty
    | None ->    ok @@ res
    )
  | Data LocalVar     {value;region=_} ->
    let {kwd_var=_;name=_;var_type;assign=_;init=expr;terminator=_} = value in
    let%bind res = self_expr init expr in
    (match var_type with
      Some (_, ty) -> self_type res ty
    | None ->    ok @@ res
    )
  | Data LocalFun     {value;region=_} ->
    let {kwd_recursive=_;kwd_function=_;fun_name=_;param=_;ret_type;kwd_is=_;return;terminator=_;attributes=_} = value in
    let%bind res = self_expr init return in
    (match ret_type with
      Some (_, ty) -> self_type res ty
    | None ->    ok @@ res
    )
  | Type {value;region=_} ->
    let {kwd_type=_;name=_;kwd_is=_;type_expr;terminator=_} = value in
    let%bind res = self_type init type_expr in
    ok @@ res



and matching_cases : type b.('a -> b -> _) -> 'a -> (b case_clause reg, _) Utils.nsepseq reg -> _ = fun self init ({value;region=_}: _ reg) ->
  let case_clause self init ({value;region=_}: _ case_clause reg) =
    let {pattern=_;arrow=_;rhs} = value in
    self init rhs
  in
  bind_fold_ne_list (case_clause self) init @@ npseq_to_ne_list value

and fold_declaration : ('a, 'err) folder -> 'a -> declaration -> ('a, 'err) result =
  fun f init d ->
  let self_expr = fold_expression f in
  let self_type = fold_type_expression f in
  let%bind init = f.d init d in
  match d with
    ConstDecl {value;region=_} ->
    let {kwd_const=_;name=_;const_type;equal=_;init=expr;terminator=_;attributes=_} = value in
    let%bind res = self_expr init expr in
    (match const_type with
      Some (_, ty) -> self_type res ty
    | None ->    ok @@ res
    )
  | FunDecl {value;region=_} ->
    let {kwd_recursive=_;kwd_function=_;fun_name=_;param=_;ret_type;kwd_is=_;return;terminator=_;attributes=_} = value in
    let%bind res = self_expr init return in
    (match ret_type with
      Some (_, ty) -> self_type res ty
    | None ->    ok @@ res
    )
  | TypeDecl {value;region=_} ->
    let {kwd_type=_;name=_;kwd_is=_;type_expr;terminator=_} = value in
    let%bind res = self_type init type_expr in
    ok @@ res

and fold_program : ('a, 'err) folder -> 'a -> t -> ('a, 'err) result =
  fun f init {decl;eof=_} ->
  let self = fold_declaration f in
  bind_fold_ne_list self init @@ decl

type ('err) mapper = {
  e : expr -> (expr, 'err) result ;
  t : type_expr -> (type_expr, 'err) result ;
  s : statement -> (statement, 'err) result ;
  d : declaration -> (declaration, 'err) result ;
}

let rec map_type_expression : ('err) mapper -> type_expr -> ('b, 'err) result = fun f t ->
  let self = map_type_expression f in
  let%bind t = f.t t in
  let return = ok in
  match t with
    TProd   {value;region} ->
    let%bind value = bind_map_npseq self value in
    return @@ TProd {value;region}
  | TSum    {value;region} ->
    let aux (e : variant reg) =
      let%bind arg = bind_map_option (fun (a,b) -> let%bind b = self b in ok (a,b)) e.value.arg in
      let value = {e.value with arg} in
      ok @@ {e with value}
    in
    let%bind variants = bind_map_npseq aux value.variants in
    let value = {value with variants} in
    return @@ TSum {value;region}
  | TRecord {value;region} ->
    let aux (element : _ reg ) =
      let%bind field_type = self element.value.field_type in
      let value = {element.value with field_type} in
      ok @@ {element with value }
    in
    let%bind ne_elements = bind_map_npseq aux value.ne_elements in
    let value = {value with ne_elements} in
    return @@ TRecord {value;region}
  | TApp    {value;region} ->
    let (const, tuple) = value in
    let%bind inside = bind_map_npseq self tuple.value.inside in
    let tuple = {tuple with value = {tuple.value with inside }} in
    let value = (const, tuple) in
    return @@ TApp {value;region}
  | TFun    {value;region} ->
    let (ty1, wild, ty2) = value in
    let%bind ty1 = self ty1 in
    let%bind ty2 = self ty2 in
    let value = (ty1, wild, ty2) in
    return @@ TFun {value;region}
  | TPar    {value;region} ->
    let%bind inside = self value.inside in
    let value = {value with inside} in
    return @@ TPar {value;region}
  | TModA {value;region} ->
    let%bind field = self value.field in
    let value = {value with field} in
    return @@ TModA {value;region}
  | (TVar    _
  | TWild    _
  | TInt     _
  | TString _ as e )-> ok @@ e

let rec map_expression : ('err) mapper -> expr -> (expr, 'err) result = fun f e  ->
  let self = map_expression f in
  let self_type = map_type_expression f in
  let return = ok in
  let%bind e = f.e e in
  let bin_op value =
    let {op;arg1;arg2} = value in
    let%bind arg1 = self arg1 in
    let%bind arg2 = self arg2 in
    ok @@ {op;arg1;arg2}
  in
  match e with
    ECase    {value;region} ->
    let {kwd_case=_;expr;kwd_of=_;lead_vbar=_;cases} = value in
    let%bind expr = self expr in
    let%bind cases = matching_cases self cases in
    let value = {value with expr;cases} in
    return @@ ECase {value;region}
  | ECond    {value;region} ->
    let ({kwd_if=_;test;kwd_then=_;ifso;ifnot} : cond_expr) = value in
    let%bind test = self test in
    let%bind ifso = self ifso in
    let%bind ifnot = self ifnot in
    let value = {value with test;ifso;ifnot} in
    return @@ ECond {value;region}
  | EAnnot   {value;region} ->
    let expr, comma, type_expr = value.inside in
    let%bind expr = self expr in
    let%bind type_expr = self_type type_expr in
    let inside = expr, comma, type_expr in
    let value = {value with inside} in
    return @@ EAnnot {value;region}
  | ELogic BoolExpr Or  {value;region} ->
    let%bind value = bin_op value in
    return @@ ELogic (BoolExpr (Or {value;region}))
  | ELogic BoolExpr And {value;region} ->
    let%bind value = bin_op value in
    return @@ ELogic (BoolExpr (And {value;region}))
  | ELogic BoolExpr Not {value;region} ->
    let%bind arg = self value.arg in
    let value = {value with arg} in
    return @@ ELogic (BoolExpr (Not {value;region}))
  | ELogic BoolExpr True _
  | ELogic BoolExpr False _ as e -> return @@ e
  | ELogic CompExpr Lt    {value;region} ->
    let%bind value = bin_op value in
    return @@ ELogic (CompExpr (Lt {value;region}))
  | ELogic CompExpr Leq   {value;region} ->
    let%bind value = bin_op value in
    return @@ ELogic (CompExpr (Leq {value;region}))
  | ELogic CompExpr Gt    {value;region} ->
    let%bind value = bin_op value in
    return @@ ELogic (CompExpr (Gt {value;region}))
  | ELogic CompExpr Geq   {value;region} ->
    let%bind value = bin_op value in
    return @@ ELogic (CompExpr (Geq {value;region}))
  | ELogic CompExpr Equal {value;region} ->
    let%bind value = bin_op value in
    return @@ ELogic (CompExpr (Equal {value;region}))
  | ELogic CompExpr Neq   {value;region} ->
    let%bind value = bin_op value in
    return @@ ELogic (CompExpr (Neq {value;region}))
  | EArith Add   {value;region} ->
    let%bind value = bin_op value in
    return @@ EArith (Add {value;region})
  | EArith Sub   {value;region} ->
    let%bind value = bin_op value in
    return @@ EArith (Sub {value;region})
  | EArith Mult  {value;region} ->
    let%bind value = bin_op value in
    return @@ EArith (Mult {value;region})
  | EArith Div   {value;region} ->
    let%bind value = bin_op value in
    return @@ EArith (Div {value;region})
  | EArith Mod   {value;region} ->
    let%bind value = bin_op value in
    return @@ EArith (Mod {value;region})
  | EArith Neg   {value;region} ->
    let%bind arg = self value.arg in
    let value = {value with arg} in
    return @@ EArith (Neg {value;region})
  | EArith Int   _
  | EArith Nat   _
  | EArith Mutez _ as e -> return @@ e
  | EString Cat {value;region} ->
    let%bind value = bin_op value in
    return @@ EString (Cat {value;region})
  | EString String   _
  | EString Verbatim _ as e -> return @@ e
  | EList ECons {value;region} ->
    let%bind value = bin_op value in
    return @@ EList (ECons {value;region})
  | EList EListComp {value;region} ->
    let%bind elements = bind_map_pseq self value.elements in
    let value = {value with elements} in
    return @@ EList (EListComp {value;region})
  | EList ENil _ as e -> return @@ e
  | EConstr NoneExpr _ as e -> return @@ e
  | EConstr SomeApp {value;region} ->
    let some_, expr = value in
    let%bind inside = bind_map_npseq self expr.value.inside in
    let expr = {expr with value = {expr.value with inside}} in
    let value = some_,expr in
    return @@ EConstr (SomeApp {value;region})
  | EConstr ConstrApp {value;region} ->
    let const, expr = value in
    let%bind expr = bind_map_option (fun (e : tuple_expr)
      -> let%bind inside = bind_map_npseq self e.value.inside in
         ok @@ {e with value = {e.value with inside}}) expr in
    let value = const,expr in
    return @@ EConstr (ConstrApp {value;region})
  | ERecord  {value;region} ->
    let aux (e : field_assignment reg) =
      let%bind field_expr = self e.value.field_expr in
      ok @@ {e with value = {e.value with field_expr}}
    in
    let%bind ne_elements = bind_map_npseq aux value.ne_elements in
    let value = {value with ne_elements} in
    return @@ ERecord {value;region}
  | EProj    _  as e -> return @@ e
  | EUpdate  {value;region} ->
    let aux (e : field_path_assignment reg) =
      let%bind field_expr = self e.value.field_expr in
      ok @@ {e with value = {e.value with field_expr}}
    in
    let%bind ne_elements = bind_map_npseq aux value.updates.value.ne_elements in
    let updates = {value.updates with value = {value.updates.value with ne_elements}} in
    let value = {value with updates} in
    return @@ EUpdate {value;region}
  | EModA {value;region} ->
    let%bind field = self value.field in
    let value = {value with field} in
    return @@ EModA {value;region}
  | EVar     _ as e -> return e
  | ECall    {value;region} ->
    let (lam, args) = value in
    let%bind lam = self lam in
    let%bind inside = bind_map_npseq self args.value.inside in
    let args = {args with value = {args.value with inside}} in
    let value = (lam,args) in
    return @@ ECall {value;region}
  | EBytes   _ as e -> return @@ e
  | EUnit    _ as e -> return @@ e
  | ETuple   {value;region} ->
    let%bind inside = bind_map_npseq self value.inside in
    let value = {value with inside} in
    return @@ ETuple {value;region}
  | EPar     {value;region} ->
    let%bind inside = self value.inside in
    let value = {value with inside} in
    return @@ EPar {value;region}
  | EFun     {value;region} ->
    let ({kwd_function=_; param=_; ret_type; kwd_is=_; return=body}: fun_expr) = value in
    let%bind body = self body in
    let%bind ret_type = bind_map_option (fun (a,b) ->
      let%bind b = self_type b in ok (a,b)) ret_type in
    let value = {value with return=body;ret_type} in
    return @@ EFun {value;region}
  | ECodeInj {value;region} ->
    let%bind code = self value.code in
    let value = {value with code} in
    return @@ ECodeInj {value;region}
  | ESet SetInj {value;region} ->
    let%bind elements = bind_map_pseq self @@ value.elements in
    let value = {value with elements} in
    return @@ ESet (SetInj {value;region})
  | ESet SetMem {value;region} ->
    let {set;kwd_contains;element} = value in
    let%bind set = self set in
    let%bind element = self element in
    let value = {set;kwd_contains;element} in
    return @@ ESet (SetMem {value;region})
  | EMap MapLookUp {value;region} ->
    let {path;index} = value in
    let%bind inside = self @@ index.value.inside in
    let index = {index with value = {index.value with inside}} in
    let value = {path;index} in
    return @@ EMap (MapLookUp {value;region})
  | EMap MapInj {value;region} ->
    let aux (b: binding reg) =
      let {source;arrow;image} = b.value in
      let%bind source = self source in
      let%bind image  = self image in
      let value = {source;arrow;image} in
      ok @@ {b with value}
    in
    let%bind elements = bind_map_pseq aux value.elements in
    let value = {value with elements} in
    return @@ EMap (MapInj {value;region})
  | EMap BigMapInj {value;region} ->
    let aux (b: binding reg) =
      let {source;arrow;image} = b.value in
      let%bind source = self source in
      let%bind image  = self image in
      let value = {source;arrow;image} in
      ok @@ {b with value}
    in
    let%bind elements = bind_map_pseq aux value.elements in
    let value = {value with elements} in
    return @@ EMap (BigMapInj {value;region})
  | EBlock {value;region} ->
    let {block;kwd_with;expr} = value in
    let%bind expr = self expr in
    let%bind block = map_block f block in
    let value = {block;kwd_with;expr} in
    return @@ EBlock {value;region}

and map_block f (block: block reg) =
  let {enclosing=_;statements;terminator=_} = block.value in
  let%bind statements = bind_map_npseq (map_statement f) @@ statements in
  let value = {block.value with statements} in
  ok @@ {block with value}

and map_statement : 'err mapper -> statement -> (statement, 'err) result = fun f s  ->
  let self_expr = map_expression f in
  let self_inst = map_instruction f in
  let self_type = map_type_expression f in
  let%bind s = f.s s in
  match s with
  | Instr inst -> let%bind inst = self_inst inst in ok @@ Instr inst
  | Data LocalConst   {value;region} ->
    let {kwd_const=_;name=_;const_type;equal=_;init;terminator=_;attributes=_} = value in
    let%bind init = self_expr init in
    let%bind const_type = bind_map_option (fun (w, ty)
      -> let%bind ty = self_type ty in ok @@ (w,ty)) const_type in
    let value = {value with init;const_type} in
    ok @@ Data (LocalConst {value;region})
  | Data LocalVar     {value;region} ->
    let {kwd_var=_;name=_;var_type;assign=_;init;terminator=_} = value in
    let%bind var_type = bind_map_option (fun (w, ty)
      -> let%bind ty = self_type ty in ok @@ (w,ty)) var_type in
    let value = {value with init;var_type} in
    ok @@ Data (LocalVar {value;region})
  | Data LocalFun     {value;region} ->
    let {kwd_recursive=_;kwd_function=_;fun_name=_;param=_;ret_type;kwd_is=_;return;terminator=_;attributes=_} = value in
    let%bind return = self_expr return in
    let%bind ret_type = bind_map_option (fun (w, ty)
      -> let%bind ty = self_type ty in ok @@ (w,ty)) ret_type in
    let value = {value with return;ret_type} in
    ok @@ Data (LocalFun {value;region})
  | Type {value;region} ->
    let {kwd_type=_;name=_;kwd_is=_;type_expr;terminator=_} = value in
    let%bind type_expr = self_type type_expr in
    let value = {value with type_expr} in
    ok @@ Type {value;region}

and map_instruction = fun f i ->
  let self = map_instruction f in
  let self_stat = map_statement f in
  let self_expr = map_expression f in
  let if_clause = function
      ClauseInstr inst ->
      let%bind instr = self inst in
      ok @@ ClauseInstr instr
    | ClauseBlock LongBlock block -> let%bind block = map_block f block in ok @@ ClauseBlock (LongBlock block)
    | ClauseBlock ShortBlock {value;region} ->
      let (s,s_opt) = value.inside in
      let%bind s = bind_map_npseq self_stat @@ s in
      let value = {value with inside = (s,s_opt)} in
      ok @@ ClauseBlock (ShortBlock {value;region})
  in
  let map_selection = function
    FieldName _
  | Component _ as s -> ok @@ s
  in
  let map_path = function
    Name _ as n -> ok @@ n
  | Path {value;region} ->
    let {struct_name=_;selector=_;field_path} = value in
    let%bind field_path = bind_map_npseq map_selection field_path in
    let value = {value with field_path} in
    ok @@ (Path {value;region} : path)
  in
  match i with
    Cond        {value;region} ->
    let {kwd_if;test;kwd_then;ifso;terminator;kwd_else;ifnot} : conditional = value in
    let%bind test  = self_expr test in
    let%bind ifso  = if_clause ifso in
    let%bind ifnot = if_clause ifnot in
    let value : conditional = {kwd_if;test;kwd_then;ifso;terminator;kwd_else;ifnot} in
    ok @@ Cond {value;region}
  | CaseInstr   {value;region} ->
    let {kwd_case=_;expr;kwd_of=_;enclosing=_;lead_vbar=_;cases} = value in
    let%bind expr = self_expr expr in
    let%bind cases = matching_cases if_clause cases in
    let value = {value with expr;cases} in
    ok @@ CaseInstr {value;region}
  | Assign      {value;region} ->
    let {lhs; assign;rhs} = value in
    let map_lhs (lhs:lhs) : (lhs,'err) result = match lhs with
      Path path -> let%bind path = map_path path in ok @@ (Path path : lhs)
    | MapPath {value;region} ->
      let {path;index} = value in
      let%bind path = map_path path in
      let%bind inside = self_expr index.value.inside in
      let value = {path;index={index with value = {index.value with inside}}} in
      ok @@ MapPath {value;region}
    in
    let%bind lhs = map_lhs lhs in
    let%bind rhs = self_expr rhs in
    let value = {lhs;assign;rhs} in
    ok @@ Assign {value;region}
  | Loop While  {value;region} ->
    let {kwd_while;cond;block} = value in
    let%bind cond = self_expr cond in
    let%bind block = map_block f block in
    let value = {kwd_while;cond;block} in
    ok @@ Loop (While {value;region})
  | Loop For ForInt  {value;region} ->
    let {kwd_for=_;binder=_;assign=_;init;kwd_to=_;bound;step;block} = value in
    let%bind init = self_expr init in
    let%bind bound = self_expr bound in
    let%bind step = bind_map_option (fun (w,s)
      -> let%bind s = self_expr s in ok @@ (w,s)) step in
    let%bind block = map_block f block in
    let value = {value with init;bound;step;block} in
    ok @@ Loop (For (ForInt {value;region}))
  | Loop For ForCollect  {value;region} ->
    let {kwd_for=_;var=_;bind_to=_;kwd_in=_;collection=_;expr;block} = value in
    let%bind expr = self_expr expr in
    let%bind block = map_block f block in
    let value = {value with expr;block} in
    ok @@ Loop (For (ForCollect {value;region}))
  | ProcCall    {value;region} ->
    let (expr, arguments) = value in
    let%bind expr = self_expr expr in
    let%bind inside = bind_map_npseq self_expr arguments.value.inside in
    let arguments = {arguments with value = {arguments.value with inside}} in
    let value = (expr,arguments) in
    ok @@ ProcCall {value;region}
  | Skip        _ as i -> ok @@ i
  | RecordPatch {value;region} ->
    let {kwd_patch=_;path;kwd_with=_;record_inj} = value in
    let%bind path = map_path path in
    let aux ({value;region} : _ reg) =
      let {field_name=_;assignment=_;field_expr} = value in
      let%bind field_expr = self_expr field_expr in
      let value = {value with field_expr} in
      ok @@ ({value;region} : _ reg)
    in
    let%bind ne_elements = bind_map_npseq  aux @@ record_inj.value.ne_elements in
    let record_inj = {record_inj with value = {record_inj.value with ne_elements}} in
    let value = {value with path;record_inj} in
    ok @@ RecordPatch {value;region}
  | MapPatch    {value;region} ->
    let {kwd_patch=_;path;kwd_with=_;map_inj} = value in
    let%bind path = map_path path in
    let aux ({value;region} : _ reg) =
      let {source;arrow;image} = value in
      let%bind source = self_expr source in
      let%bind image = self_expr image in
      let value = {source;arrow;image} in
      ok @@ ({value;region} : _ reg)
    in
    let%bind ne_elements = bind_map_npseq aux @@ map_inj.value.ne_elements in
    let map_inj = {map_inj with value = {map_inj.value with ne_elements}} in
    let value = {value with path;map_inj} in
    ok @@ MapPatch {value;region}
  | SetPatch    {value;region} ->
    let {kwd_patch=_;path;kwd_with=_;set_inj} = value in
    let%bind path = map_path path in
    let%bind ne_elements = bind_map_npseq self_expr @@ set_inj.value.ne_elements in
    let set_inj = {set_inj with value = {set_inj.value with ne_elements}} in
    let value = {value with path;set_inj} in
    ok @@ SetPatch {value;region}
  | MapRemove   {value;region} ->
    let {kwd_remove=_;key;kwd_from=_;kwd_map=_;map} = value in
    let%bind key = self_expr key in
    let%bind map = map_path map in
    let value = {value with key;map} in
    ok @@ MapRemove {value;region}
  | SetRemove   {value;region} ->
    let {kwd_remove=_;element;kwd_from=_;kwd_set=_;set} = value in
    let%bind element = self_expr element in
    let%bind set = map_path set in
    let value = {value with element;set} in
    ok @@ SetRemove {value;region}

and matching_cases : type b. (b-> (b,_) result) -> (b case_clause reg,_) Utils.nsepseq reg -> ((b case_clause reg,_) Utils.nsepseq reg,_) result = fun self cases ->
  let case_clause self (case_clause: _ case_clause reg) =
    let {pattern=_;arrow=_;rhs} = case_clause.value in
    let%bind rhs = self rhs in
    let value = {case_clause.value with rhs} in
    ok @@ {case_clause with value}
  in
  let%bind value = bind_map_npseq (case_clause self) @@ cases.value in
  ok @@ {cases with value}


and map_declaration : ('err) mapper -> declaration -> (declaration, 'err) result =
  fun f d ->
  let self_expr = map_expression f in
  let self_type = map_type_expression f in
  let return = ok in
  let%bind d = f.d d in
  match d with
    ConstDecl {value;region} ->
    let {kwd_const=_;name=_;const_type;equal=_;init;terminator=_;attributes=_} = value in
    let%bind init = self_expr init in
    let%bind const_type = bind_map_option (fun (a,b) ->
      let%bind b = self_type b in ok (a,b)) const_type in
    let value = {value with init;const_type} in
    return @@ ConstDecl {value;region}
  | FunDecl {value;region} ->
    let {kwd_recursive=_;kwd_function=_;fun_name=_;param=_;ret_type;kwd_is=_;return=expr;terminator=_;attributes=_} = value in
    let%bind expr = self_expr expr in
    let%bind ret_type = bind_map_option (fun (a,b) ->
      let%bind b = self_type b in ok (a,b)) ret_type in
    let value = {value with return=expr;ret_type} in
    return @@ FunDecl {value;region}
  | TypeDecl {value;region} ->
    let {kwd_type=_;name=_;kwd_is=_;type_expr;terminator=_} = value in
    let%bind type_expr = self_type type_expr in
    let value = {value with type_expr} in
    return @@ TypeDecl {value;region}

and map_program : ('err) mapper -> t -> (t, 'err) result =
  fun f {decl;eof} ->
  let self = map_declaration f in
  map (fun decl -> {decl;eof}) @@
  bind_map_ne_list self @@ decl

(* TODO this is stupid *)
let fold_to_map : unit -> (unit, 'err) folder -> ('err) mapper =
  fun init {e;t;s;d} ->
  let e expr =
    let%bind () = e init expr in ok @@ expr
  in
  let t ty =
    let%bind () = t init ty in ok @@ ty
  in
  let s stat =
    let%bind () = s init stat in ok @@ stat
  in
  let d decl =
    let%bind () = d init decl in ok @@ decl
  in
  {e;t;s;d}
