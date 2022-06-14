include Fuzz_shared.Monad
open Cst.Reasonligo

module Fold_helpers(M : Monad) = struct
  open Monad_context(M)

  type 'a monad = 'a t
  let ok x = return x

  let bind_map_npseq f (hd,tl) =
    let* hd = f hd in
    let* tl = bind_map_list (fun (a,b) -> let* b = f b in ok @@ (a,b)) tl in
    ok @@ (hd,tl)
  let bind_map_pseq f = bind_map_option @@ bind_map_npseq f


  type 'a folder = {
      e : 'a -> expr -> 'a monad;
      t : 'a -> type_expr -> 'a monad ;
      d : 'a -> declaration -> 'a monad;
    }



  type mapper = {
      e : expr -> (bool * expr) monad;
      t : type_expr -> type_expr monad ;
      d : declaration -> declaration monad ;
    }

  let rec map_type_expression : mapper -> type_expr -> 'b monad = fun f t ->
    let self = map_type_expression f in
    let* t = f.t t in
    let return = ok in
    match t with
      TProd   {value;region} ->
       let* inside = bind_map_npseq self value.inside in
       let value = {value with inside} in
       return @@ TProd {value;region}
    | TSum    {value;region} ->
      let aux (e : variant reg) =
         let app (x: type_expr par reg) =
           let* inside = self x.value.inside
           in ok {x with value = {x.value with inside}} in
         let* args = bind_map_option app e.value.args in
         let value = {e.value with args}
         in ok {e with value} in
       let* variants = bind_map_npseq aux value.variants in
       let value = {value with variants} in
       return @@ TSum {value; region}
    | TRecord {value;region} ->
       let aux (element : _ reg ) =
         let* field_type = self element.value.field_type in
         let value = {element.value with field_type} in
         ok @@ {element with value }
       in
       let* ne_elements = bind_map_npseq aux value.ne_elements in
       let value = {value with ne_elements} in
       return @@ TRecord {value;region}
    | TApp    {value;region} ->
       let (const, tuple) = value in
       let* inside = bind_map_npseq self tuple.value.inside in
       let tuple = {tuple with value = {tuple.value with inside }} in
       let value = (const, tuple) in
       return @@ TApp {value;region}
    | TFun    {value;region} ->
       let (ty1, wild, ty2) = value in
       let* ty1 = self ty1 in
       let* ty2 = self ty2 in
       let value = (ty1, wild, ty2) in
       return @@ TFun {value;region}
    | TPar    {value;region} ->
       let* inside = self value.inside in
       let value = {value with inside} in
       return @@ TPar {value;region}
    | TModA {value;region} ->
       let* field = self value.field in
       let value = {value with field} in
       return @@ TModA {value;region}
    | (TVar   _
      | TArg _
      | TInt    _
      | TString _ as e )-> ok @@ e

  let rec map_expression : mapper -> expr -> expr monad = fun f e  ->
    let self_type = map_type_expression f in
    let self_module = map_module f in
    let return = ok in
    let* (b, e) = f.e e in
    let self = if b then map_expression f else ok in
    let bin_op value =
      let {op;arg1;arg2} = value in
      let* arg1 = self arg1 in
      let* arg2 = self arg2 in
      ok @@ {op;arg1;arg2}
    in
    match e with
      ECase    {value;region} ->
       let {kwd_switch=_;expr;lbrace=_;cases;rbrace=_} = value in
       let* expr = self expr in
       let* cases = matching_cases self cases in
       let value = {value with expr;cases} in
       return @@ ECase {value;region}
    | ECond    {value;region} ->
      let {kwd_if; test; ifso; ifnot} = value in
      let* test_expr' =
         match test with
            `Braces t -> self t.value.inside
         | `Parens t -> self t.value.inside in
      let test' =
         match test with
            `Braces {region; value} ->
            `Braces Region.{region; value = {value with inside = test_expr'}}
         | `Parens {region; value} ->
            `Parens Region.{region; value = {value with inside = test_expr'}} in
      let map_branch (x: _ braces reg) =
         let* inside =
            let expr, semi_opt = x.value.inside in
            let* expr' = self expr
            in ok (expr', semi_opt)
         in ok {x with value = {x.value with inside}} in
      let* ifso' = map_branch ifso in
      let* ifnot' =
         bind_map_option
            (fun (kwd_else, b) -> let* b' = map_branch b in ok (kwd_else, b'))
            ifnot in
      let value = {kwd_if; test=test'; ifso=ifso'; ifnot=ifnot'}
      in return @@ ECond {value; region}
    | EAnnot   {value;region} ->
       let expr, comma, type_expr = value in
       let* expr = self expr in
       let* type_expr = self_type type_expr in
       let value = expr, comma, type_expr in
       return @@ EAnnot {value;region}
    | ELogic BoolExpr Or  {value;region} ->
       let* value = bin_op value in
       return @@ ELogic (BoolExpr (Or {value;region}))
    | ELogic BoolExpr And {value;region} ->
       let* value = bin_op value in
       return @@ ELogic (BoolExpr (And {value;region}))
    | ELogic BoolExpr Not {value;region} ->
       let* arg = self value.arg in
       let value = {value with arg} in
       return @@ ELogic (BoolExpr (Not {value;region}))
    | ELogic CompExpr Lt    {value;region} ->
       let* value = bin_op value in
       return @@ ELogic (CompExpr (Lt {value;region}))
    | ELogic CompExpr Leq   {value;region} ->
       let* value = bin_op value in
       return @@ ELogic (CompExpr (Leq {value;region}))
    | ELogic CompExpr Gt    {value;region} ->
       let* value = bin_op value in
       return @@ ELogic (CompExpr (Gt {value;region}))
    | ELogic CompExpr Geq   {value;region} ->
       let* value = bin_op value in
       return @@ ELogic (CompExpr (Geq {value;region}))
    | ELogic CompExpr Equal {value;region} ->
       let* value = bin_op value in
       return @@ ELogic (CompExpr (Equal {value;region}))
    | ELogic CompExpr Neq   {value;region} ->
       let* value = bin_op value in
       return @@ ELogic (CompExpr (Neq {value;region}))
    | EArith Add   {value;region} ->
       let* value = bin_op value in
       return @@ EArith (Add {value;region})
    | EArith Sub   {value;region} ->
       let* value = bin_op value in
       return @@ EArith (Sub {value;region})
    | EArith Mult  {value;region} ->
       let* value = bin_op value in
       return @@ EArith (Mult {value;region})
    | EArith Div   {value;region} ->
       let* value = bin_op value in
       return @@ EArith (Div {value;region})
    | EArith Land   {value;region} ->
       let* value = bin_op value in
       return @@ EArith (Land {value;region})
    | EArith Lor   {value;region} ->
       let* value = bin_op value in
       return @@ EArith (Lor {value;region})
    | EArith Lxor   {value;region} ->
       let* value = bin_op value in
       return @@ EArith (Lxor {value;region})
    | EArith Lsl   {value;region} ->
       let* value = bin_op value in
       return @@ EArith (Lsl {value;region})
    | EArith Lsr   {value;region} ->
       let* value = bin_op value in
       return @@ EArith (Lsr {value;region})
    | EArith Mod   {value;region} ->
       let* value = bin_op value in
       return @@ EArith (Mod {value;region})
    | EArith Neg   {value;region} ->
       let* arg = self value.arg in
       let value = {value with arg} in
       return @@ EArith (Neg {value;region})
    | EArith Int   _
      | EArith Nat   _
      | EArith Mutez _ as e -> return @@ e
    | EString Cat {value;region} ->
       let* value = bin_op value in
       return @@ EString (Cat {value;region})
    | EString String   _
      | EString Verbatim _ as e -> return @@ e
    | EList ECons {value;region} ->
       let {lbracket=_;lexpr;comma=_;ellipsis=_;rexpr;rbracket=_} = value in
       let* lexpr = self lexpr in
       let* rexpr = self rexpr in
       let value = {value with lexpr;rexpr} in
       return @@ EList (ECons {value;region})
    | EList EListComp {value;region} ->
       let* elements = bind_map_pseq self value.elements in
       let value = {value with elements} in
       return @@ EList (EListComp {value;region})
    | EConstr {value;region} ->
       let const, expr = value in
       let* expr = bind_map_option self expr in
       let value = const,expr in
       return @@ EConstr {value;region}
    | ERecord  {value;region} ->
       let aux (e : field_assign reg) =
         let* field_expr = self e.value.field_expr in
         ok @@ {e with value = {e.value with field_expr}}
       in
       let* ne_elements = bind_map_npseq aux value.ne_elements in
       let value = {value with ne_elements} in
       return @@ ERecord {value;region}
    | EProj    _  as e -> return @@ e
    | EUpdate  {value;region} ->
       let aux (e : field_path_assignment reg) =
         let* field_expr = self e.value.field_expr in
         ok @@ {e with value = {e.value with field_expr}}
       in
       let* ne_elements = bind_map_npseq aux value.updates.value.ne_elements in
       let updates = {value.updates with value = {value.updates.value with ne_elements}} in
       let value = {value with updates} in
       return @@ EUpdate {value;region}
    | EModA {value;region} ->
       let* field = self value.field in
       let value = {value with field} in
       return @@ EModA {value;region}
    | EVar     _ as e -> return e
    | ECall    {value;region} ->
       let (lam, args) = value in
       let* lam = self lam in
       let* args = (match args with
                    | Unit _ as u -> ok @@ u
                    | Multiple {value;region} ->
                       let* inside = bind_map_npseq self value.inside in
                       let value = {value with inside} in
                       ok @@ Multiple {value;region}
                   ) in
       let value = (lam,args) in
       return @@ ECall {value;region}
    | EBytes   _ as e -> return @@ e
    | EUnit    _ as e -> return @@ e
    | ETuple   {value;region} ->
       let* value = bind_map_npseq self value in
       return @@ ETuple {value;region}
    | EPar     {value;region} ->
       let* inside = self value.inside in
       let value = {value with inside} in
       return @@ EPar {value;region}
    | ELetIn   {value;region} ->
       let {kwd_let=_;kwd_rec=_;binding;semi=_;body;attributes=_} = value in
       let {binders;lhs_type;eq;let_rhs} = binding in
       let* let_rhs = self let_rhs in
       let* lhs_type = bind_map_option (fun (a,b) ->
                           let* b = self_type b in ok (a,b)) lhs_type in
       let binding = {binders;lhs_type;eq;let_rhs} in
       let* body = self body in
       let value = {value with binding;body} in
       return @@ ELetIn {value;region}
    | ETypeIn  {value;region} ->
       let {type_decl;semi;body} = value in
       let {kwd_type=_;name=_;eq=_;type_expr;params=_} = type_decl in
       let* type_expr = self_type type_expr in
       let* body = self body in
       let type_decl = {type_decl with type_expr} in
       let value = {type_decl;semi;body} in
       return @@ ETypeIn {value;region}
    | EModIn  {value;region} ->
       let {mod_decl;semi;body} = value in
       let {kwd_module=_;name=_;eq=_;lbrace=_;module_;rbrace=_} = mod_decl in
       let* module_ = self_module module_ in
       let* body = self body in
       let mod_decl = {mod_decl with module_} in
       let value = {mod_decl;semi;body} in
       return @@ EModIn {value;region}
    | EModAlias  {value;region} ->
       let {mod_alias;semi;body} = value in
       let {kwd_module=_;alias=_;eq=_;binders=_} = mod_alias in
       let* body = self body in
       let value = {mod_alias;semi;body} in
       return @@ EModAlias {value;region}
    | EFun     {value;region} ->
       let {binders=_; type_params=_; lhs_type; arrow=_; body;attributes=_} = value in
       let* body = self body in
       let* lhs_type = bind_map_option (fun (a,b) ->
                           let* b = self_type b in ok (a,b)) lhs_type in
       let value = {value with body;lhs_type} in
       return @@ EFun {value;region}
    | ESeq     {value;region} ->
       let* elements = bind_map_pseq self value.elements in
       let value = {value with elements} in
       return @@ ESeq {value;region}
    | ECodeInj {value;region} ->
       let* code = self value.code in
       let value = {value with code} in
       return @@ ECodeInj {value;region}

  and matching_cases self (cases: _ Utils.nsepseq reg) =
    let* value = bind_map_npseq (case_clause self) @@ cases.value in
    ok @@ {cases with value}

  and case_clause self (case_clause: _ case_clause reg) =
    let {pattern=_;arrow=_;rhs;terminator=_} = case_clause.value in
    let* rhs = self rhs in
    let value = {case_clause.value with rhs} in
    ok @@ {case_clause with value}

  and map_declaration : mapper -> declaration -> declaration monad =
    fun f d ->
    let self_expr = map_expression f in
    let self_type = map_type_expression f in
    let self_module = map_module f in
    let return = ok in
    match d with
      ConstDecl {value;region} ->
       let (kwd_let,kwd_rec,let_binding,attr) = value in
       let {binders;lhs_type;eq;let_rhs} = let_binding in
       let* let_rhs = self_expr let_rhs in
       let* lhs_type = bind_map_option (fun (a,b) ->
                           let* b = self_type b in ok (a,b)) lhs_type in
       let let_binding = {binders;lhs_type;eq;let_rhs} in
       let value = (kwd_let,kwd_rec,let_binding,attr) in
       return @@ ConstDecl {value;region}
    | TypeDecl {value;region} ->
       let {kwd_type=_;name=_;eq=_;type_expr;params=_} = value in
       let* type_expr = self_type type_expr in
       let value = {value with type_expr} in
       return @@ TypeDecl {value;region}
    | ModuleDecl {value;region} ->
       let {kwd_module=_;name=_;eq=_;lbrace=_;module_;rbrace=_} = value in
       let* module_ = self_module module_ in
       let value = {value with module_} in
       return @@ ModuleDecl {value;region}
    | ModuleAlias {value;region} ->
       let {kwd_module=_;alias=_;eq=_;binders=_} = value in
       return @@ ModuleAlias {value;region}
    | Directive _ as d -> return d

  and map_module : mapper -> Cst.Reasonligo.t -> Cst.Reasonligo.t monad =
    fun f {decl;eof} ->
    let self = map_declaration f in
    map (fun decl -> {decl;eof}) @@
      bind_map_ne_list self @@ decl
end
