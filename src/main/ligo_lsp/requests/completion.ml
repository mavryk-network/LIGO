open Handler
module Req_hover = Hover
open Lsp_helpers
module SMap = Map.Make (String)

type module_path = string list

(** The context of some completion. The constructors of this data type should be
    ordered so that the first one has the highest priority and the last one has
    the lowest. *)
type completion_context =
  | File
  | Record_field
  | Module_field
  | Scope of module_path
  | Keyword

(** Obtain the [CompletionItem.t.sortText] for some completion item given its
    context. *)
let completion_context_priority
    ?(type_aware : bool = false)
    ?(same_file : bool = false)
    (ctx : completion_context)
    : string
  =
  let base =
    match ctx with
    | File -> 0
    | Record_field -> 1
    | Module_field -> 2
    | Scope _ -> 3
    | Keyword -> 4
  in
  let scores = [| type_aware; same_file |] in
  let score = Array.sum (module Int) ~f:Bool.to_int scores in
  let max_score = Array.length scores + 1 in
  (* The LSP specification accepts [sortText] as a string that will be used to
     sort completion items, which is sorted lexicographically. If two items have
     the same [sortText]s, then their [label]s are used to compare next.
       The idea is to allocate strings [\x00], [\x01], [\x02], ... to sort these
     items according to their context. However, we may also want to have other
     factors, such as type-aware completion (even if it's currently not
     implemented). or priority to items defined in the same file, meaning that
     some items may come first.
       First we allocate the numbers 0, [max_score]*1, [max_score]*2, ... to
     represent some priority. Now, we subtract the score for this completion in
     order for it to appear higher in the completion list.
       Invariant: 0 <= [score] && [score] <= 255. *)
  String.of_char (Char.of_int_exn ((base * max_score) - score))


module type Built_in = sig
  type t

  val keywords : (Simple_utils.Region.t -> t) SMap.t
  val symbols : (Simple_utils.Region.t -> t) SMap.t
end

let dialect_keyword_completions (module Built_in : Built_in) : CompletionItem.t list =
  List.map
    Built_in.(Map.keys keywords @ Map.keys symbols)
    ~f:(fun keyword ->
      CompletionItem.create
        ~label:keyword
        ~kind:CompletionItemKind.Keyword
        ~sortText:(completion_context_priority Keyword)
        ())


let pascaligo_keyword_completions : CompletionItem.t list =
  dialect_keyword_completions (module Lx_psc_self_tokens.Token)


let cameligo_keyword_completions : CompletionItem.t list =
  dialect_keyword_completions (module Lx_ml_self_tokens.Token)


let jsligo_keyword_completions : CompletionItem.t list =
  dialect_keyword_completions (module Lx_js_self_tokens.Token)


let get_keyword_completions : Syntax_types.t -> CompletionItem.t list = function
  | PascaLIGO -> pascaligo_keyword_completions
  | CameLIGO -> cameligo_keyword_completions
  | JsLIGO -> jsligo_keyword_completions


let defs_to_completion_items
    (context : completion_context)
    (path : Path.t)
    (syntax : Syntax_types.t)
    : Scopes.Types.def list -> CompletionItem.t list
  =
  let rec drop_common_prefix scope_path cxt_path =
    match scope_path, cxt_path with
    | mod_scope :: mods_scope, mod_cxt :: mods_cxt when String.(mod_scope = mod_cxt) ->
      drop_common_prefix mods_scope mods_cxt
    | _, _ -> scope_path
  in
  let format_label mod_path name =
    match context with
    | Scope cxt_path ->
      let path = drop_common_prefix mod_path cxt_path in
      Option.some_if (List.is_empty path) name
    | _ -> Some name
  in
  List.filter_map ~f:(fun def ->
      let label = format_label (Def.get_mod_path def) (Def.get_name def) in
      Option.map label ~f:(fun label ->
          let same_file = Option.map (Def.get_path def) ~f:(Path.equal path) in
          let sortText = completion_context_priority ?same_file context in
          let kind, detail =
            match def with
            | Scopes.Types.Variable _ ->
              CompletionItemKind.Variable, Some (Req_hover.hover_string syntax def)
            | Scopes.Types.Type _ -> CompletionItemKind.TypeParameter, None
            | Scopes.Types.Module _ -> CompletionItemKind.Module, None
          in
          CompletionItem.create ~label ~kind ~sortText ?detail ()))


let get_defs_completions
    (path : Path.t)
    (syntax : Syntax_types.t)
    (cst : Dialect_cst.t)
    (pos : Position.t)
    (get_scope_info : Ligo_interface.get_scope_info)
    : CompletionItem.t list
  =
  let scope =
    List.find (Option.value ~default:[] get_scope_info.scopes) ~f:(fun (loc, _defs) ->
        match Range.of_loc loc with
        | None -> false
        | Some loc -> Range.contains_position pos loc)
  in
  let module_path =
    let open Cst_shared.Fold in
    match cst with
    | CameLIGO cst ->
      let open Cst_cameligo.Fold in
      let collect (Some_node (node, sing)) =
        match sing with
        | S_reg S_module_decl when Range.(contains_position pos (of_region node.region))
          -> Continue node.value.name
        | _ -> Skip
      in
      fold [] (Fn.flip List.cons) collect cst
    | JsLIGO cst ->
      let open Cst_jsligo.Fold in
      let collect (Some_node (node, sing)) =
        match sing with
        | S_reg S_namespace_statement
          when Range.(contains_position pos (of_region node.region)) ->
          let _, name, _, _, _ = node.value in
          Continue name
        | _ -> Skip
      in
      fold [] (Fn.flip List.cons) collect cst
    | PascaLIGO cst ->
      let open Cst_pascaligo.Fold in
      let collect (Some_node (node, sing)) =
        match sing with
        | S_reg S_module_decl when Range.(contains_position pos (of_region node.region))
          -> Continue node.value.name
        | _ -> Skip
      in
      fold [] (Fn.flip List.cons) collect cst
  in
  defs_to_completion_items
    (Scope (List.rev_map ~f:(fun name -> name#payload) module_path))
    path
    syntax
    (* TODO: In case we found [None], let's at least show the entire scope to
       the user so the completions aren't empty. This happens because scopes
       aren't accurate and may be missing on some ranges. As soon as scopes are
       improved, we should remove this workaround. *)
    (Option.value_map scope ~default:get_scope_info.definitions ~f:snd)


let complete_files (pos : Position.t) (code : string) (files : string list)
    : CompletionItem.t list
  =
  let regex = Str.regexp {|^#[ \t]*\(include\|import\)[ \t]*\"|} in
  (* n.b.: We may not use [List.nth_exn] because if the user happens to trigger
     a completion at the last line while "Editor: Render Final Newline" is
     enabled, it will crash the language server. *)
  let current_line =
    Option.value ~default:"" @@ List.nth (String.split_lines code) pos.line
  in
  if Str.string_match regex current_line 0
  then
    List.map files ~f:(fun file ->
        CompletionItem.create
          ~label:file
          ~kind:CompletionItemKind.File
          ~sortText:(completion_context_priority File)
          ())
  else []


let with_code (path : Path.t) (f : string -> 'a Handler.t) : 'a Handler.t =
  let@ docs = ask_docs_cache in
  match Docs_cache.find docs path with
  | None -> return None
  | Some file_data -> f file_data.code


(** Collects the absolute range of some CST node as well as the distance of that
    node to a given cursor. *)
type distance =
  { range : Range.t
  ; dist : Position.t
  }

(** Simultaneously calculate the [distance]s of the cursor to a dot lexeme as
    well as of the cursor to the last seen lexeme. *)
type completion_distance =
  { dot : distance option
  ; lexeme : distance option
  }

let smallest_negative_distance_monoid : distance option Cst_shared.Fold.monoid =
  { empty = None
  ; append =
      (fun lhs rhs ->
        match lhs, rhs with
        | None, None -> None
        | x, None | None, x -> x
        | ( (Some { range = lhs_range; dist = lhs_dist } as lhs)
          , (Some { range = rhs_range; dist = rhs_dist } as rhs) ) ->
          (* Return the least distance between the two. *)
          (match Position.compare_ord lhs_dist rhs_dist with
          | Less -> rhs
          (* The two have equal distances, pick the smallest of them, i.e., the
             one whose start is closest to the position. *)
          | Equal ->
            if Position.is_to_the_left lhs_range.start rhs_range.start then rhs else lhs
          | Greater -> lhs))
  }


let completion_distance_monoid : completion_distance Cst_shared.Fold.monoid =
  let dist_monoid = smallest_negative_distance_monoid in
  { empty = { dot = dist_monoid.empty; lexeme = dist_monoid.empty }
  ; append =
      (fun { dot = dot_lhs; lexeme = lexeme_lhs } { dot = dot_rhs; lexeme = lexeme_rhs } ->
        { dot = dist_monoid.append dot_lhs dot_rhs
        ; lexeme = dist_monoid.append lexeme_lhs lexeme_rhs
        })
  }


let first_monoid : 'a Cst_shared.Fold.monoid =
  { empty = None; append = Option.first_some }


type ('module_expr, 'module_type_expr, 'projection) expr_kind =
  | Module_path_expr of 'module_expr
  | Module_path_type_expr of 'module_type_expr
  | Projection of 'projection

(* TODO: we should handle field completion using ast_typed rather than scopes *)
let complete_fields
    (path : Path.t)
    (syntax : Syntax_types.t)
    (cst : Dialect_cst.t)
    (pos : Position.t)
    (get_scope_info : Ligo_interface.get_scope_info)
    : CompletionItem.t list
  =
  (* Recursively resolve a projection path. This is done with
     [find_record_from_path], which takes the current field's path and its type.
     We look into the record's known fields and check for the presence of the
     current field. If resolved, the algorithm recursively looks up this field's
     type and proceeds to try to resolve it as a record with the remainder of
     the field path. Once there is no more fields, we return the current record. *)
  let core_record_to_completion_items (row : Ast_core.row) : CompletionItem.t list =
    List.map (Map.to_alist row.fields) ~f:(fun (Label label, texp) ->
        let () = Ast_core.PP.type_expression Format.str_formatter texp in
        let detail = Format.flush_str_formatter () in
        let sortText = completion_context_priority Record_field in
        CompletionItem.create ~label ~kind:CompletionItemKind.Field ~detail ~sortText ())
  in
  let rec find_record_in_core : Ast_core.type_content -> Ast_core.row option = function
    | T_record row -> Some row
    | T_variable var ->
      Option.bind
        (List.find_map get_scope_info.definitions ~f:(function
            | Type tdef ->
              if Ligo_prim.Type_var.is_name var tdef.name then Some tdef else None
            | Variable _ | Module _ -> None))
        ~f:(fun tdef -> find_record_in_core tdef.content.type_content)
    | _ -> None
  in
  let rec find_record_from_path
      (struct_type : Ast_core.type_expression)
      (field_path : string option list)
      : Ast_core.row option
    =
    let open Option.Monad_infix in
    find_record_in_core struct_type.type_content
    >>= fun struct_type ->
    match field_path with
    | [] -> Some struct_type
    | selection :: field_path ->
      selection
      >>= fun name ->
      Ligo_prim.(Record.find_opt struct_type.fields (Label.of_string name))
      >>= Fn.flip find_record_from_path field_path
  in
  (* Calculate the least distance between [range] and [pos]. Returns [None] if
     [pos] is to the right of [range]. *)
  let distance_to_pos (range : Range.t) : Position.t option =
    match Position.compare_ord range.end_ pos with
    | Less | Equal ->
      Some
        (Position.create
           ~line:(range.end_.line - pos.line)
           ~character:(range.end_.character - pos.character))
    | Greater -> None
  in
  let get_module_from_pos (filter_def : Def.t -> bool) (module_pos : Position.t)
      : CompletionItem.t list option
    =
    let open Option.Monad_infix in
    let module_defs_to_completion_items (defs : Scopes.def list) : CompletionItem.t list =
      defs_to_completion_items Module_field path syntax
      @@ List.filter
           ~f:(fun def ->
             match Def.get_def_type def with
             | Module_field -> filter_def def
             | Local | Parameter | Global -> false)
           defs
    in
    let rec get_module_defs : Scopes.Types.mod_case -> CompletionItem.t list option
      = function
      | Def defs -> Some (module_defs_to_completion_items defs)
      | Alias (_alias, resolved) ->
        Option.bind resolved ~f:(fun resolved ->
            List.find_map get_scope_info.definitions ~f:(function
                | Variable _ | Type _ -> None
                | Module m ->
                  if String.(m.name = resolved) then get_module_defs m.mod_case else None))
    in
    Go_to_definition.get_definition module_pos path get_scope_info.definitions
    >>= function
    | Module { mod_case; _ } -> get_module_defs mod_case
    | Variable _ | Type _ -> None
  in
  let projection_impl
      (struct_pos : Position.t)
      (proj_fields_before_cursor : string option list)
      : CompletionItem.t list option
    =
    match Go_to_definition.get_definition struct_pos path get_scope_info.definitions with
    | Some (Variable { t; _ }) ->
      let mk_completions t =
        Option.map ~f:core_record_to_completion_items
        @@ find_record_from_path t proj_fields_before_cursor
      in
      (match t with
      | Core t -> mk_completions t
      | Resolved t -> mk_completions (Checking.untype_type_expression t)
      | Unresolved -> None)
    | None | Some (Type _ | Module _) -> None
  in
  let module_path_impl
      (module_names_before_cursor : Cst_shared.Tree.lexeme Lexing_shared.Wrap.t list)
      (filter_def : Scopes.def -> bool)
      : CompletionItem.t list option
    =
    Option.bind (List.last module_names_before_cursor) ~f:(fun module_name ->
        let module_pos = Position.of_pos module_name#region#start in
        get_module_from_pos filter_def module_pos)
  in
  let module_path_impl_expr
      (module_names_before_cursor : Cst_shared.Tree.lexeme Lexing_shared.Wrap.t list)
      : CompletionItem.t list option
    =
    module_path_impl module_names_before_cursor (function
        | Type _ -> false
        | Variable _ | Module _ -> true)
  in
  let module_path_impl_type_expr
      (module_names_before_cursor : Cst_shared.Tree.lexeme Lexing_shared.Wrap.t list)
      : CompletionItem.t list option
    =
    module_path_impl module_names_before_cursor (function
        | Variable _ -> false
        | Type _ | Module _ -> true)
  in
  let mk_dist region =
    let range = Range.of_region region in
    Option.map (distance_to_pos range) ~f:(fun dist -> { range; dist })
  in
  let open Cst_shared.Fold in
  (* The code handling CameLIGO and JsLIGO diverge significantly for a few
     reasons:
     * In CameLIGO, both a module path as well as a projection are given as
       lists. If you have [A.B.C.d.e.f] then you get one projection nested
       inside one module path, so [A.B.C.(d.e.f)].
     * In JsLIGO, [A.B.C.d.e.f] is given as a left-balanced tree for a
       projection and as a right-balanced tree for a module access. Furthermore,
       the module accesses are nested inside the projection. So it would become
       [(A.(B.(C.(d))).e).f]. That's very confusing!
     The solution is to "linearize" both CameLIGO and JsLIGO trees as
     [string option list]s and use the [first_monoid] to handle a module path as
     the base case for CameLIGO and the projection as the base case for JsLIGO.
       The PascaLIGO CST is very similar to CameLIGO's, so the solution is just
     copied and pasted there. The same CameLIGO considerations also hold for
     PascaLIGO. *)
  match cst with
  | CameLIGO cst ->
    let open Cst_cameligo.CST in
    let open Cst_cameligo.Fold in
    (* Find the greatest dot position that is less than or equal to the position
       of the cursor. Returns a negative number if to the left, null if inside,
       or positive otherwise. *)
    let farthest_dot_position_before_cursor, farthest_lexeme_position_before_cursor =
      let empty = completion_distance_monoid.empty in
      let collect (Some_node (node, sing)) =
        match sing with
        | S_dot -> Last { empty with dot = mk_dist node#region }
        (* If we are writing at the end of the file, we don't want to consider
           the [eof] [lexeme wrap]. *)
        | S_eof -> Stop
        | S_wrap _ -> Last { empty with lexeme = mk_dist node#region }
        | S_reg _ -> Continue { empty with lexeme = mk_dist node.region }
        | _ -> Skip
      in
      let { dot; lexeme } = fold_map completion_distance_monoid collect cst in
      Option.value_map
        (Option.both dot lexeme)
        ~default:(pos, pos)
        ~f:(fun (dot, lexeme) -> dot.range.start, lexeme.range.start)
    in
    let expr_start (expr : expr) : Position.t =
      Position.of_pos (expr_to_region expr)#start
    in
    (* Transform an expression such as ["A.B.C.d.e.f"] into a list containing
       names such as [[ "A" ; "B" ; "C" ; "e" ; "f" ]] such that all names are
       to the left of the cursor so we may handle the completion based on the
       last name that appears (module or field). The returned position is that
       of the start of the record or tuple being completed, in this case, ["d"],
       which will be missing from the list. *)
    let linearize_projection (node : projection) : Position.t * lexeme wrap option list =
      let hd, tl = node.field_path in
      ( expr_start node.record_or_tuple
      , List.take_while ((node.selector, hd) :: tl) ~f:(fun (dot, _field) ->
            Position.(is_to_the_left (of_pos dot#region#stop))
              farthest_dot_position_before_cursor)
        |> List.map ~f:(fun (_dot, (field : selection)) ->
               match field with
               | FieldName name -> Some name
               | Component _ -> None) )
    in
    let linearize_module_path (type expr) (node : expr module_path) : lexeme wrap list =
      List.take_while
        (Simple_utils.Utils.nsepseq_to_list node.module_path)
        ~f:(fun name ->
          Position.(is_to_the_left (of_pos name#region#stop))
            farthest_dot_position_before_cursor)
    in
    (* Returns: position of struct (if there is one), list of module names
       before the cursor, and the list of field names before the cursor ([None]
       means it's a [Component] rather than [FieldName]). *)
    let linearize_module_path_expr (node : expr module_path)
        : Position.t option * lexeme wrap list * lexeme wrap option list
      =
      let module_names_before_cursor = linearize_module_path node in
      let struct', proj_fields_before_cursor =
        match node.field with
        | E_Proj proj
          when Position.(
                 is_to_the_left
                   (of_pos proj.value.selector#region#start)
                   farthest_dot_position_before_cursor) ->
          Tuple.T2.map_fst ~f:Option.some (linearize_projection proj.value)
        | _ -> None, []
      in
      struct', module_names_before_cursor, proj_fields_before_cursor
    in
    let expr_path_impl
        (node : (expr module_path reg, type_expr module_path reg, Nothing.t) expr_kind)
        : CompletionItem.t list option
      =
      let struct', module_names_before_cursor, proj_fields_before_cursor =
        match node with
        | Module_path_expr expr -> linearize_module_path_expr expr.value
        | Module_path_type_expr type_expr ->
          None, linearize_module_path type_expr.value, []
        | Projection _ -> .
      in
      let proj_fields_before_cursor =
        List.map ~f:(Option.map ~f:(fun field -> field#payload)) proj_fields_before_cursor
      in
      (* Are we completing a module or a projection? *)
      match struct' with
      | None ->
        (match node with
        | Module_path_expr _ -> module_path_impl_expr module_names_before_cursor
        | Module_path_type_expr _ -> module_path_impl_type_expr module_names_before_cursor
        | Projection _ -> .)
      | Some struct' -> projection_impl struct' proj_fields_before_cursor
    in
    let is_reg_node_of_interest (type a) (node : a reg) : bool =
      let open Range in
      let range = of_region node.region in
      (* We only care if the region contains the last dot (the one we are trying
         to complete) as well as the last lexeme. This last part is surprising,
         but important: it's possible that there is some dot before the cursor,
         but it's not a projection we're trying to complete. Having the last
         lexeme ensures that we're not trying to compare something that is way
         back in the file. See "Complete from scope after a dot" in the
         completion tests for an example of why this is needed. *)
      contains_position farthest_dot_position_before_cursor range
      && contains_position farthest_lexeme_position_before_cursor range
    in
    let field_completion (Some_node (node, sing)) =
      match sing with
      | S_reg S_projection
      (* It's a projection, but is it the one we're trying to complete? If the
         cursor is immediately after the dot ([r.]), it may either have a
         [ghost_ident] or something the user has typed (but backtraced). To
         solve this, we check whether the token that immediately precedes the
         cursor is part of this projection. Then, we take every field whose
         selector (the dot) is before such token. *)
        when is_reg_node_of_interest node ->
        let struct_pos, proj_fields_before_cursor = linearize_projection node.value in
        let proj_fields_before_cursor =
          List.map
            ~f:(Option.map ~f:(fun field -> field#payload))
            proj_fields_before_cursor
        in
        Continue (projection_impl struct_pos proj_fields_before_cursor)
      | S_reg (S_module_path S_expr) when is_reg_node_of_interest node ->
        Continue (expr_path_impl (Module_path_expr node))
      | S_reg (S_module_path S_type_expr) when is_reg_node_of_interest node ->
        Continue (expr_path_impl (Module_path_type_expr node))
      | _ -> Skip
    in
    if Position.equal pos farthest_dot_position_before_cursor
    then []
    else Option.value ~default:[] (fold_map first_monoid field_completion cst)
  | JsLIGO cst ->
    let open! Cst_jsligo.CST in
    let open Cst_jsligo.Fold in
    let farthest_dot_position_before_cursor, farthest_lexeme_position_before_cursor =
      let empty = completion_distance_monoid.empty in
      let collect (Some_node (node, sing)) =
        match sing with
        | S_dot -> Last { empty with dot = mk_dist node#region }
        | S_eof -> Stop
        | S_wrap wrapped ->
          (* There is no way to distinguish a semicolon inserted with ASI from
             one that was inserted by the user, unfortunately, so we need this
             workaround. *)
          (match wrapped with
          | S_lexeme when String.(node#payload = ";") -> Stop
          | _ -> Last { empty with lexeme = mk_dist node#region })
        | S_reg _ -> Continue { empty with lexeme = mk_dist node.region }
        | _ -> Skip
      in
      let { dot; lexeme } = fold_map completion_distance_monoid collect cst in
      Option.value_map
        (Option.both dot lexeme)
        ~default:(pos, pos)
        ~f:(fun (dot, lexeme) -> dot.range.start, lexeme.range.start)
    in
    let rec linearize_expr : expr -> (variable, dot) Either.t list = function
      | EVar name -> [ Either.First name ]
      | EProj proj -> linearize_projection proj
      | EModA access -> linearize_module_access_expr access
      | EPar expr -> linearize_expr expr.value.inside
      (*| EObject obj -> expr.value.inside._*)
      | _ -> []
    and linearize_module_access_expr (access : expr module_access reg)
        : (variable, dot) Either.t list
      =
      Either.First access.value.module_name
      :: Either.Second access.value.selector
      :: linearize_expr access.value.field
    and linearize_projection (proj : projection reg) : (variable, dot) Either.t list =
      match proj.value.selection with
      | FieldName { value = { dot; value }; region = _ } ->
        linearize_expr proj.value.expr @ [ Either.Second dot; Either.First value ]
      | Component _ -> []
    in
    let rec linearize_type_expr : type_expr -> (variable, dot) Either.t list = function
      | TVar name -> [ Either.First name ]
      | TModA access -> linearize_module_access_type_expr access
      | TPar expr -> linearize_type_expr expr.value.inside
      (*| TObject obj -> expr.value.inside._*)
      | _ -> []
    and linearize_module_access_type_expr (access : type_expr module_access reg)
        : (variable, dot) Either.t list
      =
      Either.First access.value.module_name
      :: Either.Second access.value.selector
      :: linearize_type_expr access.value.field
    in
    let rec either_list_to_nsepseq
        : ('a, 'sep) Either.t list -> ('a, 'sep) Simple_utils.Utils.nsepseq option
      = function
      | [ Either.First name ] -> Some (name, [])
      | Either.First name :: Either.Second dot :: tl ->
        Option.map
          ~f:(Simple_utils.Utils.nsepseq_cons name dot)
          (either_list_to_nsepseq tl)
      | _ -> None
    in
    let path_expr_impl (is_type_expr : bool) (fields : (dot * lexeme wrap) list)
        : CompletionItem.t list option
      =
      let module_names_before_cursor, rest =
        List.split_while fields ~f:(fun (dot, module_name) ->
            Char.is_uppercase (String.get module_name#payload 0)
            && Position.(
                 is_to_the_left
                   (of_pos dot#region#stop)
                   farthest_dot_position_before_cursor))
      in
      let rest =
        List.take_while rest ~f:(fun (dot, _field) ->
            Position.(
              is_to_the_left (of_pos dot#region#stop) farthest_dot_position_before_cursor))
      in
      match rest with
      | (dot, struct') :: tl
        when Position.(
               is_to_the_left
                 (of_pos dot#region#start)
                 farthest_dot_position_before_cursor) ->
        let struct_pos = Position.of_pos struct'#region#start in
        let proj_fields_before_cursor =
          List.take_while tl ~f:(fun (dot, _field) ->
              Position.(
                is_to_the_left
                  (of_pos dot#region#stop)
                  farthest_dot_position_before_cursor))
          |> List.map ~f:(fun (_dot, field) -> Some field#payload)
        in
        projection_impl struct_pos proj_fields_before_cursor
      | _ ->
        (if is_type_expr then module_path_impl_type_expr else module_path_impl_expr)
        @@ List.map module_names_before_cursor ~f:snd
    in
    let complete_jsligo
        (node :
          (expr module_access reg, type_expr module_access reg, projection reg) expr_kind)
        : CompletionItem.t list option
      =
      match
        either_list_to_nsepseq
          (match node with
          | Module_path_expr module_access -> linearize_module_access_expr module_access
          | Module_path_type_expr module_access ->
            linearize_module_access_type_expr module_access
          | Projection proj -> linearize_projection proj)
      with
      | None -> None
      | Some (hd, tl) ->
        let is_type_expr =
          match node with
          | Module_path_expr _ | Projection _ -> false
          | Module_path_type_expr _ -> true
        in
        (* We always want to take the first field, otherwise the cursor would be
           before the completion, which is impossible. So this ghost is OK. *)
        path_expr_impl is_type_expr ((Lexing_shared.Wrap.ghost ".", hd) :: tl)
    in
    let is_reg_node_of_interest (type a) (node : a reg) : bool =
      let open Range in
      let range = of_region node.region in
      contains_position farthest_dot_position_before_cursor range
      && contains_position farthest_lexeme_position_before_cursor range
    in
    let field_completion (Some_node (node, sing)) =
      match sing with
      | S_reg S_projection when is_reg_node_of_interest node ->
        Continue (complete_jsligo (Projection node))
      | S_reg (S_module_access S_expr) when is_reg_node_of_interest node ->
        Continue (complete_jsligo (Module_path_expr node))
      | S_reg (S_module_access S_type_expr) when is_reg_node_of_interest node ->
        Continue (complete_jsligo (Module_path_type_expr node))
      | _ -> Skip
    in
    if Position.equal pos farthest_dot_position_before_cursor
    then []
    else Option.value ~default:[] (fold_map first_monoid field_completion cst)
  | PascaLIGO cst ->
    (* PascaLIGO's completion code is just a copy and paste of CameLIGO's
       completion code because their CSTs are very similar, unlike JsLIGO. *)
    let open Cst_pascaligo.CST in
    let open Cst_pascaligo.Fold in
    let farthest_dot_position_before_cursor, farthest_lexeme_position_before_cursor =
      let empty = completion_distance_monoid.empty in
      let collect (Some_node (node, sing)) =
        match sing with
        | S_dot -> Last { empty with dot = mk_dist node#region }
        | S_eof -> Stop
        | S_wrap _ -> Last { empty with lexeme = mk_dist node#region }
        | S_reg _ -> Continue { empty with lexeme = mk_dist node.region }
        | _ -> Skip
      in
      let { dot; lexeme } = fold_map completion_distance_monoid collect cst in
      Option.value_map
        (Option.both dot lexeme)
        ~default:(pos, pos)
        ~f:(fun (dot, lexeme) -> dot.range.start, lexeme.range.start)
    in
    let expr_start (expr : expr) : Position.t =
      Position.of_pos (expr_to_region expr)#start
    in
    let linearize_projection (node : projection) : Position.t * lexeme wrap option list =
      let hd, tl = node.field_path in
      ( expr_start node.record_or_tuple
      , List.take_while ((node.selector, hd) :: tl) ~f:(fun (dot, _field) ->
            Position.(is_to_the_left (of_pos dot#region#stop))
              farthest_dot_position_before_cursor)
        |> List.map ~f:(fun (_dot, (field : selection)) ->
               match field with
               | FieldName name -> Some name
               | Component _ -> None) )
    in
    let linearize_module_path (type expr) (node : expr module_path) : lexeme wrap list =
      List.take_while
        (Simple_utils.Utils.nsepseq_to_list node.module_path)
        ~f:(fun name ->
          Position.(is_to_the_left (of_pos name#region#stop))
            farthest_dot_position_before_cursor)
    in
    let linearize_module_path_expr (node : expr module_path)
        : Position.t option * lexeme wrap list * lexeme wrap option list
      =
      let module_names_before_cursor = linearize_module_path node in
      let struct', proj_fields_before_cursor =
        match node.field with
        | E_Proj proj
          when Position.(
                 is_to_the_left
                   (of_pos proj.value.selector#region#start)
                   farthest_dot_position_before_cursor) ->
          Tuple.T2.map_fst ~f:Option.some (linearize_projection proj.value)
        | _ -> None, []
      in
      struct', module_names_before_cursor, proj_fields_before_cursor
    in
    let expr_path_impl
        (node : (expr module_path reg, type_expr module_path reg, Nothing.t) expr_kind)
        : CompletionItem.t list option
      =
      let struct', module_names_before_cursor, proj_fields_before_cursor =
        match node with
        | Module_path_expr expr -> linearize_module_path_expr expr.value
        | Module_path_type_expr type_expr ->
          None, linearize_module_path type_expr.value, []
        | Projection _ -> .
      in
      let proj_fields_before_cursor =
        List.map ~f:(Option.map ~f:(fun field -> field#payload)) proj_fields_before_cursor
      in
      match struct' with
      | None ->
        (match node with
        | Module_path_expr _ -> module_path_impl_expr module_names_before_cursor
        | Module_path_type_expr _ -> module_path_impl_type_expr module_names_before_cursor
        | Projection _ -> .)
      | Some struct' -> projection_impl struct' proj_fields_before_cursor
    in
    let is_reg_node_of_interest (type a) (node : a reg) : bool =
      let open Range in
      let range = of_region node.region in
      contains_position farthest_dot_position_before_cursor range
      && contains_position farthest_lexeme_position_before_cursor range
    in
    let field_completion (Some_node (node, sing)) =
      match sing with
      | S_reg S_projection when is_reg_node_of_interest node ->
        let struct_pos, proj_fields_before_cursor = linearize_projection node.value in
        let proj_fields_before_cursor =
          List.map
            ~f:(Option.map ~f:(fun field -> field#payload))
            proj_fields_before_cursor
        in
        Continue (projection_impl struct_pos proj_fields_before_cursor)
      | S_reg (S_module_path S_expr) when is_reg_node_of_interest node ->
        Continue (expr_path_impl (Module_path_expr node))
      | S_reg (S_module_path S_type_expr) when is_reg_node_of_interest node ->
        Continue (expr_path_impl (Module_path_type_expr node))
      | _ -> Skip
    in
    if Position.equal pos farthest_dot_position_before_cursor
    then []
    else Option.value ~default:[] (fold_map first_monoid field_completion cst)


let mk_completion_list (items : CompletionItem.t list)
    : [ `CompletionList of CompletionList.t | `List of CompletionItem.t list ] option
  =
  Option.some @@ `CompletionList (CompletionList.create ~isIncomplete:false ~items ())


let on_req_completion (pos : Position.t) (path : Path.t)
    : [ `CompletionList of CompletionList.t | `List of CompletionItem.t list ] option
    Handler.t
  =
  when_some' (Path.get_syntax path)
  @@ fun syntax ->
  let keyword_completions = get_keyword_completions syntax in
  (* TODO (#1657): After a project system is implemented, we should support
     completing from files here. Meanwhile, we leave it with []. *)
  (*with_code path
  @@ fun _code ->
  let file_completions = complete_files pos code files in *)
  let file_completions = [] in
  let completions_without_scopes = file_completions @ keyword_completions in
  (* Even if scopes fail for whatever reason, we can at least show files and
     keywords to the user. *)
  let completions_so_far = mk_completion_list completions_without_scopes in
  with_cached_doc path completions_so_far
  @@ fun { get_scope_info; _ } ->
  with_cst path completions_so_far
  @@ fun cst ->
  let field_completions = complete_fields path syntax cst pos get_scope_info in
  let all_completions =
    (* If we are completing a record or module field, there is no need to also
       suggest scopes or keywords. *)
    if List.is_empty field_completions
    then (
      let scope_completions = get_defs_completions path syntax cst pos get_scope_info in
      let field_and_scope_completions =
        (* Keep the first item to deal with shadowing. *)
        List.remove_consecutive_duplicates
          ~which_to_keep:`First
          ~equal:(fun x y -> String.equal x.label y.label)
          (List.sort (field_completions @ scope_completions) ~compare:(fun x y ->
               String.compare x.label y.label))
      in
      field_and_scope_completions @ completions_without_scopes)
    else field_completions
  in
  return @@ mk_completion_list all_completions