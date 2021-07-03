open Result

let (let*) = Result.bind

type t = {
  syntax_name:                string;
  file_types:                 string list;
  scope_name:                 string;
  folding_start_marker:       regexp option;
  folding_stop_marker:        regexp option;
  syntax_patterns:            pattern_kind list;
  repository:                 pattern list;
}

and error = 
  Referenced_rule_does_not_exist of string
| Not_a_valid_reference of string
| Meta_name_some_but_empty of string
| Begin_cant_be_empty of string
| End_cant_be_empty of string

and pattern = {
  name: string;
  kind: pattern_kind;
}

and regexp = string

and pattern_kind = 
  Begin_end of begin_end_pattern
| Match     of match_pattern
| Patterns  of patterns 
| Reference of string

and patterns = {
  p_name: string option;
  p_kind: pattern_kind list
}

and begin_end_pattern = {
  meta_name:      string option;
  begin_:         regexp;
  begin_captures: (int * string) list;
  end_:           regexp;
  end_captures:   (int * string) list;
  patterns:       pattern_kind list;
}

and match_pattern = {
  match_:   regexp;
  match_name: string option;
  captures: (int * string) list
}

module JSON = struct 
  


  let rec capture i = 
    (string_of_int (fst i), `Assoc [("name", `String (snd i))])

  and captures l = 
    `Assoc (List.map capture l)

  and pattern_kind = function 
    Begin_end { 
      meta_name;
      begin_; 
      begin_captures; 
      end_; 
      end_captures; 
      patterns
    } -> 
      (match meta_name with 
        Some s -> [("name", `String s)];
      | None -> [])
      @
      [
        ("begin", `String begin_);
        ("end", `String end_);
        ("beginCaptures", captures begin_captures);
        ("endCaptures", captures end_captures);
        ("patterns", `List (List.map (fun a -> `Assoc (pattern_kind a)) patterns))
      ]
  | Match {match_; captures=c; match_name} -> 
    (match match_name with 
      Some s -> [("name", `String s)]
    | None -> [])
    @
    [
      ("match", `String match_);      
      ("captures", captures c)
    ] 
  | Patterns {p_name; p_kind} ->
    (match p_name with 
      Some s -> [("name", `String s)]
    | None -> [])
    @
    [
      ("patterns", `List (List.map (fun a -> `Assoc (pattern_kind a)) p_kind))
    ]
  | Reference s -> 
      [("include", `String s)]

  and pattern {name; kind} = 
    `Assoc ([
      ("name", `String name);
    ] @ pattern_kind kind)

  and repository r : Yojson.Safe.t = 
    `Assoc (List.map (fun i -> (i.name, `Assoc (pattern_kind i.kind))) r)
    
  and to_yojson: t -> (Yojson.Safe.t, error) result = fun s -> 
    ok @@ `Assoc
    ((match s.folding_start_marker, s.folding_stop_marker with 
        Some folding_start_marker, Some folding_stop_marker -> [
          ("foldingStartMarker", `String folding_start_marker);
          ("foldingStopMarker", `String folding_stop_marker)
        ]
      | _, _ -> [])
    @
      [
      ("name", `String s.syntax_name);
      ("scopeName", `String s.scope_name);
      ("fileTypes", `List (List.map (fun s -> `String s) s.file_types));
      ("patterns", `List (List.map (fun a -> `Assoc (pattern_kind a)) s.syntax_patterns));
      ("repository", repository s.repository)
    ])

end

module Validate = struct
  
  let rec check_reference repository r =
    if r = "$self" then 
      ok true
    else if String.length r > 1 && r.[0] = '#' then
      let repository_item = String.sub r 1 (String.length r - 1) in
      let exists = List.exists (fun i -> i.name = repository_item) repository in 
      if exists then 
        ok true
      else 
        error (Referenced_rule_does_not_exist r)
    else 
      error (Not_a_valid_reference r)
        
  and pattern_kind name repository = function 
    Begin_end {meta_name; begin_; end_; patterns; _} -> 
      let* _ = match meta_name with 
        Some s when String.trim s = "" -> error (Meta_name_some_but_empty name)
      | _ -> ok true
      in
      if String.trim begin_ = "" then 
        error (Begin_cant_be_empty name)
      else if String.trim end_ = "" then
        error (End_cant_be_empty name)
      else
        let patterns = List.fold_left (fun a c -> if is_error a then a else pattern_kind name repository c) (ok true) patterns in
        fold ~ok ~error patterns
  | Match _m -> ok true  
  | Patterns patterns -> 
    let patterns = List.fold_left (fun a c -> if is_error a then a else pattern_kind name repository c) (ok true) patterns.p_kind in
    fold ~ok ~error patterns
  | Reference s -> check_reference repository s


  let syntax s = 
    let repository = s.repository in
    let patterns = List.fold_left (fun a c -> if is_error a then a else pattern_kind c.name repository c.kind) (ok true) repository in
    let curr = fold ~ok ~error patterns in
    let patterns = List.fold_left (fun a c -> if is_error a then a else pattern_kind "@syntax_patterns" repository c) curr s.syntax_patterns in
    fold ~ok ~error patterns

  (* let check: t -> (bool, validation_error) result  *)
end

let to_json: t -> (Yojson.Safe.t, error) result = fun s ->
  let* _ = Validate.syntax s in
  JSON.to_yojson s

module Helpers = struct 
  let macro syntax = {
    name = "macro";
    kind = Begin_end {
      meta_name = Some ("string.quoted.double." ^ syntax);
      begin_ = "^\\s*((#)\\w+)";
      begin_captures = [
        (1, "meta.preprocessor." ^ syntax);
        (2, "punctuation.definition.directive." ^ syntax)
      ];
      end_ = "$";
      end_captures = [];
      patterns = [
        Reference "#string";
        Reference "#comment"
      ]
    }
  }
  let string syntax = {
      name = "string";
      kind = Begin_end {
        meta_name = Some ("string.quoted.double." ^ syntax);
        begin_ = "\"";
        begin_captures = [];
        end_ = "\"";
        end_captures = [];
        patterns = [
          Match {
            match_ = "\\\\.";
            match_name = Some ("constant.character.escape." ^ syntax);
            captures = []
          }
        ]
      }
    }

    let ocaml_comment syntax = {
      name = "comment";
      kind = Patterns {
        p_name = None;
        p_kind = [
          Match {
            match_name = Some ("comment.line.double-slash." ^ syntax);
            match_ = "(//.*)";
            captures = []
          };
          Begin_end {
            meta_name = Some ("comment.block." ^ syntax);
            begin_ = "\\(\\*";
            begin_captures = [];
            end_ = "\\*\\)";
            end_captures = [];
            patterns = []
          }
        ]
      }
    }

    let c_comment syntax = {
      name = "comment";
      kind = Patterns {
        p_name = None;
        p_kind = [
          Match {
            match_name = Some ("comment.line.double-slash." ^ syntax);
            match_ = "(//.*)";
            captures = []
          };
          Begin_end {
            meta_name = Some ("comment.block." ^ syntax);
            begin_ = "\\/\\*";
            begin_captures = [];
            end_ = "\\*\\/";
            end_captures = [];
            patterns = []
          }
        ]
      }
    }

    let numeric_literals syntax = {
      name = "numeric-literals";
      kind = Match {
        match_name = Some ("constant.numeric." ^ syntax);
        match_ = "\\b\\d+";
        captures = []
      }
    }


  let ident_regexp = ""
  let small_ident_regexp = ""
  let capitalized_ident_regexp = ""
  let byte_regexp = ""
  let tez_regexp = ""
  let nat_regexp = ""
  let attribute_regexp = ""
  let verbatim_ocaml_regexp = ""
  let verbatim_js_regexp = ""

  let c_line_comment = "" 
  let c_block_comment = ""
  let ocaml_block_comment = ""
  let double_quoted_string = "" 
  let single_quoted_string = ""
  let preprocessor_include = ""
  
end
