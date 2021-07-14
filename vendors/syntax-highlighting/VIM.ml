[@@@warning "-27"]


type 'a command = {
  group_name: string;
  value: 'a;
  contained: bool;
  contained_in: string list;
}

type keyword = string list command 

type match_ = string command

type match_group = {
  match_group_name: string;
  start:  string option;
  end_:   string option
}

type region_inside = {
  start: string option;
  end_: string option;
  contains: string list;
  match_groups: match_group list;
  next_groups: string list
}

type region = region_inside command

type syntax = 
  Keyword of keyword
| Match of match_
| Region of region

type link = {
  group_name: string;
  highlight: Textmate.highlight_name
}

type highlight = 
  Link of link

type item = 
  Syntax of syntax
| Highlight of highlight
| VIMComment of string

type t = item list

module Print = struct 
  open Format

  
  let print_list fmt prefix = function 
      [] -> ()
    | _ as l ->
      fprintf fmt prefix;
      pp_print_list ~pp_sep:(fun fmt a -> fprintf fmt ",") (fun fmt a -> fprintf fmt "%s" a) fmt l;
      fprintf fmt " "

  let rec print_syntax fmt = function
    Keyword {group_name; value=keywords; contained; contained_in} -> 
      fprintf fmt "syntax keyword %s %s " group_name (String.concat " " keywords);
      if contained then 
        fprintf fmt "contained ";
      print_list fmt "containedin=" contained_in;
      fprintf fmt "\n"
  | Match {group_name; value=regexp; contained; contained_in} ->
      fprintf fmt "syntax match %s \"%s\"" group_name regexp;
      if contained then 
        fprintf fmt "contained ";
      print_list fmt "containedin=" contained_in;
      fprintf fmt "\n"
  | Region {group_name; value={start; end_; contains; match_groups; next_groups}; contained; contained_in; } ->
      fprintf fmt "syntax region %s " group_name;
      (match start with 
        Some start -> fprintf fmt "start=%s " start;
      | None -> ());
      (match end_ with 
        Some end_ -> fprintf fmt "end=%s " end_;
      | None -> ());
      if contained then 
        fprintf fmt "contained ";
      print_list fmt "containedin=" contained_in;
      print_list fmt "contains=" contains;
      print_list fmt "next_group=" next_groups;
      fprintf fmt "\n"

    and print_match_groups fmt = function 
      {match_group_name; start; end_} :: rest -> 
        fprintf fmt "matchgroup=%s " match_group_name;
        (match start with 
          Some s -> fprintf fmt "start=%s " s
        | None -> ());
        (match end_ with 
          Some s -> fprintf fmt "end=%s " s
        | None -> ());
        print_match_groups fmt rest
    | [] -> ()

    let highlight_to_string = function
      Textmate.Comment -> "Comment"
    | Constant         -> "Constant"
    | String           -> "String"
    | Character        -> "Character"
    | Number           -> "Number"
    | Boolean          -> "Boolean"
    | Float            -> "Float"
    | Identifier       -> "Identifier"
    | Builtin_function
    | Function         -> "Function"
    | Statement        -> "Statement"
    | Conditional      -> "Conditional"
    | Repeat           -> "Repeat"
    | Label            -> "Label"
    | Operator         -> "Operator"
    | Keyword          -> "Keyword"
    | Exception        -> "Exception"
    | PreProc          -> "PreProc"
    | Builtin_type
    | Type             -> "Type"
    | StorageClass     -> "StorageClass"
    | Builtin_module
    | Structure        -> "Structure"
    | Typedef          -> "Typedef"
    | SpecialChar      -> "SpecialChar"
    | SpecialComment   -> "SpecialComment"
    | Underlined       -> "Underlined"
    | Error            -> "Error"
    | Todo             -> "Todo"

    let print_highlight fmt = function
      Link {group_name; highlight} ->
        fprintf fmt "highlight link %s %s \n" group_name (highlight_to_string highlight)

end

module Convert = struct 

  let textmate_regexp_to_vim_regexp = fun r -> 
    None

  let rec pattern_to_vim: Textmate.pattern -> item list = function
    {name; kind = Begin_end {meta_name=highlight; begin_; end_; patterns}} ->
      let start_name = name in
      let (name, before) : (string * item list) = List.fold_left (fun (name, before) c -> 
        let regexp, highlight = c in
        let next_name = name ^ "_" in
        (next_name,
          [Syntax (Region {
            group_name = name;
            value = {
              start = Some regexp;
              end_ = None;
              contains = patterns;
              match_groups = [];
              next_groups = [next_name]
            };
            contained = false;
            contained_in = [];
          })] 
          @ 
          (match highlight with 
          Some highlight ->
            [Highlight (Link {group_name = name; highlight})]
          | None -> [])
          @
         before)
      ) (name, []) begin_ in
      let curr = [
        Syntax (Region {
          group_name = name;
          value      = {
            start = textmate_regexp_to_vim_regexp begin_;
            end_ =  textmate_regexp_to_vim_regexp end_;
            contains = patterns;
            match_groups = [];
            next_groups = match end_ with | [] -> [] | _ -> [name ^ "_"]
          };
          contained  = false;
          contained_in = []
        })]
        @ (match highlight with 
          Some highlight ->
            [Highlight (Link {group_name = name; highlight})]
        | None -> [])
       in
      let (_, after) : (string * item list) = List.fold_left (fun (name, after) c -> 
        let regexp, highlight = c in
        let next_name = name ^ "_" in
        (next_name,
          [Syntax (Region {
            group_name = name;
            value = {
              start = Some regexp;
              end_ = None;
              contains = [];
              match_groups = [];
              next_groups = match after with [] -> [] | _ -> [next_name]
            };
            contained = false;
            contained_in = [];
          })] 
          @ 
          (match highlight with 
          Some highlight ->
            [Highlight (Link {group_name = name; highlight})]
          | None -> [])
          @
         before)
      ) (name ^ "_", []) end_
      in
      [VIMComment start_name]
      @
      before
      @
      curr 
      @ 
      after
      
  | {name; kind = Match {match_; match_name; captures}} -> 
    [VIMComment name;
    Syntax (Match {
      group_name = name;
      value = match_;
      contained = false;
      contained_in = [];
    })]
    @
    (match match_name with 
      Some highlight -> [Highlight (Link {group_name = name; highlight})]
    | None -> [])
  | {name; kind = Patterns patterns} ->
    let start_name = name in
    let (name, children) = List.fold_left (fun (name, a) c -> (name ^ "_", pattern_to_vim ({name = name ^ "_" ; kind = c}) :: a)) (name, []) patterns in
    [VIMComment start_name;
     Syntax (Region {
       group_name = start_name;
       value = {
        start = None;
        end_ = None;
        contains = List.fold_left (fun a c -> 
          (match a with 
            [] -> name ^ "_"
          | hd :: _ -> hd ^ "_") :: a
          ) [] children;
        match_groups = [];
        next_groups = [];
       };
       contained = false;
       contained_in = []
     })
    ]
    
  | {name; kind = Reference r} -> 
    [
      VIMComment name;
      Syntax (Region {
        group_name = name;
        value = {
          start = None;
          end_ = None;
          contains = [];
          match_groups = [];
          next_groups = [r]
        };
        contained = false;
        contained_in = []
      })
    ]
end
