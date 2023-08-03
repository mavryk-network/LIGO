(* Injecting the virtual token ES6FUN before a '(' that looks like a
   lambda *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module Std    = Simple_utils.Std
module Utils  = Simple_utils.Utils

(* Utilities *)

let (<@) = Utils.(<@)

type tokens = Token.t list

type state = {
  prefix : tokens;  (* Tokens from the start, up to "(" excluded. *)
  level  : int;     (* Nesting of arrays as parameters. *)
  params : Token.t Utils.nseq; (* Tokens from "(" for possible parameters. *)
  region : Region.t (* Region of "(" for possible parameters. *)
}

let mk_acc state =
  let previous, rest = state.params
  in previous, rest @ state.prefix

let return state =
  List.rev (Utils.nseq_to_list state.params @ state.prefix)

let mk_fun state =
  let {prefix; params; region; _} = state in
  let previous, rest = params
  and es6fun = Token.mk_ES6FUN region
  in previous, rest @ es6fun :: prefix

let init_state acc previous current = {
  prefix   = previous :: acc;
  level    = 0;
  params   = current, [];
  region   = Token.to_region current
}

let shift token state =
  {state with params = Utils.nseq_cons token state.params}

let push token state =
  {state with params = Utils.nseq_cons token state.params;
              level = state.level + 1}

let pop token state =
  {state with params = Utils.nseq_cons token state.params;
              level = max 0 (state.level - 1)}

let inject (tokens : tokens) : tokens =
  let open Token in
  let rec aux (previous, acc) current tokens =
    match previous, current, tokens with
      (LBRACKET _ | LPAR _ | EQ _ | COMMA _ | COLON _ | GT _ | ARROW _),
      LPAR _,
      (RPAR _ as next) :: tokens (* () *)
    | Ident _ , ARROW _, next :: tokens -> (* x => *)
        let es6fun = mk_ES6FUN (to_region current) in
        aux (current, es6fun :: previous :: acc) next tokens
    | Ident _ , ARROW _, [] -> (* x => *)
        (* Syntax error, but we assume a function *)
        let es6fun = mk_ES6FUN (to_region previous) in
        List.rev (current :: es6fun :: previous :: acc)

    | (LBRACKET _ | LPAR _ | EQ _ | COMMA _ | COLON _ | GT _ | ARROW _),
      LPAR _,
      (LBRACKET _ as next) :: tokens -> (* ([ *)
        let state = init_state acc previous current
        in scan_parameters (push next state) tokens

    | (LBRACKET _ | LPAR _ | EQ _ | COMMA _ | COLON _ | GT _ | ARROW _),
      LPAR _,
      (Ident _ | WILD _ as next) :: tokens -> (* (x  (_ *)
        let state = init_state acc previous current
        in scan_parameters (shift next state) tokens

    | _, _, next :: tokens -> (* Sliding left the 3-token window *)
       aux (current, previous :: acc) next tokens
    | _, _, [] -> List.rev (current :: previous :: acc)

  and scan_parameters state = function
    (* Likely a function *)
    RPAR _ as current :: ((COLON _ | ARROW _) :: _ as tokens) ->
      aux (mk_fun state) current tokens
  | COMMA _ as current :: tokens when state.level = 0 ->
      aux (mk_fun state) current tokens
  | COLON _ as current :: tokens ->
      aux (mk_fun state) current tokens
    (* Undetermined *)
  | LBRACKET _ as current :: tokens ->
      scan_parameters (push current state) tokens
  | RBRACKET _ as current :: tokens ->
      scan_parameters (pop current state) tokens
  | (COMMA _ | Ident _ | WILD _ as current) :: tokens ->
      scan_parameters (shift current state) tokens
    (* Likely not a function *)
  | []                -> return state
  | current :: tokens -> aux (mk_acc state) current tokens
  in
  match tokens with
    fst :: snd :: tokens -> aux (fst, []) snd tokens
  | _ -> tokens

let inject tokens = Ok (inject tokens)

(* Exported *)

let filter :
  ?print_passes:Std.t ->
  add_warning:(Main_warnings.all -> unit) ->
  Token.t list ->
  _ result =
  fun ?print_passes ~add_warning:_ tokens -> (* No warning registered *)
  let () =
    match print_passes with
      Some std ->
        Std.(add_line std.out
             "Running JsLIGO token self-pass: \
              Injecting ES6FUN tokens.")
    | None -> ()
  in inject tokens
