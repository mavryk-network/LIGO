(* Injecting the virtual token ES6FUN before a '(' that looks like a
   lambda *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module Std    = Simple_utils.Std

(* Utilities *)

type tokens = Token.t list

(* Filter with Injection *)

let inject (tokens : tokens) : tokens =
  let open Token in
  let rec aux acc previous current next =
    match previous, current, next with
      _, _, [] -> List.rev (current :: previous :: acc)
    | (LPAR _ | EQ _ | COMMA _ | COLON _),
      LPAR _,
      (RPAR _ | LBRACKET _ | WILD _ | Ident _ as next) :: tokens ->
        let lambda = mk_ES6FUN (to_region current) in
        aux (lambda :: previous :: acc) current next tokens
    | _, _, next :: tokens -> (* Shift *)
        aux (previous :: acc) current next tokens
  in match tokens with
       fst :: snd :: tokens -> aux [] fst snd tokens
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
