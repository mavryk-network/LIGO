(* Regions of a file *)

(* A shorthand *)

let sprintf = Printf.sprintf

(* The object type for regions *)

type file = string [@@deriving yojson]
type pos = Pos.t * Pos.t [@@deriving yojson]
type byte_pos = Lexing.position * Lexing.position

type is_ghost = bool [@@deriving yojson]

type t = <
  start : Pos.t;
  stop  : Pos.t;

  (* Setters *)

  shift_bytes     : int -> t;
  shift_one_uchar : int -> t;
  set_file        : string -> t;

  (* Getters *)

  file      : file;
  pos       : pos;
  byte_pos  : byte_pos;

  (* Predicates *)

  is_ghost : is_ghost;

  (* Conversions to [string] *)

  to_string : ?file:bool -> ?offsets:bool -> [`Byte | `Point] -> string;
  compact   : ?file:bool -> ?offsets:bool -> [`Byte | `Point] -> string
>

(* A synonym *)

type region = t


(* A convenience *)

type 'a reg = {region : t; value : 'a; }

(* Injections *)

exception Different_files of string * string
exception Out_of_order_pos of string * string

let make ~(start : Pos.t) ~(stop : Pos.t) =
  if String.(<>) start#file stop#file
  then raise (Different_files (start#file, stop#file))
  else
  if start#line > stop#line
   || start#line = stop#line && start#byte_offset > stop#byte_offset
  then
    let pos1 = start#compact `Point
    and pos2 = stop#compact ` Point
    in raise (Out_of_order_pos (pos1, pos2))
  else
    object
      val    start = start
      method start = start
      val    stop  = stop
      method stop  = stop

      method shift_bytes len =
        let start = start#shift_bytes len
        and stop  = stop#shift_bytes len
        in {< start; stop >}

      method shift_one_uchar len =
        let start = start#shift_one_uchar len
        and stop  = stop#shift_one_uchar len
        in {< start; stop >}

      method set_file name =
        let start = start#set_file name
        and stop  = stop#set_file name
        in {< start; stop >}

      (* Getters *)

      method file     = start#file
      method pos      = start, stop
      method byte_pos = start#byte, stop#byte

      (* Predicates *)

      method is_ghost = start#is_ghost && stop#is_ghost

      (* Conversions to strings *)

      method to_string ?(file=true) ?(offsets=true) mode =
        let horizontal = if offsets then "character" else "column"
        and start_offset =
          if offsets then start#offset mode else start#column mode
        and stop_offset =
          if offsets then stop#offset mode else stop#column mode in
        let info =
          if file
          then sprintf "File %S, line %i, %s"
                 (String.escaped start#file) start#line horizontal
          else sprintf "Line %i, %s" start#line horizontal
        in if stop#line = start#line
           then
             if start_offset = stop_offset
             then sprintf "%s %i" info start_offset
             else sprintf "%ss %i-%i" info start_offset stop_offset
           else sprintf "%s %i to line %i, %s %i"
                        info start_offset stop#line horizontal
                        stop_offset

      method compact ?(file=true) ?(offsets=true) mode =
        if start#is_ghost || stop#is_ghost then "ghost"
        else
          let file_opt  =
            if file then Filename.basename start#file ^ ":" else ""
          and start_str = start#compact ~file:false ~offsets mode
          and stop_str  = stop#compact ~file:false ~offsets mode in
          if String.equal start#file stop#file then
            if start#line = stop#line then
              sprintf "%s%s-%i"
                      file_opt start_str
                      (if offsets then stop#offset mode else stop#column mode)
            else sprintf "%s%s-%s" file_opt start_str stop_str
          else sprintf "%s:%s-%s:%s"
                       start#file start_str stop#file stop_str
    end

let empty pos = make ~start:pos ~stop:pos

(* Making a region from the matched prefix of a lexing buffer *)

let from_lexbuf lexbuf =
  let start = Lexing.lexeme_start_p lexbuf |> Pos.from_byte
  and stop  = Lexing.lexeme_end_p lexbuf   |> Pos.from_byte
  in make ~start ~stop

(* Special regions *)

let ghost = make ~start:Pos.ghost ~stop:Pos.ghost

let wrap_ghost value = {value ; region = ghost}

let min ~file = make ~start:(Pos.min ~file) ~stop:(Pos.min ~file)

(* Comparisons *)

let equal (r1 : t) (r2 : t) =
  String.equal r1#file r2#file
&& Pos.equal r1#start r2#start
&& Pos.equal r1#stop  r2#stop

let reg_equal eq {region=r1;value=v1} {region=r2;value=v2} =
  equal r1 r2 && eq v1 v2

let lt r1 r2 =
  String.equal r1#file r2#file
&& not r1#is_ghost
&& not r2#is_ghost
&& Pos.lt r1#start r2#start
&& Pos.lt r1#stop  r2#stop

let compare r1 r2 =
  if equal r1 r2 then 0
  else if lt r1 r2 then -1
  else 1

let cover r1 r2 =
  if r1#is_ghost
  then r2
  else if r2#is_ghost
       then r1
       else if   lt r1 r2
            then make ~start:r1#start ~stop:r2#stop
            else make ~start:r2#start ~stop:r1#stop

let to_yojson f =
  `List [ Pos.to_yojson f#start; Pos.to_yojson f#stop]

let to_human_yojson f =
  `Assoc [
      ("start", Pos.to_human_yojson f#start) ;
      ("stop",  Pos.to_human_yojson f#stop) ;
    ]

let of_yojson = fun t ->
  match t with
    `List [start; stop] ->
      (match Pos.of_yojson start, Pos.of_yojson stop with
         Ok start, Ok stop -> Ok (make ~start ~stop)
       | (Error _ as e), _ | _, (Error _ as e) -> e)
  | _ ->
     Utils.error_yojson_format "{start: Pos.t, stop: Pos.t}"

(* let to_yojson region = *)
(*   `Assoc ([ *)
(*         ("start", Pos.to_yojson region#start); *)
(*         ("stop",  Pos.to_yojson region#stop); *)
(*         ("file", Region.file_to_yojson region#file); *)
(*         ("pos", Region.pos_to_yojson region#pos); *)
(*         ("byte_pos", Region.byte_pos_to_yojson region#byte_pos); *)
(*         ("is_ghost", Region.is_ghost_to_yojson region#is_ghost); *)
(*     ]) *)


let reg_to_yojson value_to_yojson reg =
  let { region; value; } = reg in
  `Assoc ([
        ("region", to_yojson region);
        ("value", value_to_yojson value);
    ])

let reg_of_yojson value_of_yojson (yojson: Yojson.Safe.t) = match yojson with
  | `Assoc ([ ("region", region); ("value", value) ]) ->
     (match of_yojson region, value_of_yojson value with
     | Ok region, Ok value -> Ok { region; value }
     | _ ->
        Error "of_yojson failed for region or value")
  | _ ->
     Utils.error_yojson_format "{region: t, value: 'value}"
