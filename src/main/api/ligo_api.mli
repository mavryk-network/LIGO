module Mutate : sig
  val mutate_ast
    :  Compiler_options.Raw_options.t
    -> string
    -> Simple_utils.Display.ex_display_format
    -> int option
    -> no_colour:bool
    -> unit
    -> (string * string, string * string) result

  val mutate_cst
    :  Compiler_options.Raw_options.t
    -> string
    -> Simple_utils.Display.ex_display_format
    -> int option
    -> no_colour:bool
    -> unit
    -> (string * string, string * string) result
end

val dump_changelog
  :  Simple_utils.Display.ex_display_format
  -> bool
  -> unit
  -> (string * string, string * string) result

module Formatter : sig
  val declarations_ppformat
    :  display_format:string Simple_utils.Display.display_format
    -> no_colour:'a
    -> Format.formatter
    -> string * Ast_typed.expression_variable list
    -> unit

  val declarations_jsonformat
    :  string * Ast_typed.expression_variable list
    -> Yojson.Safe.t

  val declarations_format
    : (string * Ast_typed.expression_variable list) Simple_utils.Display.format

  val changelog_ppformat
    :  display_format:string Simple_utils.Display.display_format
    -> no_colour:'a
    -> Format.formatter
    -> string
    -> unit

  val changelog_jsonformat : string -> Yojson.Safe.t
  val changelog_format : string Simple_utils.Display.format

  val contract_size_ppformat
    :  display_format:string Simple_utils.Display.display_format
    -> no_colour:'a
    -> Format.formatter
    -> int
    -> unit

  val contract_size_jsonformat : int -> Yojson.Safe.t
  val contract_size_format : int Simple_utils.Display.format

  val list_ppformat
    :  display_format:string Simple_utils.Display.display_format
    -> no_colour:'a
    -> Format.formatter
    -> string list
    -> unit

  val list_jsonformat : string list -> Yojson.Safe.t
  val list_format : string list Simple_utils.Display.format

  val new_project_ppformat
    :  display_format:string Simple_utils.Display.display_format
    -> no_colour:'a
    -> Format.formatter
    -> string list
    -> unit

  val new_project_jsonformat : string list -> Yojson.Safe.t
  val new_project_format : string list Simple_utils.Display.format

  module Michelson_formatter : sig
    val pp_hex
      :  Format.formatter
      -> 'a Tezos_client_015_PtLimaPt.Michelson_v1_macros.node
      -> unit

    type michelson_format =
      [ `Hex
      | `Json
      | `Text
      ]

    type michelson_comments =
      { location : bool
      ; source : bool
      ; env : bool
      }

    type shrunk_variable_meta =
      { location : Simple_utils.Location.t
      ; name : string option
      ; source_type : int option
      }

    type shrunk_meta =
      { location : Simple_utils.Location.t
      ; env : shrunk_variable_meta option list
      ; source_type : int option
      }

    type shrunk_result =
      { types : Ast_typed.type_expression list
      ; michelson : (shrunk_meta, string) Tezos_micheline.Micheline.node
      }

    val comment : michelson_comments -> shrunk_meta -> string option
    val yojson_to_json : Yojson.Safe.t -> Data_encoding.json
    val location_to_json : Simple_utils.Location.t -> Data_encoding.json option
    val source_type_to_json : Ast_typed.type_expression -> Data_encoding.json
    val json_object : (string * Data_encoding.json option) list -> Data_encoding.json

    module TypeOrd : sig
      type t = Ast_typed.type_expression

      val compare : Ast_typed.type_expression -> Ast_typed.type_expression -> int
    end

    module TypeSet : sig
      type elt = TypeOrd.t
      type t = Stdlib.Set.Make(TypeOrd).t

      val empty : t
      val is_empty : t -> bool
      val mem : elt -> t -> bool
      val add : elt -> t -> t
      val singleton : elt -> t
      val remove : elt -> t -> t
      val union : t -> t -> t
      val inter : t -> t -> t
      val disjoint : t -> t -> bool
      val diff : t -> t -> t
      val compare : t -> t -> int
      val equal : t -> t -> bool
      val subset : t -> t -> bool
      val iter : (elt -> unit) -> t -> unit
      val map : (elt -> elt) -> t -> t
      val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
      val for_all : (elt -> bool) -> t -> bool
      val exists : (elt -> bool) -> t -> bool
      val filter : (elt -> bool) -> t -> t
      val filter_map : (elt -> elt option) -> t -> t
      val partition : (elt -> bool) -> t -> t * t
      val cardinal : t -> int
      val elements : t -> elt list
      val min_elt : t -> elt
      val min_elt_opt : t -> elt option
      val max_elt : t -> elt
      val max_elt_opt : t -> elt option
      val choose : t -> elt
      val choose_opt : t -> elt option
      val split : elt -> t -> t * bool * t
      val find : elt -> t -> elt
      val find_opt : elt -> t -> elt option
      val find_first : (elt -> bool) -> t -> elt
      val find_first_opt : (elt -> bool) -> t -> elt option
      val find_last : (elt -> bool) -> t -> elt
      val find_last_opt : (elt -> bool) -> t -> elt option
      val of_list : elt list -> t
      val to_seq_from : elt -> t -> elt Seq.t
      val to_seq : t -> elt Seq.t
      val to_rev_seq : t -> elt Seq.t
      val add_seq : elt Seq.t -> t -> t
      val of_seq : elt Seq.t -> t
    end

    val fold_micheline
      :  ('a, 'b) Tezos_micheline.Micheline.node
      -> f:('acc -> ('a, 'b) Tezos_micheline.Micheline.node -> 'acc)
      -> init:'acc
      -> 'acc

    val shrink : (Mini_c.meta, string) Tezos_micheline.Micheline.node -> shrunk_result
    val variable_meta_to_json : shrunk_variable_meta -> Data_encoding.json
    val meta_encoding : shrunk_meta Data_encoding.t
    val comment_encoding : michelson_comments -> shrunk_meta Data_encoding.t option
    val pp_result_json : michelson_comments -> Format.formatter -> shrunk_result -> unit

    val result_ppformat
      :  [< `Hex | `Json | `Text ]
      -> michelson_comments
      -> display_format:string Simple_utils.Display.display_format
      -> no_colour:'a
      -> Format.formatter
      -> shrunk_result
      -> unit

    val result_jsonformat
      :  [< `Hex | `Json | `Text ]
      -> michelson_comments
      -> shrunk_result
      -> Yojson.Safe.t

    val convert_michelson_comments
      :  [ `All | `Env | `Location | `Source ] list
      -> michelson_comments

    val shrunk_result_format
      :  michelson_format
      -> [ `All | `Env | `Location | `Source ] list
      -> shrunk_result Simple_utils.Display.format

    val michelson_format
      :  michelson_format
      -> [ `All | `Env | `Location | `Source ] list
      -> (Mini_c.meta, string) Tezos_micheline.Micheline.node Simple_utils.Display.format

    val michelson_constant_jsonformat
      :  [< `Hex | `Json | `Text ]
      -> ('a, string) Tezos_micheline.Micheline.node
      -> Yojson.Safe.t

    val michelson_constant_ppformat
      :  display_format:string Simple_utils.Display.display_format
      -> no_colour:'a
      -> Format.formatter
      -> Memory_proto_alpha.Protocol.Script_expr_hash.t
         * ('b, string) Tezos_micheline.Micheline.node
      -> unit

    val michelson_constant_format
      : (Memory_proto_alpha.Protocol.Script_expr_hash.t
        * (int, string) Tezos_micheline.Micheline.node)
        Simple_utils.Display.format
  end
end

module Print : sig
  val loc : Ast_aggregated.Location.t

  val pretty_print
    :  Compiler_options.Raw_options.t
    -> string
    -> Simple_utils.Display.ex_display_format
    -> unit
    -> (string * string, string * string) result

  val dependency_graph
    :  Compiler_options.Raw_options.t
    -> string
    -> Simple_utils.Display.ex_display_format
    -> unit
    -> (string * string, string * string) result

  val preprocess
    :  Compiler_options.Raw_options.t
    -> string
    -> Simple_utils.Display.ex_display_format
    -> unit
    -> (string * string, string * string) result

  val cst
    :  Compiler_options.Raw_options.t
    -> string
    -> Simple_utils.Display.ex_display_format
    -> unit
    -> (string * string, string * string) result

  val ast
    :  Compiler_options.Raw_options.t
    -> string
    -> Simple_utils.Display.ex_display_format
    -> unit
    -> (string * string, string * string) result

  val ast_core
    :  Compiler_options.Raw_options.t
    -> string
    -> Simple_utils.Display.ex_display_format
    -> unit
    -> (string * string, string * string) result

  val ast_typed
    :  Compiler_options.Raw_options.t
    -> string
    -> Simple_utils.Display.ex_display_format
    -> unit
    -> (string * string, string * string) result

  val ast_aggregated
    :  Compiler_options.Raw_options.t
    -> string
    -> Simple_utils.Display.ex_display_format
    -> unit
    -> (string * string, string * string) result

  val ast_expanded
    :  Compiler_options.Raw_options.t
    -> string
    -> Simple_utils.Display.ex_display_format
    -> bool
    -> unit
    -> (string * string, string * string) result

  val mini_c
    :  Compiler_options.Raw_options.t
    -> string
    -> Simple_utils.Display.ex_display_format
    -> string option
    -> unit
    -> (string * string, string * string) result
end

module Api_helpers : sig
  module Trace = Simple_utils.Trace

  val toplevel
    :  ?warning_as_error:bool
    -> display_format:Simple_utils.Display.ex_display_format
    -> no_colour:bool
    -> Simple_utils.Display.displayable
    -> ('value * Main_warnings.all list, 'a * Main_warnings.all list) result
    -> (string * string, string * string) result

  val format_result
    :  ?warning_as_error:bool
    -> display_format:Simple_utils.Display.ex_display_format
    -> no_colour:bool
    -> 'value Simple_utils.Display.format
    -> (raise:(Main_errors.all, Main_warnings.all) Simple_utils.Trace.raise -> 'value)
    -> (string * string, string * string) result
end

module Compile : sig
  val loc : Ast_aggregated.Location.t

  val no_comment
    :  ('a, 'b) Scoping.Micheline.node
    -> (Mini_c.meta, 'b) Scoping.Micheline.node

  val has_env_comments : [> `Env | `Location ] list -> bool

  val read_file_constants
    :  raise:
         ( [> `Main_cannot_open_global_constants of string
           | `Main_cannot_parse_global_constants of string * string
           ]
         , 'a )
         Api_helpers.Trace.raise
    -> string option
    -> string list

  module Path : sig
    type t = string
  end

  type source =
    | Text of string * Syntax_types.t
    | File of string

  val contract
    :  Compiler_options.Raw_options.t
    -> source
    -> Simple_utils.Display.ex_display_format
    -> Formatter.Michelson_formatter.michelson_format
    -> [ `All | `Env | `Location | `Source ] list
    -> unit
    -> (string * string, string * string) result

  val expression
    :  Compiler_options.Raw_options.t
    -> string
    -> string option
    -> Simple_utils.Display.ex_display_format
    -> bool
    -> Formatter.Michelson_formatter.michelson_format
    -> unit
    -> (string * string, string * string) result

  val constant
    :  Compiler_options.Raw_options.t
    -> string
    -> string option
    -> Simple_utils.Display.ex_display_format
    -> bool
    -> unit
    -> (string * string, string * string) result

  val parameter
    :  Compiler_options.Raw_options.t
    -> string
    -> string
    -> string
    -> string
    -> string option
    -> string option
    -> string option
    -> Simple_utils.Display.ex_display_format
    -> bool
    -> Formatter.Michelson_formatter.michelson_format
    -> unit
    -> (string * string, string * string) result

  val storage
    :  Compiler_options.Raw_options.t
    -> string
    -> string
    -> string
    -> string
    -> string option
    -> string option
    -> string option
    -> Simple_utils.Display.ex_display_format
    -> bool
    -> Formatter.Michelson_formatter.michelson_format
    -> unit
    -> (string * string, string * string) result
end

module Transpile : sig
  val contract
    :  string
    -> string
    -> string
    -> Simple_utils.Display.ex_display_format
    -> bool
    -> unit
    -> (string * string, string * string) result

  val expression
    :  string
    -> string
    -> string
    -> Simple_utils.Display.ex_display_format
    -> bool
    -> unit
    -> (string * string, string * string) result
end

module Info : sig
  val measure_contract
    :  Compiler_options.Raw_options.t
    -> string
    -> Simple_utils.Display.ex_display_format
    -> bool
    -> unit
    -> (string * string, string * string) result

  val list_declarations
    :  Compiler_options.Raw_options.t
    -> string
    -> Simple_utils.Display.ex_display_format
    -> bool
    -> unit
    -> (string * string, string * string) result

  val get_scope
    :  Compiler_options.Raw_options.t
    -> string
    -> Simple_utils.Display.ex_display_format
    -> bool
    -> unit
    -> (string * string, string * string) result
end

module Run : sig
  val test
    :  Compiler_options.Raw_options.t
    -> string
    -> Simple_utils.Display.ex_display_format
    -> bool
    -> unit
    -> (string * string, string * string) result

  val test_expression
    :  Compiler_options.Raw_options.t
    -> string
    -> string option
    -> Simple_utils.Display.ex_display_format
    -> bool
    -> unit
    -> (string * string, string * string) result

  val dry_run
    :  Compiler_options.Raw_options.t
    -> string
    -> string
    -> string
    -> string
    -> string
    -> string option
    -> string option
    -> string option
    -> Simple_utils.Display.ex_display_format
    -> bool
    -> unit
    -> (string * string, string * string) result

  val interpret
    :  Compiler_options.Raw_options.t
    -> string
    -> string option
    -> string
    -> string
    -> string option
    -> string option
    -> string option
    -> Simple_utils.Display.ex_display_format
    -> bool
    -> unit
    -> (string * string, string * string) result

  val evaluate_call
    :  Compiler_options.Raw_options.t
    -> string
    -> string
    -> string
    -> string
    -> string option
    -> string option
    -> string option
    -> Simple_utils.Display.ex_display_format
    -> bool
    -> unit
    -> (string * string, string * string) result

  val evaluate_expr
    :  Compiler_options.Raw_options.t
    -> string
    -> string
    -> string
    -> string option
    -> string option
    -> string option
    -> Simple_utils.Display.ex_display_format
    -> bool
    -> unit
    -> (string * string, string * string) result
end

module Ligo_init : sig
  type project_entity =
    [ `CONTRACT
    | `LIBRARY
    ]

  val list' : kind:project_entity -> string list

  val list
    :  kind:[< `CONTRACT | `LIBRARY ]
    -> display_format:Simple_utils.Display.ex_display_format
    -> no_colour:bool
    -> unit
    -> (string * string, string * string) result

  val new_project
    :  version:string
    -> kind:project_entity
    -> project_name_opt:string option
    -> template:string
    -> display_format:Simple_utils.Display.ex_display_format
    -> no_colour:bool
    -> registry:string
    -> unit
    -> (string * string, string * string) result
end