module Textmate = SyntaxHighlighting.Textmate
module Helpers = Textmate.Helpers

let (let*) = Result.bind

module Name = struct
  let macro               = "macro"
  let type_declaration    = "type-decl"
  let let_declaration     = "let-decl"
  let comment             = "comment"
  let let_name            = "let-name"
  let expr                = "expr"
  let type_annotation     = "type_annotation"
  let pattern             = "pattern"
  let operators           = "operators"
  let record_expression   = "record_expr"
  let tuple_record_name   = "tuple_record_name"
  let tuple_arg_annot_type = "tuple_arg_annot_type"
  let if_or_switch_block   = "if-or-switch-block"
  let constructor          = "constructor"
  let string               = "string"
  let builtin_modules      = "builtin-modules"
  let type_identifier      = "type-identifier"
  let type_decl_identifier = "type-decl-identifier"
  let builtin_types        = "builtin-types"
  let pattern_record_item  = "pattern-record-item"
  let pattern_record       = "pattern-record"
  let builtin_big_map      = "builtin-big-map"
  let builtin_bitwise      = "builtin-bitwise"
  let builtin_bytes        = "builtin-bytes"
  let builtin_crypto       = "builtin-crypto"
  let builtin_list         = "builtin-list"
  let builtin_map          = "builtin-map"
  let builtin_set          = "builtin-set"
  let builtin_string       = "builtin-string"
  let builtin_tezos        = "builtin-tezos"
  let builtin_test         = "builtin-test"
  let builtin_toplevel     = "builtin-toplevel"
  let pattern_par          = "pattern-par"
  let pattern_sum          = "pattern-sum"
end

let syntax_highlighting () = 
  let* json = Textmate.to_json "religo" {
    syntax_name = "ReasonLIGO";
    scope_name = "source.religo";
    file_types = [
      "religo";
      "rligo"
    ];
    folding_start_marker = Some "{";
    folding_stop_marker = Some "}";
    syntax_patterns = [
      Reference Name.macro;
      Reference Name.type_declaration;
      Reference Name.let_declaration;
      Reference Name.comment
    ];
    repository = [
      Helpers.macro;
      {
        name = Name.let_declaration;
        kind = Begin_end {
          meta_name = None;
          begin_ = [("\\b(let)\\b", Some Function)];
          end_ = [("(?=let|type|\\[@|\\/\\*|\\/\\/)", None)];
          patterns = [
            Name.let_name;
            Name.expr;
            Name.comment;
          ]
        }
      };
      {
        name = Name.let_name;
        kind = Begin_end {
          meta_name = None;
          begin_ = [
            ("\\G[ ]*(\\b(rec)\\b\\s\\b)?", Some StorageClass);
            ("([a-z_][A-Za-z0-9_]*)\\b", Some Identifier)
          ];          
          end_ = [("(\\=)", Some Operator)];
          patterns = [
            Name.type_annotation;
            Name.comment
          ]
        };
      };
      {
        name = Name.type_annotation;
        kind = Begin_end {
          meta_name = None;
          begin_ = [("\\G[ ]*(\\:)", None)];
          end_ = [("(?=\\=)", None)];
          patterns = [
            Name.pattern;
            Name.comment
          ]
        }
      };
      {
        name = Name.operators;
        kind = Match {
          match_name = Some Operator;
          match_ = "\\b(mod|ediv)\\b|(\\+|\\-|\\*|\\/|==|\\|\\||\\&\\&)";
          captures = [];
        }
      }] 
      @
      Helpers.string
      @ 
      [
      {
        name = Name.record_expression;
        kind = Begin_end {
          meta_name = None;
          begin_ = [("(?<=\\=)\\s*\\{", None)];
          end_ = [("\\}", None)];
          patterns = [
            Name.tuple_record_name;
            Name.expr
          ]
        }
      };
      {
        name = Name.tuple_record_name;
        kind = Begin_end {
          meta_name = None;
          begin_ = [("(?<=\\(|,|\\{)\\s*([a-z][A-Za-z0-9_]*)\\s*(?=\\,|:|\\)|\\})", Some Identifier)];
          end_ = [("(?!\\,|\\)|\\})", None)];
          patterns = [
            Name.comment
          ]
        }
      };
      {
        name = Name.tuple_arg_annot_type;
        kind = Begin_end {
          meta_name = None;
          begin_ = [("\\:[ ]*", None)];
          end_ = [("(?=,|\\)|\\=\\>|\\})", None)];
          patterns = [
            Name.pattern
          ]
        }
      };
      {
        name = Name.if_or_switch_block;
        kind = Begin_end {
          meta_name = None;
          begin_ = [
            ("\\b(if|switch)\\b", Some Conditional);
            ("[ ]*(\\(|[a-z_])", None)
          ];
          end_ = [("\\)", None)];
          patterns = [
            Name.expr
          ]
        }
      };
      {
        name = Name.constructor;
        kind = Match {
          match_ = "(\\b[A-Z][a-zA-Z0-9_]*(\\b|\\())";
          match_name = Some Label;
          captures = []
        }
      };
      {
        name = Name.expr;
        kind = Patterns [
            Reference Name.string;
            Reference Name.comment;
            Reference Name.if_or_switch_block;
            Match {
              match_name = Some Conditional;
              match_ = "\\b(else)\\b";
              captures = []
            };
            Reference Name.record_expression;
            Reference Name.tuple_record_name;
            Reference Name.tuple_arg_annot_type;
            Reference Name.builtin_modules;
            Reference Name.operators;
            Match {
              match_name = None;
              match_ = "\\b([A-Z][a-zA-Z0-9_]+)\\.\\b";
              captures = [
                (1, Structure)
              ]
            };
            Match {
              match_name = None;
              match_ = "\\b([a-z_][a-zA-Z0-9_]*)\\b";
              captures = [
                (1, Identifier)
              ]
            };
            Reference Name.constructor;
            Match {
              match_name = Some Number;
              match_ = "\\b([0-9_]+)(tez|mutez|n)?\\b";
              captures = []
            };
            Match {
              match_name = Some Number;
              match_ = "\\b0x([0-9_]+)?\\b";
              captures = []
            };
            Match {
              match_name = Some Boolean;
              match_ = "\\b(true|false)\\b";
              captures = []
            }
          ]
      };
      {
        name = Name.type_declaration;
        kind = Begin_end {
          meta_name = None; 
          begin_ = [("\\b(type)\\b", Some Type)];
          end_ = [("(?=let|type|\\[@|\\/\\*|\\/\\/)", None)];
          patterns = [
            Name.comment;
            Name.type_identifier;
            Name.type_decl_identifier
          ]
        }
      };
      {
        name = Name.type_decl_identifier;
        kind = Begin_end {
          meta_name = None;
          begin_ = [("(=)", Some Operator)];
          end_ = [("(?=let|type|\\[@|\\/\\*|\\/\\/)", None)];
          patterns = [
            Name.comment;
            Name.pattern
          ]
        }
      };
      {
        name = Name.builtin_types;
        kind = Match {
          match_name = Some Builtin_type; 
          match_ = "\\b(int|nat|address|tez|contract|list|option|unit|bool|signature|bytes|big_map|chain_id|key|key_hash|map|operation|set|string|timestamp)\\b";
          captures = [];
        }
      };
      {
        name = Name.builtin_big_map;
        kind = Match {
          match_name = None;
          match_ = "\\b(Big_map)\\.(empty|literal|find_opt|mem|update|add|remove|get_and_update|identifier)\\b";
          captures = [
            (1, Builtin_module);
            (2, Builtin_function)
          ]
        }
      };
      {
        name = Name.builtin_bitwise;
        kind = Match {
          match_name = None;
          match_ = "\\b(Bitwise)\\.(and|or|xor|shift_left|shift_right)\\b";
          captures = [
            (1, Builtin_module);
            (2, Builtin_function)
          ]
        }
      };
      {
        name = Name.builtin_bytes;
        kind = Match {
          match_name = None;
          match_ = "\\b(Bytes)\\.(concat|sub|pack|unpack|length)\\b";
          captures = [
            (1, Builtin_module);
            (2, Builtin_function)
          ]
        }
      };
      {
        name = Name.builtin_crypto;
        kind = Match {
          match_name = None;
          match_ = "\\b(Crypto)\\.(blake2b|sha256|sha512|hash_key|check)\\b";
          captures = [
            (1, Builtin_module);
            (2, Builtin_function)
          ]
        }
      };
      {
        name = Name.builtin_list;
        kind = Match {
          match_name = None;
          match_ = "\\b(List)\\.(length|size|head_opt|tail_opt|iter|map|fold|fold_left|fold_right)\\b";
          captures = [
            (1, Builtin_module);
            (2, Builtin_function)
          ]
        }
      };
      {
        name = Name.builtin_map;
        kind = Match {
          match_name = None;
          match_ = "\\b(Map)\\.(empty|literal|find_opt|update|add|remove|iter|map|fold|size|mem|get_and_update)\\b";
          captures = [
            (1, Builtin_module);
            (2, Builtin_function)
          ]
        }
      };
      {
        name = Name.builtin_set;
        kind = Match {
          match_name = None;
          match_ = "\\b(Set)\\.(empty|literal|mem|cardinal|add|remove|iter|fold|fold_desc)\\b";
          captures = [
            (1, Builtin_module);
            (2, Builtin_function)
          ]
        }
      };
      {
        name = Name.builtin_string;
        kind = Match {
          match_name = None;
          match_ = "\\b(String)\\.(length|sub|concat)\\b";
          captures = [
            (1, Builtin_module);
            (2, Builtin_function)
          ]
        }
      };
      {
        name = Name.builtin_tezos;
        kind = Match {
          match_name = None;
          match_ = "\\b(Tezos)\\.(now|balance|amount|sender|address|self_address|self|source|implicit_account|create_contract|failwith|chain_id|transaction|set_delegate|get_contract_opt|get_entrypoint_opt|level|pairing_check|sapling_empty_state|sapling_verify_update|create_ticket|read_ticket|split_ticket|join_tickets|level|pairing_check|never)\\b";
          captures = [
            (1, Builtin_module);
            (2, Builtin_function)
          ]
        }
      };
      {
        name = Name.builtin_test;
        kind = Match {
          match_name = None;
          match_ = "\\b(Test)\\.(originate|set_now|set_source|set_baker|transfer|transfer_exn|get_storage|get_balance|michelson_equal|log|reset_state|nth_bootstrap_account|last_originations|compile_expression|compile_expression_subst|compile_value)\\b";
          captures = [
            (1, Builtin_module);
            (2, Builtin_function)
          ]
        }
      };
      {
        name = Name.builtin_toplevel;
        kind = Match {
          match_name = None;
          match_ = "\\b(is_nat|abs|int|failwith|assert|ediv)\\b";
          captures = [
            (1, Builtin_module);
            (2, Builtin_function)
          ]
        }
      };
      {
        name = Name.builtin_modules;
        kind = Patterns [
          Reference Name.builtin_big_map;
          Reference Name.builtin_bitwise;
          Reference Name.builtin_bytes;
          Reference Name.builtin_crypto;
          Reference Name.builtin_list;
          Reference Name.builtin_map;
          Reference Name.builtin_set;
          Reference Name.builtin_string;
          Reference Name.builtin_tezos;
          Reference Name.builtin_test;
          Reference Name.builtin_toplevel;
        ]
      };
      {
        name = Name.pattern;
        kind = Patterns [
          Reference Name.pattern_par;
          Reference Name.pattern_record;
          Reference Name.pattern_sum;
          Reference Name.builtin_types;
          Match {
            match_name = Some Builtin_type;
            match_ = "\\b([_a-z][a-zA-Z0-9$_]*)\\b";
            captures = [];
          }
        ]
      };
      {
        name = Name.pattern_par;
        kind = Begin_end {
          meta_name = None;
          begin_ = [("\\(", None)];
          end_ = [("\\)", None)];
          patterns = [
            Name.pattern
          ]
        }
      };
      {
        name = Name.pattern_sum;
        kind = Match {
          match_name = None;
          match_ = "\\b(\\|?[A-Z][a-zA-Z0-9_]*)+\\b";
          captures = [
            (1, Label)
          ]
        }
      };
      {
        name = Name.pattern_record;
        kind = Begin_end {
          meta_name = None;
          begin_ = [("{", None)];
          end_ = [("}", None)];
          patterns = [
            Name.comment;
            Name.pattern_record_item
          ]
        }
      };
      {
        name = Name.pattern_record_item;
        kind = Begin_end {
          meta_name = None;
          begin_ = [("([a-z_][A-Za-z0-9_]*)", Some Type)];
          end_ = [("(?=\\,|\\})", None)];
          patterns = [
            Name.comment;
            Name.pattern
          ]
        }
      };
      {
        name = Name.type_identifier;
        kind = Match {
          match_name = None;
          match_ = "\\b([_a-z][a-zA-Z0-9$_]*)\\b";
          captures = [1, Type]
        }
      };
      
      Helpers.c_comment;
      
    ]
  } in
  let s = Yojson.Safe.pretty_to_string json in
  Result.ok s