module Textmate = SyntaxHighlighting.Textmate
module Helpers = Textmate.Helpers

let (let*) = Result.bind

let syntax_highlighting () = 
  let* json = Textmate.to_json {
    syntax_name = "ReasonLIGO";
    scope_name = "source.religo";
    file_types = [
      "religo";
      "rligo"
    ];
    folding_start_marker = Some "{";
    folding_stop_marker = Some "}";
    syntax_patterns = [
      Reference "#macro";
      Reference "#type-decl";
      Reference "#let-decl";
      Reference "#comment"
    ];
    repository = [
      Helpers.macro "religo";
      {
        name = "let-decl";
        kind = Begin_end {
          meta_name = None;
          begin_ = "\\b(let)\\b";
          begin_captures = [
            (1, "keyword.other.let-binding.religo")
          ];
          end_ = "(?=let|type|\\[@|\\/\\*|\\/\\/)";
          end_captures = [];
          patterns = [
            Reference "#let-name";
            Reference "#expr";
            Reference "#comment";
          ]
        }
      };
      {
        name = "let-name";
        kind = Begin_end {
          meta_name = None;
          begin_ = "\\G[ ]*(\\b(rec)\\b\\s\\b)?([a-z_][A-Za-z0-9_]*)\\b";
          begin_captures = [
            (2, "storage.modifier.recursive.religo");
            (3, "entity.name.variable.religo");
          ];
          end_ = "(\\=)";
          end_captures = [
            (1, "keyword.operator.assignment.religo")
          ];
          patterns = [
            Reference "#type_annotation";
            Reference "#comment"
          ]
        };
      };
      {
        name = "type_annotation";
        kind = Begin_end {
          meta_name = None;
          begin_ = "\\G[ ]*(\\:)";
          begin_captures = [];
          end_ = "(?=\\=)";
          end_captures = [];
          patterns = [
            Reference "#pattern";
            Reference "#comment"
          ]
        }
      };
      {
        name = "operators";
        kind = Match {
          match_name = Some "keyword.operator.religo";
          match_ = "\\b(mod|ediv)\\b|(\\+|\\-|\\*|\\/|==|\\|\\||\\&\\&)";
          captures = [];
        }
      };
      Helpers.string "religo";
      {
        name = "record_expr";
        kind = Begin_end {
          meta_name = None;
          begin_ = "(?<=\\=)\\s*\\{";
          begin_captures = [];
          end_ = "\\}";
          end_captures = [];
          patterns = [
            Reference "#tuple_record_name";
            Reference "#expr"
          ]
        }
      };
      {
        name = "tuple_record_name";
        kind = Begin_end {
          meta_name = None;
          begin_ = "(?<=\\(|,|\\{)\\s*([a-z][A-Za-z0-9_]*)\\s*(?=\\,|:|\\)|\\})";
          begin_captures = [
            (1, "variable.parameter.religo")
          ];
          end_ = "(?!\\,|\\)|\\})";
          end_captures = [];
          patterns = [
            Reference "#comment"
          ]
        }
      };
      {
        name = "tuple_arg_annot_type";
        kind = Begin_end {
          meta_name = None;
          begin_ = "\\:[ ]*";
          begin_captures = [];
          end_ = "(?=,|\\)|\\=\\>|\\})";
          end_captures = [];
          patterns = [
            Reference "#pattern"
          ]
        }
      };
      {
        name = "if-or-switch-block";
        kind = Begin_end {
          meta_name = None;
          begin_ = "\\b(if|switch)\\b[ ]*(\\(|[a-z_])";
          begin_captures = [
            (1, "keyword.control.religo") 
          ];
          end_ = "\\)";
          end_captures = [];
          patterns = [
            Reference "#expr"
          ]
        }
      };
      {
        name = "constructor";
        kind = Match {
          match_ = "(\\b[A-Z][a-zA-Z0-9_]*(\\b|\\())";
          match_name = Some "variable.other.enummember";
          captures = []
        }
      };
      {
        name = "expr";
        kind = Patterns {
          p_name = None;
          p_kind = [
            Reference "#string";
            Reference "#comment";
            Reference "#if-or-switch-block";
            Match {
              match_name = Some "keyword.control.else.religo";
              match_ = "\\b(else)\\b";
              captures = []
            };
            Reference "#record_expr";
            Reference "#tuple_record_name";
            Reference "#tuple_arg_annot_type";
            Reference "#builtin-modules";
            Reference "#operators";
            Match {
              match_name = None;
              match_ = "\\b([A-Z][a-zA-Z0-9_]+)\\.\\b";
              captures = [
                (1, "storage.class.religo")
              ]
            };
            Match {
              match_name = None;
              match_ = "\\b([a-z_][a-zA-Z0-9_]*)\\b";
              captures = [
                (1, "storage.var.religo")
              ]
            };
            Reference "#constructor";
            Match {
              match_name = Some "constant.numeric.religo";
              match_ = "\\b([0-9_]+)(tez|mutez|n)?\\b";
              captures = []
            };
            Match {
              match_name = Some "constant.numeric.religo";
              match_ = "\\b0x([0-9_]+)?\\b";
              captures = []
            };
            Match {
              match_name = Some "constant.language.religo";
              match_ = "\\b(true|false)\\b";
              captures = []
            }
          ]
        }
      };
      {
        name = "type-decl";
        kind = Begin_end {
          meta_name = None; 
          begin_ = "\\b(type)\\b";
          begin_captures = [
            (1, "keyword.other.type.religo")
          ];
          end_ = "(?=let|type|\\[@|\\/\\*|\\/\\/)";
          end_captures = [];
          patterns = [
            Reference "#comment";
            Reference "#type-identifier";
            Reference "#type-decl-identifier"
          ]
        }
      };
      {
        name = "type-decl-identifier";
        kind = Begin_end {
          meta_name = None;
          begin_ = "(=)";
          begin_captures = [
            (1, "keyword.operator.assignment.religo")
          ];
          end_ = "(?=let|type|\\[@|\\/\\*|\\/\\/)";
          end_captures = [];
          patterns = [
            Reference "#comment";
            Reference "#pattern"
          ]
        }
      };
      {
        name = "builtin-types";
        kind = Match {
          match_name = Some "support.type.religo"; 
          match_ = "\\b(int|nat|address|tez|contract|list|option|unit|bool|signature|bytes|big_map|chain_id|key|key_hash|map|operation|set|string|timestamp)\\b";
          captures = [];
        }
      };
      {
        name = "builtin-big-map";
        kind = Match {
          match_name = None;
          match_ = "\\b(Big_map)\\.(empty|literal|find_opt|mem|update|add|remove|get_and_update|identifier)\\b";
          captures = [
            (1, "support.class.religo");
            (2, "support.function.religo")
          ]
        }
      };
      {
        name = "builtin-bitwise";
        kind = Match {
          match_name = None;
          match_ = "\\b(Bitwise)\\.(and|or|xor|shift_left|shift_right)\\b";
          captures = [
            (1, "support.class.religo");
            (2, "support.function.religo")
          ]
        }
      };
      {
        name = "builtin-bytes";
        kind = Match {
          match_name = None;
          match_ = "\\b(Bytes)\\.(concat|sub|pack|unpack|length)\\b";
          captures = [
            (1, "support.class.religo");
            (2, "support.function.religo")
          ]
        }
      };
      {
        name = "builtin-crypto";
        kind = Match {
          match_name = None;
          match_ = "\\b(Crypto)\\.(blake2b|sha256|sha512|hash_key|check)\\b";
          captures = [
            (1, "support.class.religo");
            (2, "support.function.religo")
          ]
        }
      };
      {
        name = "builtin-list";
        kind = Match {
          match_name = None;
          match_ = "\\b(List)\\.(length|size|head_opt|tail_opt|iter|map|fold|fold_left|fold_right)\\b";
          captures = [
            (1, "support.class.religo");
            (2, "support.function.religo")
          ]
        }
      };
      {
        name = "builtin-map";
        kind = Match {
          match_name = None;
          match_ = "\\b(Map)\\.(empty|literal|find_opt|update|add|remove|iter|map|fold|size|mem|get_and_update)\\b";
          captures = [
            (1, "support.class.religo");
            (2, "support.function.religo")
          ]
        }
      };
      {
        name = "builtin-set";
        kind = Match {
          match_name = None;
          match_ = "\\b(Set)\\.(empty|literal|mem|cardinal|add|remove|iter|fold|fold_desc)\\b";
          captures = [
            (1, "support.class.religo");
            (2, "support.function.religo")
          ]
        }
      };
      {
        name = "builtin-string";
        kind = Match {
          match_name = None;
          match_ = "\\b(String)\\.(length|sub|concat)\\b";
          captures = [
            (1, "support.class.religo");
            (2, "support.function.religo")
          ]
        }
      };
      {
        name = "builtin-tezos";
        kind = Match {
          match_name = None;
          match_ = "\\b(Tezos)\\.(now|balance|amount|sender|address|self_address|self|source|implicit_account|create_contract|failwith|chain_id|transaction|set_delegate|get_contract_opt|get_entrypoint_opt|level|pairing_check|sapling_empty_state|sapling_verify_update|create_ticket|read_ticket|split_ticket|join_tickets|level|pairing_check|never)\\b";
          captures = [
            (1, "support.class.religo");
            (2, "support.function.religo")
          ]
        }
      };
      {
        name = "builtin-test";
        kind = Match {
          match_name = None;
          match_ = "\\b(Test)\\.(originate|set_now|set_source|set_baker|transfer|transfer_exn|get_storage|get_balance|michelson_equal|log|reset_state|nth_bootstrap_account|last_originations|compile_expression|compile_expression_subst|compile_value)\\b";
          captures = [
            (1, "support.class.religo");
            (2, "support.function.religo")
          ]
        }
      };
      {
        name = "builtin-toplevel";
        kind = Match {
          match_name = None;
          match_ = "\\b(is_nat|abs|int|failwith|assert|ediv)\\b";
          captures = [
            (1, "support.class.religo");
            (2, "support.function.religo")
          ]
        }
      };
      {
        name = "builtin-modules";
        kind = Patterns {
          p_name = None;
          p_kind = [
            Reference "#builtin-big-map";
            Reference "#builtin-bitwise";
            Reference "#builtin-bytes";
            Reference "#builtin-crypto";
            Reference "#builtin-list";
            Reference "#builtin-map";
            Reference "#builtin-set";
            Reference "#builtin-string";
            Reference "#builtin-tezos";
            Reference "#builtin-test";
            Reference "#builtin-toplevel";
          ]
        }
      };
      {
        name = "pattern";
        kind = Patterns {
          p_name = None;
          p_kind = [
            Reference "#pattern-par";
            Reference "#pattern-record";
            Reference "#pattern-sum";
            Reference "#builtin-types";
            Match {
              match_name = Some "storage.type.religo";
              match_ = "\\b([_a-z][a-zA-Z0-9$_]*)\\b";
              captures = [];
            }
          ]
        }
      };
      {
        name = "pattern-par";
        kind = Begin_end {
          meta_name = None;
          begin_ = "\\(";
          begin_captures = [];
          end_ = "\\)";
          end_captures = [];
          patterns = [
            Reference "#pattern"
          ]
        }
      };
      {
        name = "pattern-sum";
        kind = Match {
          match_name = None;
          match_ = "\\b(\\|?[A-Z][a-zA-Z0-9_]*)+\\b";
          captures = [
            (1, "variable.other.enummember")
          ]
        }
      };
      {
        name = "pattern-record";
        kind = Begin_end {
          meta_name = None;
          begin_ = "{";
          begin_captures = [];
          end_ = "}";
          end_captures = [];
          patterns = [
            Reference "#comment";
            Reference "#pattern-record-item"
          ]
        }
      };
      {
        name = "pattern-record-item";
        kind = Begin_end {
          meta_name = None;
          begin_ = "([a-z_][A-Za-z0-9_]*)";
          begin_captures = [
            (1, "entity.name.type.religo")
          ];
          end_ = "(?=\\,|\\})";
          end_captures = [];
          patterns = [
            Reference "#comment";
            Reference "#pattern"
          ]
        }
      };
      {
        name = "type-identifier";
        kind = Match {
          match_name = None;
          match_ = "\\b([_a-z][a-zA-Z0-9$_]*)\\b";
          captures = [1, "entity.name.type.religo"]
        }
      };
      (* TODO FIX COMMENTS PROPERLY! *)
      Helpers.c_comment "religo";
      
    ]
  } in
  let s = Yojson.Safe.pretty_to_string json in
  Result.ok s