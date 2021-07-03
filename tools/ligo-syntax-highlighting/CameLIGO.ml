module Textmate = SyntaxHighlighting.Textmate
module Helpers = Textmate.Helpers

let (let*) = Result.bind

let syntax_highlighting () =
  let* json = Textmate.to_json {
    syntax_name          = "mligo";
    scope_name           = "source.mligo";
    file_types           = [];
    folding_start_marker = None;
    folding_stop_marker  = None;
    syntax_patterns      = [
      Reference "#string";
      Reference "#single-quotes";
      Reference "#comment";
      Reference "#macro";
      Reference "#list-cons";
      Reference "#let-binding";
      Reference "#lambda";
      Reference "#type-definition";
      Reference "#type-annotation"; 
      Reference "#control-keywords";
      Reference "#other-keywords";
      Reference "#numeric-literals";
      Reference "#operators";
      Reference "#identifier-constructor";
      Reference "#identifier-lower"
    ];
    repository           = [
      Helpers.macro "mligo";
      {
        name = "let-binding";
        kind = Begin_end {
          meta_name = None;
          begin_ = "\\b(let)\\b";
          begin_captures = [
            (1, "keyword.other.let-binding.mligo")
          ];
          end_ = "(\\=)";
          end_captures = [
            (1, "keyword.operator.eq.mligo")
          ];
          patterns = [
            Reference "#comment";
            Reference "#let-rec";
            Reference "#let-function";
            Reference "#let-constant";
          ]
        }
      };
      {
        name = "let-rec";
        kind = Match {
          match_ = "\\b(rec)\\b";
          captures = [];
          match_name = Some "keyword.other.recursive.mligo"
        }
      };
      {
        name = "let-function";
        kind = Begin_end {
          meta_name = None;
          begin_ = "\\G\\s*([a-zA-Z_]\\w*)\\b(?=\\s*\\()";
          begin_captures = [
            (1, "entity.name.function.mligo");
          ];
          end_ = "(?=\\=)";
          end_captures = [];
          patterns = [
            Reference "#comment";
            Reference "#parameter-list";
            Reference "#type-annotation"
          ]
        }
      };
      {
        name = "parameter-list";
        kind = Begin_end {
          meta_name = None;
          begin_ = "\\(";
          begin_captures = [
            (1, "keyword.operator.parenthesis.mligo")
          ];
          end_ = "\\)";
          end_captures = [
            (1, "keyword.operator.parenthesis.mligo")
          ];
          patterns = [
            Reference "#comment";
            Reference "#identifier-parameter";
            Reference "#type-annotation"
          ]
        }
      };
      {
        name = "parenthesized-definition";
        kind = Begin_end {
          meta_name = None;
          begin_ = "\\(";
          begin_captures = [
            (1, "keyword.operator.parenthesis.mligo")
          ];
          end_ = "\\)";
          end_captures = [
            (1, "keyword.operator.parenthesis.mligo")
          ];
          patterns = [
            Reference "#comment";
            Reference "#identifier-variable-decl";
            Reference "#type-annotation"
          ]
        }
      };
      {
        name = "type-annotation";
        kind = Begin_end {
          meta_name = None;
          begin_ = "(:)\\s*";
          begin_captures = [
            (1, "keyword.operator.type.mligo")
          ];
          end_ = "(?:[;|]|(?=[)=}])|$)";
          end_captures = [];
          patterns = [
            Reference "#comment";
            Reference "#type-expression"
          ]
        }
      };
      {
        name = "type-expression";
        kind = Patterns {
          p_name = None;
          p_kind = [
            Begin_end {
              meta_name = None;
              begin_ = "\\(";
              begin_captures = [];
              end_ = "\\)";
              end_captures = [];
              patterns = [
                Reference "#comment";
                Reference "#type-expression"
              ]
            };
            Match {
              match_ = "([^=()|;}/]+)";
              match_name = None;
              captures = [
                (1, "entity.name.type.mligo")
              ]
            }
          ]
        }
      };
      {
        name = "let-constant";
        kind = Begin_end {
          meta_name = None;
          begin_ = "\\G";
          begin_captures = [];
          end_ = "(?=\\=)";
          end_captures = [];
          patterns = [
            Reference "#comment";
            Reference "#type-annotation";
            Reference "#parenthesized-definition";
            Reference "#identifier-variable-decl";
          ]
        }
      };
      {
        name = "lambda";
        kind = Begin_end {
          meta_name = None;
          begin_ = "\\b(fun)\\b";
          begin_captures = [
            (1, "keyword.other.lambda.mligo")
          ];
          end_ = "(->)";
          end_captures = [
            (1, "keyword.operator.lambda.mligo")
          ];
          patterns = [
            Reference "#comment";
            Reference "#parameter-list"
          ]
        }
      };
      {
        name = "type-definition";
        kind = Begin_end {
          meta_name = None;
          begin_ = "\\b(type)\\s+([a-zA-Z_]\\w*)\\b";
          begin_captures = [
            (1, "keyword.other.typedef.mligo");
            (2, "entity.name.type.mligo")
          ];
          end_ = "(?=(?:\\blet\\b|\\btype\\b|^\\s*#\\w+))";
          end_captures = [];
          patterns = [
            Reference "#comment";
            Reference "#struct-type";
            Reference "#sum-type";
            Reference "#type-alias"
          ]
        }
      };
      {
        name = "struct-type";
        kind = Begin_end {
          meta_name = None;
          begin_ = "\\{";
          begin_captures = [];
          end_ = "\\}";
          end_captures = [];
          patterns = [
            Reference "#comment";
            Reference "#identifier-variable-decl";
            Reference "#type-annotation"
          ]
        }
      };
      {
        name = "sum-type";
        kind = Begin_end {
          meta_name = None;
          begin_ = "\\b([A-Z]\\w*)\\s+(of)?";
          begin_captures = [
            (1, "entity.name.function.mligo");
            (2, "keyword.other.of.mligo")
          ];
          end_ = "(\\||(?=\\blet\\b|\\btype\\b|^\\s*#\\w+))";
          end_captures = [];
          patterns = [
            Reference "#comment";
            Reference "#type-expression"
          ]
        }
      };
      {
        name = "type-alias";
        kind = Begin_end {
          meta_name = None;
          begin_ = "\\G\\s*=\\s*(?=[(a-z])";
          begin_captures = [];
          end_ = "(?=\\blet\\b|\\btype\\b|^\\s*#\\w+)";
          end_captures = [];
          patterns = [
            Reference "#comment";
            Reference "#type-expression"
          ]
        }
      };
      Helpers.string "mligo";
      {
        name = "single-quotes";
        kind = Begin_end {
          meta_name = Some "string.quoted.single.mligo";
          begin_ = "\\'";
          begin_captures = [];
          end_ = "\\'";
          end_captures = [];
          patterns = []
        }
      };
      Helpers.ocaml_comment "mligo";      
      {
        name = "list-cons";
        kind = Match {
          match_name = Some "keyword.operator.cons.mligo";
          match_ = "::";
          captures = []
        }
      };
      {
        name = "control-keywords";
        kind = Match {
          match_name = Some "keyword.control.mligo";
          match_ = "\\b(match|with|if|then|else|assert|failwith|begin|end)\\b";
          captures = []
        }
      };
      {
        name = "other-keywords";
        kind = Match {
          match_name = Some "keyword.other.mligo";
          match_ = "\\b(in)\\b";
          captures = []
        }
      };
      Helpers.numeric_literals "mligo";
      {
        name = "operators";
        kind = Match {
          match_name = Some "keyword.operator.other.mligo";
          match_ = "([-+*/])";
          captures = []
        }
      };
      {
        name = "identifier-lower";
        kind = Match {
          match_name = None;
          match_ = "\\b([a-z_]\\w*)\\b";
          captures = [
            (1, "entity.name.variable.mligo")
          ]
        }
      };
      {
        name = "identifier-constructor";
        kind = Match {
          match_name = None;
          match_ = "\\b([A-Z]\\w*)\\b";
          captures = [
            (1, "entity.name.function.constructor.mligo")
          ]
        }
      };
      {
        name = "identifier-parameter";
        kind = Match {
          match_name = None;
          match_ = "\\b([a-zA-Z_]\\w*)\\b";
          captures = [(1, "support.variable.parameter.mligo")]          
        }
      };
      {
        name = "identifier-variable-decl";
        kind = Match {
          match_name = None;
          match_ = "\\b([a-zA-Z_]\\w*)\\b";
          captures = [
            (1, "support.variable.mligo")
          ]
        }
      }
    ]
  } in
  let s = Yojson.Safe.pretty_to_string json in
  Result.ok s
