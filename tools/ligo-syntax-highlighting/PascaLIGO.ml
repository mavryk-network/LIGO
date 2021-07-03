module Textmate = SyntaxHighlighting.Textmate
module Helpers = Textmate.Helpers

let (let*) = Result.bind

let syntax_highlighting () = 
  let* json = Textmate.to_json {
    syntax_name          = "ligo";
    scope_name           = "source.ligo";
    file_types           = [];
    folding_start_marker = None;
    folding_stop_marker = None;
    syntax_patterns = [
      Reference "#string";
      Reference "#single-quotes";
      Reference "#comment";
      Reference "#macro";
      Reference "#function";
      Reference "#binding";
      Reference "#type-annotation";
      Reference "#type-definition";
      Reference "#control-keywords";
      Reference "#other-keywords";
      Reference "#operators";
      Reference "#function-application";
      Reference "#identifiers";
    ];
    repository = [
      {
        name = "function";
        kind = Begin_end {
          meta_name = None;
          begin_ = "\\b(recursive\\s+)?(function)\\s+([a-zA-Z_]\\w*)\\b";
          begin_captures = [
            (1, "keyword.other.recursive.ligo");
            (2, "keyword.other.function.ligo");
            (3, "entity.name.function");
          ];
          end_ = "\\b(is)\\b";
          end_captures = [
            (1, "keyword.operator.is.ligo")
          ];
          patterns = [
            Reference "#comment";
            Reference "#type-annotation";
            Reference "#binding";
          ]
        }
      };
      { 
        name = "binding";
        kind = Begin_end {
          meta_name = None;
          begin_ = "\\b(var|const)\\s+([a-zA-Z_]\\w*)\\b";
          begin_captures = [
            (1, "keyword.other.binding.ligo");
            (2, "entity.name.variable")
          ];
          end_ = "(?=[=),;]|:=)";
          end_captures = [];
          patterns = [
            Reference "#comment";
            Reference "#type-annotation"
        ]}
      };
      { 
        name = "macro";
        kind = Begin_end {
          meta_name = None;
          begin_ = "^\\s*((#)\\w+)";
          begin_captures = [
              (1, "meta.preprocessor.ligo");
              (2, "punctuation.definition.directive.ligo")
          ];
          end_ = "$";
          end_captures = [];
          patterns = [
            Reference "#string";
            Reference "#comment"
          ]
        }
      };
      {
        name = "type-annotation";
        kind = Begin_end {
          meta_name = None;
          begin_ = "(:(?!=))\\s*";
          begin_captures = [
              (1, "keyword.operator.type.ligo" )
          ];
          end_ = "(?:\\||(?=[;)=}\\]]|\\bis\\b|:=)|$)";
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
              match_name = None;
              match_ = "((?:(?!\\bis\\b|:=)[^=()|;}/\\]])*)";
              captures = [
                  (1, "entity.name.type.ligo")
              ]
            }
        ]
      }
    };
    {
      name = "type-definition";
      kind = Begin_end {
        meta_name = None;
        begin_ = "\\b(type)\\s+([a-zA-Z_]\\w*)\\s+(is)\\b";
        begin_captures = [
            (1, "keyword.other.typedef.ligo");
            (2, "entity.name.type.ligo");
            (3, "keyword.other.is.ligo")
        ];
        end_ = "(?=\\b(?:function|type|const|var)\\b|^\\s*#\\w+)";
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
        begin_ = "\\b(record)\\s*(\\[?)";
        begin_captures = [
            (1, "keyword.other.record.ligo");
            (2, "keyword.other.begin.ligo")
        ];
        end_ = "(\\]|\\bend\\b)";
        end_captures = [
            (1, "keyword.other.end.ligo")
        ];
        patterns = [
            Reference "#comment";
            Reference "#identifiers";
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
            (1, "entity.name.function.ligo");
            (2, "keyword.other.of.ligo")
        ];
        end_ = "(\\||(?=\\b(?:function|type|const|var)\\b|^\\s*#\\w+))";
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
        begin_ = "\\G\\s*(?!record\\b)(?=[(a-z])";
        begin_captures = [];
        end_ = "(?=\\b(?:function|type|const|var)\\b|^\\s*#\\w+)";
        end_captures = [];
        patterns = [
            Reference "#comment";
            Reference "#type-expression"
        ]
      }
    };
    Helpers.string "ligo";

    {
      name = "single-quotes";
      kind = Begin_end {
        meta_name = Some "string.quoted.single.ligo";
        begin_ = "\\'";
        begin_captures = [];
        end_ = "\\'";
        end_captures = [];
        patterns = []
      }
    };
    Helpers.ocaml_comment "ligo";
    {
      name = "list-cons";
      kind = Match {
        match_ = "::";
        match_name = Some "keyword.operator.cons.ligo";
        captures = []
      }
    };
    {
      name = "control-keywords";
      kind = Match {
        match_name = Some "keyword.control.ligo";
        match_ = "\\b(case|of|if|then|else|for|in|step|to|skip|assert|failwith|begin|end|contains)\\b";
        captures = []
      }
    };
    {
      name = "other-keywords";
      kind = Match {
        match_name = Some "keyword.other.ligo";
        match_ = "\\b(block|with|record|set|map|list)\\b";
        captures = []
      }
    };
    {
      name = "numeric-literals";
      kind = Match {
        match_name = Some "constant.numeric.ligo";
        match_ = "\\b\\d+";
        captures = []
      }
    };
    {
      name = "operators";
      kind = Match {
        match_name = Some "keyword.operator.other.ligo";
        match_ = "([-+*/=]|->|:=)";
        captures = []
      }
    };
    {
      name = "function-application";
      kind = Match {
        match_name = None;
        match_ = "\\b([a-zA-Z_]\\w*)\\s+\\(";
        captures = [
            (1, "entity.name.function")
        ]
      }
    };
    {
      name = "identifiers";
      kind = Match {
        match_name = None;
        match_ = "\\b([a-zA-Z_]\\w*)\\b";
        captures = [
            (1, "entity.name.variable")
        ]
      }
    }
    ]
  } in
  let s = Yojson.Safe.pretty_to_string json in
  Result.ok s