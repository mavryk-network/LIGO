module Textmate = SyntaxHighlighting.Textmate
module Helpers = Textmate.Helpers

let (let*) = Result.bind

module Name = struct
  let string                = "string"
  let single_quotes         = "single-quotes"
  let comment               = "comment"
  let macro                 = "macro"
  let function_             = "function"
  let binding               = "binding"
  let type_annotation       = "type-annotation"
  let type_definition       = "type-definition"
  let control_keywords      = "control-keywords"
  let other_keywords        = "other-keywords"
  let operators             = "operators"
  let function_application  = "function-application"
  let identifiers           = "identifiers"
  let type_expression       = "type-expression"
  let struct_type           = "struct-type"
  let sum_type              = "sum-type"
  let type_alias            = "type-alias"
end

let syntax_highlighting () = 
  let* json = Textmate.to_json "ligo" {
    syntax_name          = "ligo";
    scope_name           = "source.ligo";
    file_types           = [];
    folding_start_marker = None;
    folding_stop_marker = None;
    syntax_patterns = [
      Reference Name.string;
      Reference Name.single_quotes;
      Reference Name.comment;
      Reference Name.macro;
      Reference Name.function_;
      Reference Name.binding;
      Reference Name.type_annotation;
      Reference Name.type_definition;
      Reference Name.control_keywords;
      Reference Name.other_keywords;
      Reference Name.operators;
      Reference Name.function_application;
      Reference Name.identifiers;
    ];
    repository = [
      Helpers.macro;
      {
        name = Name.function_;
        kind = Begin_end {
          meta_name = None;
          begin_ = [
            ("\\b(recursive\\s+)?", Some StorageClass);
            ("(function)\\s+", Some Function); 
            ("([a-zA-Z_]\\w*)\\b", Some Identifier)];
          end_ = [("\\b(is)\\b", Some Operator)];
          patterns = [
            Name.comment;
            Name.type_annotation;
            Name.binding;
          ]
        }
      };
      { 
        name = Name.binding;
        kind = Begin_end {
          meta_name = None;
          begin_ = [
            ("\\b(var|const)\\s+", Some Statement);
            ("([a-zA-Z_]\\w*)\\b", Some Identifier)];
          end_ = [
            ("(?=[=),;]|:=)", None)
          ];
          patterns = [
            Name.comment;
            Name.type_annotation
        ]}
      };
      {
        name = Name.type_annotation;
        kind = Begin_end {
          meta_name = None;
          begin_ = [("(:(?!=))\\s*", Some Operator)];
          end_ = [("(?:\\||(?=[;)=}\\]]|\\bis\\b|:=)|$)", None)];
          patterns = [
              Name.comment;
              Name.type_expression
          ]
        }
      };
      {
        name = Name.type_expression;
        kind = Patterns [
          Begin_end {
            meta_name = None;
            begin_ = [("\\(", None)];
            end_ = [("\\)", None)];
            patterns = [
                Name.comment;
                Name.type_expression
            ]
          };
          Match {
            match_name = None;
            match_ = "((?:(?!\\bis\\b|:=)[^=()|;}/\\]])*)";
            captures = [
                (1, Type)
            ]
          }
      ]
    };
    {
      name = Name.type_definition;
      kind = Begin_end {
        meta_name = None;
        begin_ = 
          [("\\b(type)", Some Type);
           ("\\s+([a-zA-Z_]\\w*)", Some Identifier);
           ("\\s+(is)\\b", Some Statement);
          ];
        end_ = [("(?=\\b(?:function|type|const|var)\\b|^\\s*#\\w+)", None)];
        patterns = [
            Name.comment;
            Name.struct_type;
            Name.sum_type;
            Name.type_alias
        ]
      }
    };
    {
      name = Name.struct_type;
      kind = Begin_end {
        meta_name = None;
        begin_ = [
          ("\\b(record)\\s*", Some Statement);
          ("(\\[?)", Some Statement)
        ];
        end_ = [("(\\]|\\bend\\b)", Some Statement)];
        patterns = [
            Name.comment;
            Name.identifiers;
            Name.type_annotation
        ]
      }
    };
    {
      name = Name.sum_type;
      kind = Begin_end {
        meta_name = None;
        begin_ = [
          ("\\b([A-Z]\\w*)\\s+", Some Label); 
          ("(of)?", Some Statement)
        ];
        end_ = [
          ("(\\||(?=\\b(?:function|type|const|var)\\b|^\\s*#\\w+))", None)
        ];
        patterns = [
            Name.comment;
            Name.type_expression
        ]
      }
    };
    {
      name = Name.type_alias;
      kind = Begin_end {
        meta_name = None;
        begin_ = [("\\G\\s*(?!record\\b)(?=[(a-z])", None)];
        end_ = [("(?=\\b(?:function|type|const|var)\\b|^\\s*#\\w+)", None)];
        patterns = [
            Name.comment;
            Name.type_expression
        ]
      }
    };
    ]
    @
    Helpers.string
    @
    [
    {
      name = Name.single_quotes;
      kind = Begin_end {
        meta_name = Some String;
        begin_ = [("\\'", None)];
        end_ = [("\\'", None)];
        patterns = []
      }
    };
    Helpers.ocaml_comment;
    {
      name = Name.control_keywords;
      kind = Match {
        match_name = Some Conditional;
        match_ = "\\b(case|of|if|then|else|for|in|step|to|skip|assert|failwith|begin|end|contains)\\b";
        captures = []
      }
    };
    {
      name = Name.other_keywords;
      kind = Match {
        match_name = Some Statement;
        match_ = "\\b(block|with|record|set|map|list)\\b";
        captures = []
      }
    };
    {
      name = Name.operators;
      kind = Match {
        match_name = Some Operator;
        match_ = "([-+*/=]|->|:=)";
        captures = []
      }
    };
    {
      name = Name.function_application;
      kind = Match {
        match_name = None;
        match_ = "\\b([a-zA-Z_]\\w*)\\s+\\(";
        captures = [
            (1, Identifier)
        ]
      }
    };
    {
      name = Name.identifiers;
      kind = Match {
        match_name = None;
        match_ = "\\b([a-zA-Z_]\\w*)\\b";
        captures = [
            (1, Identifier)
        ]
      }
    }
    ]
  } in
  let s = Yojson.Safe.pretty_to_string json in
  Result.ok s