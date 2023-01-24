include Ligo_api_common
module Mutate = Mutate_js

module Run = struct
  module Compile = Ligo_compile
  module Helpers = Ligo_compile.Helpers
  module Run = Ligo_run.Of_michelson
  module Raw_options = Compiler_options.Raw_options

  let test _ _ _ _ () = failwith "not implemented for JS"
  let test_expression _ _ _ _ _ () = failwith "not implemented for JS"
  let dry_run _ _ _ _ _ _ _ _ _ _ _ () = failwith "not implemented for JS"
  let interpret _ _ _ _ _ _ _ _ _ _ () = failwith "not implemented for JS"
  let evaluate_call _ _ _ _ _ _ _ _ _ _ () = failwith "not implemented for JS"
  let evaluate_expr _ _ _ _ _ _ _ _ _ () = failwith "not implemented for JS"
end

module Ligo_init = struct
  type project_entity =
    [ `CONTRACT
    | `LIBRARY
    ]

  let contract_template_url_map =
    Map.of_alist_exn
      (module String)
      [ "NFT-factory-cameligo", "https://github.com/ligolang/NFT-factory-cameligo"
      ; "NFT-factory-jsligo", "https://github.com/ligolang/NFT-factory-jsligo"
      ; "randomness-cameligo", "https://github.com/ligolang/randomness-cameligo"
      ; "randomness-jsligo", "https://github.com/ligolang/randomness-jsligo"
      ; "shifumi-cameligo", "https://github.com/ligolang/shifumi-cameligo"
      ; "shifumi-jsligo", "https://github.com/ligolang/shifumi-jsligo"
      ; "multisig-cameligo", "https://github.com/ligolang/multisig-cameligo"
      ; "multisig-jsligo", "https://github.com/ligolang/multisig-jsligo"
      ; "advisor-cameligo", "https://github.com/ligolang/advisor-cameligo"
      ; "advisor-jsligo", "https://github.com/ligolang/advisor-jsligo"
      ; ( "predictive-market-cameligo"
        , "https://github.com/ligolang/predictive-market-cameligo" )
      ; "predictive-market-jsligo", "https://github.com/ligolang/predictive-market-jsligo"
      ; "permit-cameligo", "https://github.com/ligolang/permit-cameligo"
      ; "permit-jsligo", "https://github.com/ligolang/permit-jsligo"
      ; "dao-cameligo", "https://github.com/ligolang/dao-cameligo"
      ; "dao-jsligo", "https://github.com/ligolang/dao-jsligo"
      ]


  let library_template_url_map =
    Map.of_alist_exn
      (module String)
      [ "ligo-bigarray", "https://github.com/ligolang/bigarray-cameligo"
      ; "ligo-math-lib", "https://github.com/ligolang/math-lib-cameligo"
      ; "ligo-fa", "https://github.com/ligolang/contract-catalogue"
      ; "ligo-permit", "https://github.com/ligolang/permit-cameligo"
      ; "ligo-breathalyzer", "https://github.com/marigold-dev/breathalyzer"
      ; "ligo-extendable-fa2", "https://github.com/smart-chain-fr/ligoExtendableFA2"
      ]


  let determine_map ~kind =
    match kind with
    | `CONTRACT -> contract_template_url_map
    | `LIBRARY -> library_template_url_map


  let list' ~kind = List.sort ~compare:String.compare @@ Map.keys (determine_map ~kind)

  let list ~kind ~display_format ~no_colour () =
    Api_helpers.format_result ~display_format ~no_colour Formatter.list_format
    @@ fun ~raise:_ -> list' ~kind


  let new_project
      ~version:_
      ~kind:_
      ~project_name_opt:_
      ~template:_
      ~display_format:_
      ~no_colour:_
      ~registry:_
      ()
    =
    failwith "not implemented for JS"
end
