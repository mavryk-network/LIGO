open Api_helpers

let dump_changelog display_format no_colour () =
  let value = Changelog.changelog in
  let format = Formatter.changelog_format in
  format_result ~display_format ~no_colour format (fun ~raise:_ -> value)


module Compile = Compile
module Transpile = Transpile
module Info = Info
module Print = Print
module Api_helpers = Api_helpers
module Formatter = Formatter
