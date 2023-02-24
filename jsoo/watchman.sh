#! /bin/sh

watchman watch ../_build/default/src/passes/01-lexing/jsligo/
watchman watch ../_build/default/src/passes/01-lexing/cameligo/
watchman -j <<-EOT
["trigger", "../_build/default/src/passes/01-lexing/jsligo/", {
  "name": "cp-jsoo-js",
  "expression": ["suffix", "js"],
  "chdir": "$PWD",
  "command": ["npm", "run", "cp-static"]
}]
EOT
