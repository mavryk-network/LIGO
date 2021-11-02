#!/bin/sh
set +x

SOURCES="../src/stages/2-ast_imperative/types.ml ../src/stages/common/types.ml ../src/stages/common/enums.ml ../vendors/ligo-utils/simple-utils/var.ml ../vendors/ligo-utils/simple-utils/location.ml"

# hacks
cp ../src/stages/common/enums.ml ../src/stages/common/enums_backup.ml
sed -i 's/\[\@\@deriving only_interpreter_tags\]//' ../src/stages/common/enums.ml

for source in $SOURCES ; do
  coq-of-ocaml -config ./coq-of-ocaml.json $source || true
done

# hacks
mv ../src/stages/common/enums_backup.ml ../src/stages/common/enums.ml