opam exec -- dune build ./src/bin/js_main.bc.js
chmod 777 _build/default/src/bin/js_main.*  jsoo/dist/*
cp _build/default/src/bin/js_main.* jsoo/dist
