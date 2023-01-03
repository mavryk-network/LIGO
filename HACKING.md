# JS build of Ligo compiler

1. Run `opam exec -- dune build ./src/bin/js_main.bc.js` to compile and generate the JS bundle
2. Run `npm run dev` to generate a demo webide. Of course, if this is your first time setting up, run `npm install`
3. Use your favourite http server to serve the PoC webide assets. Eg: `python -m http.server`
