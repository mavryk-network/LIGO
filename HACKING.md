# JS build of Ligo compiler

1. Run `opam exec -- dune build ./src/bin/js_main.bc.js` to compile and generate the JS bundle
2. Run `npm run dev` to generate a demo webide. Of course, if this is your first time setting up, run `npm install`
3. Use your favourite http server to serve the PoC webide assets. Eg: `python -m http.server`

## Where to find the NPM dependencies

1. For ocaml-bls12-381 - build from source, `mkdir -p dist/ && cp _build/default/src/blst.* dist`. You can now run `npm pack` or `npm publish`
2. For hacl-wasm - can be found directly from NPM, but it needs patches. The one there assumes a full Node.js environment when loaded in commonjs mode. This is reasonable, but not ideal for us as we rely on a commonjs setup for bundling.
3. For ocaml-secp256k1-wasm - build from source, and you'll find `./dist/` folder.
