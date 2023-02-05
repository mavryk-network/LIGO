WEB_STAGING_AREA = $(TMPDIR)/ligo-ide

.PHONY: build-demo-webide demo-webide-start

node_modules/@prometheansacrifice/secp256k1-wasm: package.json
	npm i

node_modules/@prometheansacrifice/ocaml-bls12-381/dist: package.json
	npm i

node_modules/@prometheansacrifice/hacl-wasm: package.json
	npm i

node_modules/@prometheansacrifice/secp256k1-wasm/src/secp256k1.wasm: node_modules/@prometheansacrifice/secp256k1-wasm

node_modules/@prometheansacrifice/ocaml-bls12-381/dist/blst.wasm: node_modules/@prometheansacrifice/ocaml-bls12-381/dist

node_modules/@prometheansacrifice/hacl-wasm/*.wasm: node_modules/@prometheansacrifice/hacl-wasm

node_modules: package.json package-lock.json
	npm i

_demo-webide_build/demo-webide.bundle.js: node_modules ./demo-webide.js ./rollup.config.mjs
	npm run build

build-demo-webide: node_modules/@prometheansacrifice/secp256k1-wasm/src/secp256k1.wasm node_modules/@prometheansacrifice/hacl-wasm/*.wasm ../tools/webide-new/ligo-webide-frontend/ligo-ide/public/favicon.ico _demo-webide_build/demo-webide.bundle.js ./node_modules/@prometheansacrifice/ocaml-bls12-381/dist/blst.wasm index.html ../_build/default/src/bin/js_main.bc.js
	rm -rf $(WEB_STAGING_AREA)
	mkdir $(WEB_STAGING_AREA)
	cp ../_build/default/src/bin/js_main.bc.runtime.js $(WEB_STAGING_AREA) # TODO(prometheansacrifice): this is only needed for dev builds
	cp $^ $(WEB_STAGING_AREA)/


demo-webide-start:
	make build-demo-webide
	cp -R ../_build $(WEB_STAGING_AREA)
	python -m http.server -d $(WEB_STAGING_AREA)

