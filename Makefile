# last refactor: 2021-05-05
.ONESHELL:

all: test

# Use install-deps instead of 'install' because usually 'make install' adds a
# binary to the system path and we don't want to confuse users
install-deps:
#	Install ligo/tezos specific system-level dependencies
	sudo scripts/install_native_dependencies.sh
	scripts/install_build_environment.sh # TODO: or scripts/install_opam.sh ?

build-deps:
	export PATH="/usr/local/bin$${PATH:+:}$${PATH:-}"
#	Create opam dev switch locally for use with Ligo, add merlin/etc
	if [ ! -d "./_opam" ];
	then scripts/setup_switch.sh;
	fi
	eval $$(opam config env)
# NEW-PROTOCOL-TEMPORARY
	git submodule sync --recursive
	git submodule update --init --recursive --remote
# NEW-PROTOCOL-TEMPORARY
#	Install OCaml build dependencies for Ligo
	scripts/install_vendors_deps.sh

build: build-deps
	export PATH="/usr/local/bin$${PATH:+:}$${PATH:-}"
	eval $$(opam config env)
#	Build Ligo for local dev use
	scripts/build_ligo_local.sh

test: build
	scripts/check_duplicate_filenames.sh || exit
	export PATH="/usr/local/bin$${PATH:+:}$${PATH:-}"
	eval $$(opam config env)
	scripts/test_ligo.sh

clean:
	dune clean
	rm -fr _coverage_all _coverage_cli _coverage_ligo

coverage:
	eval $$(opam config env)
	find . -name '*.coverage' | xargs rm -f
	dune runtest --instrument-with bisect_ppx --force
	bisect-ppx-report html -o ./_coverage_all --title="LIGO overall test coverage"
	bisect-ppx-report summary --per-file

install:
	cp _build/install/default/bin/ligo /usr/local/bin/ligo

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

_build/default/src/bin/js_main.bc.js: ./src/bin/js_main.ml ./src/bin/dune
	opam exec -- dune build $(<:.ml=.bc.js)


WEB_STAGING_AREA = $(TMPDIR)/ligo-ide

.PHONY: build-demo-webide demo-webide-start
build-demo-webide: node_modules/@prometheansacrifice/secp256k1-wasm/src/secp256k1.wasm node_modules/@prometheansacrifice/hacl-wasm/*.wasm tools/webide-new/ligo-webide-frontend/ligo-ide/public/favicon.ico _demo-webide_build/demo-webide.bundle.js ./node_modules/@prometheansacrifice/ocaml-bls12-381/dist/blst.wasm index.html _build/default/src/bin/js_main.bc.js
	rm -rf $(WEB_STAGING_AREA)
	mkdir $(WEB_STAGING_AREA)
	cp _build/default/src/bin/js_main.bc.runtime.js $(WEB_STAGING_AREA) # TODO(prometheansacrifice): this is only needed for dev builds
	cp $^ $(WEB_STAGING_AREA)/


demo-webide-start:
	make build-demo-webide
	python -m http.server -d $(WEB_STAGING_AREA)

