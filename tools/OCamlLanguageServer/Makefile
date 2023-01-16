all: run

build: bin/main.ml
	dune build

install: build ../../_build/default/tools/OCamlLanguageServer/bin/main.exe
	cd ../lsp/vscode-plugin/ && \
	cp ../../../_build/default/tools/OCamlLanguageServer/bin/main.exe bin/ligo-squirrel && \
	yarn package && \
	code --install-extension ligo-vscode-0.4.28.vsix --force

run: install
	code
