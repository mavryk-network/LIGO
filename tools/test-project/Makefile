all: run

build: TestProject.ml
	dune build

install: build ../../_build/default/tools/test-project/TestProject.exe
	cd ../lsp/vscode-plugin/ && \
	cp ../../../_build/default/tools/test-project/TestProject.exe bin/ligo-squirrel && \
	yarn package && \
	code --install-extension ligo-vscode-0.4.27.vsix --force

run: install
	code
