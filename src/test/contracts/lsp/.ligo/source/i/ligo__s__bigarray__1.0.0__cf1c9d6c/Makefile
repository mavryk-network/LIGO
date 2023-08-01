ligo_compiler?=docker run --rm -v "$(PWD)":"$(PWD)" -w "$(PWD)" ligolang/ligo-ci-test:stable

.PHONY: test
test:
	@$(ligo_compiler) run test ./test/bigarray.test.mligo
