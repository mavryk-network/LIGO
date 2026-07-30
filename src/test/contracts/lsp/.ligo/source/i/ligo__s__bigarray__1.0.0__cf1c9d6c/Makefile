ligo_compiler?=docker run --rm -v "$(PWD)":"$(PWD)" -w "$(PWD)" mavrykdynamics/ligo:stable

.PHONY: test
test:
	@$(ligo_compiler) run test ./test/bigarray.test.mligo
