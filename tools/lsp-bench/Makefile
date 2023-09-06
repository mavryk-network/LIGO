# examples: make bench filter="Simple references" ; make bench filter="Complex benchmarks/one_big_file references edits"
# You can bench other executable, e.g. LIGO_LSP_TEST_EXE="ligo-lsp-exe" make bench

bench:
	git submodule update --init --recursive
	$(BUILD_FLAGS) stack run -- "-v2" "$(filter)"

bench-no-logging:
	LIGO_LSP_TEST_EXE="ligo lsp --disable-lsp-requests-logging" make bench
