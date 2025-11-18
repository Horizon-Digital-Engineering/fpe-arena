# FF3 Format Preserving Encryption - Erlang Umbrella Implementation
# OTP-style umbrella application with rebar3 build system

REBAR = rebar3

.PHONY: all compile clean test ct eunit dialyzer cover docs shell escripts help

# Default target
all: compile

# Compile all applications (library + CLIs)
compile:
	$(REBAR) compile

# Clean build artifacts
clean:
	$(REBAR) clean
	rm -rf _build logs

# Run library tests (Common Test suite)
test: ct

# Run Common Test suite (library only)
ct:
	$(REBAR) ct --verbose

# Run unit tests (if any eunit tests exist)
eunit:
	$(REBAR) eunit

# Run dialyzer (static analysis)
dialyzer:
	$(REBAR) dialyzer

# Generate coverage reports (library only)
cover:
	$(REBAR) cover --verbose

# Generate documentation
docs:
	$(REBAR) edoc

# Start Erlang shell with library loaded
shell:
	$(REBAR) shell

# Build all CLI escripts
escripts: compile
	$(REBAR) escriptize -a ff3_cli
	$(REBAR) escriptize -a ff3_validate
	$(REBAR) escriptize -a ff3_bench
	$(REBAR) escriptize -a ff3_stresstest

# Test individual CLI apps
test-cli: escripts
	@echo "🧪 Testing CLI applications..."
	@echo "Testing ff3_cli (self-test)..."
	@echo "test" | ./_build/default/bin/ff3_cli || true
	@echo
	@echo "Testing ff3_validate..."
	@./_build/default/bin/ff3_validate
	@echo
	@echo "Testing ff3_bench..."
	@./_build/default/bin/ff3_bench
	@echo
	@echo "Testing ff3_stresstest..."
	@./_build/default/bin/ff3_stresstest

# Run old-style test for compatibility (using library API)
legacy-test: compile
	@echo "🧪 Running legacy compatibility test..."
	@erl -pa _build/default/lib/ff3/ebin -noshell -eval "
		Key = ff3_api:hex_to_bytes(\"EF4359D8D580AA4F7F036D6F04FC6A94\"),
		Tweak = ff3_api:hex_to_bytes(\"D8E7920AFA330A73\"),
		Alphabet = ff3_alphabets:digits_alphabet(),
		Plaintext = \"890121234567890000\",
		Expected = \"750918814058654607\",
		Result = ff3_api:encrypt(Plaintext, Key, Tweak, Alphabet),
		Decrypted = ff3_api:decrypt(Result, Key, Tweak, Alphabet),
		case {Result, Decrypted} of
			{Expected, Plaintext} ->
				io:format(\"✅ Legacy test passed!~n\");
			_ ->
				io:format(\"❌ Legacy test failed!~n\"),
				io:format(\"Expected: ~s, Got: ~s~n\", [Expected, Result])
		end.
	" -s init stop

# Build release
release:
	$(REBAR) release

# Check formatting
format:
	$(REBAR) fmt --check

# Format code
fmt:
	$(REBAR) fmt --write

# Complete test suite with coverage
check: clean compile ct dialyzer cover

# Full integration test (library + all CLIs)
integration-test: check escripts test-cli

# Help target
help:
	@echo "Available targets:"
	@echo "  compile       - Compile all applications (library + CLIs)"
	@echo "  escripts      - Build all CLI escripts"
	@echo "  test/ct       - Run Common Test suite (library only)"
	@echo "  test-cli      - Test all CLI applications"
	@echo "  eunit         - Run EUnit tests"
	@echo "  dialyzer      - Run static analysis"
	@echo "  cover         - Generate coverage reports (library only)"
	@echo "  docs          - Generate documentation"
	@echo "  shell         - Start Erlang shell with library loaded"
	@echo "  legacy-test   - Run old-style test for compatibility"
	@echo "  clean         - Clean build artifacts"
	@echo "  check         - Full library test suite (compile + ct + dialyzer + cover)"
	@echo "  integration-test - Full integration test (library + all CLIs)"
	@echo "  help          - Show this help"