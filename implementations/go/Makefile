# Go FF3 Implementation Makefile

# Build directories
BINDIR = bin
ARTIFACTDIR = ../../artifacts

# Ensure directories exist
$(BINDIR):
	mkdir -p $(BINDIR)

$(ARTIFACTDIR):
	mkdir -p $(ARTIFACTDIR)

# Build targets
.PHONY: build build-validate build-stress build-bench

build: build-validate build-stress build-bench

build-validate: $(BINDIR)
	go build -o $(BINDIR)/ff3-validate ./cmd/ff3-validate

build-stress: $(BINDIR)
	go build -o $(BINDIR)/ff3-stresstest ./cmd/ff3-stresstest

build-bench: $(BINDIR)
	go build -o $(BINDIR)/ff3-bench ./cmd/ff3-bench

# Test targets
.PHONY: test validate stress bench

test:
	go test -v ./pkg/ff3

validate: build-validate
	./$(BINDIR)/ff3-validate --vectors ../../shared/test-vectors/nist_ff3_official_vectors.json

stress: build-stress $(ARTIFACTDIR)
	./$(BINDIR)/ff3-stresstest \
		--count 100000 \
		--lengths 9,12,16 \
		--alphabet digits \
		--radix 10 \
		--key 000102030405060708090A0B0C0D0E0F \
		--tweak 00010203040506A1 \
		--json-out $(ARTIFACTDIR)/go-stresstest.json

bench: build-bench $(ARTIFACTDIR)
	./$(BINDIR)/ff3-bench \
		--alphabet digits \
		--radix 10 \
		--lengths 9,12,16 \
		--cases enc,dec,roundtrip \
		--n 20000 \
		--warmup 2000 \
		--key 000102030405060708090A0B0C0D0E0F \
		--tweak 00010203040506A1 \
		--json-out $(ARTIFACTDIR)/go-bench.json

# Extended test targets
.PHONY: stress-large bench-quick validate-verbose

stress-large: build-stress $(ARTIFACTDIR)
	./$(BINDIR)/ff3-stresstest \
		--count 1000000 \
		--lengths 9,12,16,20 \
		--alphabet base62 \
		--radix 62 \
		--key 000102030405060708090A0B0C0D0E0F \
		--tweak 00010203040506A1 \
		--json-out $(ARTIFACTDIR)/go-stresstest-large.json

bench-quick: build-bench $(ARTIFACTDIR)
	./$(BINDIR)/ff3-bench \
		--alphabet digits \
		--radix 10 \
		--lengths 9,16 \
		--cases enc,roundtrip \
		--quick \
		--key 000102030405060708090A0B0C0D0E0F \
		--tweak 00010203040506A1 \
		--json-out $(ARTIFACTDIR)/go-bench-quick.json

validate-verbose: build-validate
	./$(BINDIR)/ff3-validate \
		--vectors ../../shared/test-vectors/nist_ff3_official_vectors.json \
		--verbose \
		--json-out $(ARTIFACTDIR)/go-validate.json

# Multi-alphabet benchmarking
.PHONY: bench-all-alphabets

bench-all-alphabets: build-bench $(ARTIFACTDIR)
	@echo "Benchmarking digits (radix 10)..."
	./$(BINDIR)/ff3-bench --alphabet digits --radix 10 --lengths 9,12,16 --cases enc,dec,roundtrip --n 20000 --json-out $(ARTIFACTDIR)/go-bench-digits.json
	@echo "Benchmarking hex (radix 16)..."
	./$(BINDIR)/ff3-bench --alphabet hex --radix 16 --lengths 9,12,16 --cases enc,dec,roundtrip --n 20000 --json-out $(ARTIFACTDIR)/go-bench-hex.json
	@echo "Benchmarking base36 (radix 36)..."
	./$(BINDIR)/ff3-bench --alphabet base36 --radix 36 --lengths 9,12,16 --cases enc,dec,roundtrip --n 20000 --json-out $(ARTIFACTDIR)/go-bench-base36.json
	@echo "Benchmarking base62 (radix 62)..."
	./$(BINDIR)/ff3-bench --alphabet base62 --radix 62 --lengths 9,12,16 --cases enc,dec,roundtrip --n 20000 --json-out $(ARTIFACTDIR)/go-bench-base62.json

# Clean targets
.PHONY: clean clean-bins clean-artifacts

clean: clean-bins

clean-bins:
	rm -rf $(BINDIR)
	rm -f ff3-validate ff3-stresstest ff3-bench

clean-artifacts:
	rm -f $(ARTIFACTDIR)/go-*.json

# Help target
.PHONY: help

help:
	@echo "Available targets:"
	@echo "  build              - Build all CLI tools"
	@echo "  build-validate     - Build NIST vector validation tool"
	@echo "  build-stress       - Build stress test tool"
	@echo "  build-bench        - Build benchmark tool"
	@echo ""
	@echo "  test               - Run Go unit tests"
	@echo "  validate           - Run NIST vector validation"
	@echo "  stress             - Run basic stress test (100k ops)"
	@echo "  bench              - Run basic benchmark"
	@echo ""
	@echo "  stress-large       - Run large stress test (1M ops)"
	@echo "  bench-quick        - Run quick benchmark"
	@echo "  validate-verbose   - Run validation with verbose output"
	@echo "  bench-all-alphabets - Benchmark all alphabet types"
	@echo ""
	@echo "  clean              - Clean build artifacts"
	@echo "  clean-artifacts    - Clean benchmark/test output files"