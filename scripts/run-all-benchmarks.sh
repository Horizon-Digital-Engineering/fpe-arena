#!/bin/bash
# Run all 11 language benchmark implementations with shared config
# Outputs results to artifacts/ directory
#
# Usage: ./run-all-benchmarks.sh [OPTIONS]
#   --config <file>     Use custom config file (default: shared/benchmarks/benchmark-config.json)
#   --output <dir>      Output directory for results (default: artifacts/)
#   --help              Show this help message

# Don't exit on error - continue testing all implementations
set +e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

# Default values
CONFIG_FILE="$PROJECT_DIR/shared/benchmarks/benchmark-config.json"
ARTIFACTS_DIR="$PROJECT_DIR/artifacts"

# Parse command-line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --config)
            CONFIG_FILE="$2"
            shift 2
            ;;
        --output)
            ARTIFACTS_DIR="$2"
            shift 2
            ;;
        --help|-h)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Run all 11 FF3 benchmark implementations with shared configuration"
            echo ""
            echo "Options:"
            echo "  --config <file>     Custom config file (default: shared/benchmarks/benchmark-config.json)"
            echo "  --output <dir>      Output directory (default: artifacts/)"
            echo "  --help, -h          Show this help message"
            echo ""
            echo "Example:"
            echo "  $0 --config my-config.json --output results/"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            echo "Use --help for usage information"
            exit 1
            ;;
    esac
done

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "======================================"
echo "FF3 Multi-Language Benchmark Suite"
echo "======================================"
echo ""
echo "Config: $CONFIG_FILE"
echo "Output: $ARTIFACTS_DIR/"
echo ""

# Create artifacts directory if it doesn't exist
mkdir -p "$ARTIFACTS_DIR"

# Track results
TOTAL=11
SUCCESS=0
FAILED=0

# Function to run a benchmark
run_benchmark() {
    local lang="$1"
    local cmd="$2"
    local timeout="${3:-120}"  # Default 2 minute timeout
    local output_file="$ARTIFACTS_DIR/${lang}-bench.json"

    echo -n "Running $lang benchmark... "

    if timeout "${timeout}s" bash -c "$cmd" > "$output_file" 2>/dev/null; then
        echo -e "${GREEN}✓${NC} → ${lang}-bench.json"
        ((SUCCESS++))
        return 0
    else
        local exit_code=$?
        if [ $exit_code -eq 124 ]; then
            echo -e "${YELLOW}⏱ TIMEOUT${NC}"
        else
            echo -e "${RED}✗ FAILED${NC}"
        fi
        ((FAILED++))
        return 1
    fi
}

echo "======================================"
echo "Running benchmarks..."
echo "======================================"
echo ""

# 1. Rust
run_benchmark "rust" \
    "cd $PROJECT_DIR/implementations/rust && cargo build --release --quiet --bin ff3-benchmark && ./target/release/ff3-benchmark --config $CONFIG_FILE"

# 2. Go
run_benchmark "go" \
    "$PROJECT_DIR/implementations/go/bin/ff3-bench --config $CONFIG_FILE"

# 3. .NET
run_benchmark "dotnet" \
    "cd $PROJECT_DIR/implementations/dotnet && dotnet run --project FF3.Benchmark --configuration Release -- --config $CONFIG_FILE"

# 4. Java
run_benchmark "java" \
    "cd $PROJECT_DIR/implementations/java && ./gradlew --quiet :ff3-benchmark:run --args='--config $CONFIG_FILE'"

# 5. C++
run_benchmark "cpp" \
    "$PROJECT_DIR/implementations/cpp/build/ff3_benchmark --config $CONFIG_FILE"

# 6. Python
run_benchmark "python" \
    "cd $PROJECT_DIR/implementations/python && python3 cli/ff3_benchmark.py --config $CONFIG_FILE"

# 7. JavaScript
run_benchmark "javascript" \
    "cd $PROJECT_DIR/implementations/javascript && node bin/ff3-benchmark.js --config $CONFIG_FILE"

# 8. Swift (use prebuilt release binary for speed)
run_benchmark "swift" \
    "$PROJECT_DIR/implementations/swift/.build/release/FF3Benchmark --config $CONFIG_FILE"

# 9. Haskell (with -O2 optimization)
run_benchmark "haskell" \
    "cd $PROJECT_DIR/implementations/haskell && cabal build ff3-benchmark -v0 --ghc-options='-O2' 2>&1 && cabal run ff3-benchmark -v0 --ghc-options='-O2' -- --config $CONFIG_FILE"

# 10. Erlang (production build with no_debug_info)
run_benchmark "erlang" \
    "cd $PROJECT_DIR/implementations/erlang && rebar3 as prod compile >/dev/null 2>&1 && erl -pa _build/prod/lib/*/ebin -noshell -eval 'ff3_bench_main:main([\"--config\", \"$CONFIG_FILE\"]), halt()'"

# 11. Ada/SPARK
run_benchmark "ada" \
    "$PROJECT_DIR/implementations/ada/build/bin/ff3_benchmark --config $CONFIG_FILE"

echo ""
echo "======================================"
echo "Results Summary"
echo "======================================"
echo ""
echo -e "Total:     $TOTAL"
echo -e "Success:   ${GREEN}$SUCCESS${NC}"
echo -e "Failed:    ${RED}$FAILED${NC}"
echo ""

if [ $FAILED -eq 0 ]; then
    echo -e "${GREEN}✓ All benchmarks completed successfully!${NC}"
    echo ""
    echo "Output files in $ARTIFACTS_DIR/:"
    ls -lh "$ARTIFACTS_DIR"/*.json 2>/dev/null | awk '{print "  " $9 " (" $5 ")"}'
    exit 0
else
    echo -e "${YELLOW}⚠ Some benchmarks failed. Check individual implementations.${NC}"
    exit 1
fi
