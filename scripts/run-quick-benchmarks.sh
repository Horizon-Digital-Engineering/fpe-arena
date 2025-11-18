#!/bin/bash
# Quick test - run subset of benchmarks with --quick flag
# Uses shared config but with reduced iterations
#
# Usage: ./run-quick-benchmarks.sh [OPTIONS]
#   --config <file>     Use custom config file (default: shared/benchmarks/benchmark-config.json)
#   --output <dir>      Output directory for results (default: artifacts/)
#   --help              Show this help message

# Don't exit on error - we want to continue testing even if one fails
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
            echo "Quick test - run subset of FF3 benchmarks (5 fastest implementations)"
            echo ""
            echo "Options:"
            echo "  --config <file>     Custom config file (default: shared/benchmarks/benchmark-config.json)"
            echo "  --output <dir>      Output directory (default: artifacts/)"
            echo "  --help, -h          Show this help message"
            echo ""
            echo "Example:"
            echo "  $0 --config my-config.json --output test-results/"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            echo "Use --help for usage information"
            exit 1
            ;;
    esac
done

GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m'

echo "======================================"
echo "FF3 Quick Benchmark Test (subset)"
echo "======================================"
echo ""
echo "Config: $CONFIG_FILE"
echo "Output: $ARTIFACTS_DIR/"
echo ""

mkdir -p "$ARTIFACTS_DIR"

SUCCESS=0
FAILED=0

run_benchmark() {
    local lang="$1"
    local cmd="$2"
    local output_file="$ARTIFACTS_DIR/${lang}-bench.json"

    echo -n "Testing $lang... "

    if eval "$cmd" > "$output_file" 2>&1; then
        echo -e "${GREEN}✓${NC}"
        ((SUCCESS++))
        return 0
    else
        echo -e "${RED}✗ FAILED${NC}"
        ((FAILED++))
        return 1
    fi
}

# Test a few fast implementations
echo "Testing 5 implementations with --quick flag..."
echo ""

run_benchmark "go" \
    "$PROJECT_DIR/implementations/go/bin/ff3-bench --config $CONFIG_FILE --quick"

run_benchmark "python" \
    "cd $PROJECT_DIR/implementations/python && python3 cli/ff3_benchmark.py --config ../../shared/benchmarks/benchmark-config.json --quick"

run_benchmark "javascript" \
    "cd $PROJECT_DIR/implementations/javascript && node bin/ff3-benchmark.js --config ../../shared/benchmarks/benchmark-config.json --quick"

run_benchmark "ada" \
    "$PROJECT_DIR/implementations/ada/build/bin/ff3_benchmark --config $CONFIG_FILE --quick"

run_benchmark "rust" \
    "cd $PROJECT_DIR/implementations/rust && cargo run --release --bin ff3-benchmark -- --config ../../shared/benchmarks/benchmark-config.json --quick"

echo ""
echo "Success: $SUCCESS/5"

if [ $FAILED -eq 0 ]; then
    echo -e "${GREEN}✓ All quick tests passed!${NC}"
else
    echo -e "${RED}✗ Some tests failed${NC}"
fi
