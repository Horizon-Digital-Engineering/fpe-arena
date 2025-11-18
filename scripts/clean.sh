#!/bin/bash

# Clean script for FF3 multi-language implementations
# Usage: ./scripts/clean.sh [language]
# Without language argument: cleans all implementations
# Must be run from project root directory

set -e

# Ensure we're in the project root
if [ ! -d "implementations" ] || [ ! -d "scripts" ]; then
    echo -e "${RED}❌ Error: This script must be run from the project root directory${NC}"
    echo "Usage: ./scripts/clean.sh [options]"
    exit 1
fi

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}🧹 FF3 Multi-Language Clean Script${NC}"
echo "=================================="

# Function to clean specific language implementation
clean_language() {
    local lang=$1
    local impl_dir="implementations/$lang"

    if [ ! -d "$impl_dir" ]; then
        echo -e "${YELLOW}⚠️  $lang implementation directory not found, skipping${NC}"
        return 0
    fi

    echo -e "${BLUE}🧹 Cleaning $lang implementation...${NC}"
    cd "$impl_dir"

    case $lang in
        "ada")
            if [ -f "alire.toml" ]; then
                alr clean 2>/dev/null || true
            fi
            rm -rf obj/ lib/ build/ alire/
            echo -e "${GREEN}✅ Ada/SPARK cleaned${NC}"
            ;;
        "cpp")
            rm -rf build/ CMakeCache.txt CMakeFiles/ cmake_install.cmake
            echo -e "${GREEN}✅ C++ cleaned${NC}"
            ;;
        "dotnet")
            if [ -f "*.csproj" ] || ls *.csproj 1> /dev/null 2>&1; then
                dotnet clean 2>/dev/null || true
            fi
            find . -type d \( -name bin -o -name obj \) -prune -exec rm -rf {} + 2>/dev/null || true
            echo -e "${GREEN}✅ .NET cleaned${NC}"
            ;;
        "erlang")
            if [ -f "Makefile" ]; then
                make clean 2>/dev/null || true
            fi
            rm -rf _build/ ebin/ *.dump
            echo -e "${GREEN}✅ Erlang cleaned${NC}"
            ;;
        "go")
            if [ -f "Makefile" ]; then
                make clean 2>/dev/null || true
            elif [ -f "go.mod" ]; then
                go clean -cache -modcache -testcache 2>/dev/null || true
                rm -f ff3-bench ff3-demo ff3-test ff3-validate ff3-stresstest main
            fi
            rm -rf bin/
            echo -e "${GREEN}✅ Go cleaned${NC}"
            ;;
        "haskell")
            if command -v stack >/dev/null 2>&1; then
                stack clean 2>/dev/null || true
            fi
            if command -v cabal >/dev/null 2>&1; then
                cabal clean 2>/dev/null || true
            fi
            rm -rf .stack-work/ dist/ dist-newstyle/
            echo -e "${GREEN}✅ Haskell cleaned${NC}"
            ;;
        "java")
            if [ -f "build.gradle" ]; then
                ./gradlew clean 2>/dev/null || true
            elif [ -f "pom.xml" ]; then
                mvn clean 2>/dev/null || true
            fi
            rm -rf build/ target/ .gradle/ */build/
            echo -e "${GREEN}✅ Java cleaned${NC}"
            ;;
        "javascript")
            if [ -f "package.json" ]; then
                npm cache clean --force 2>/dev/null || true
            fi
            rm -rf node_modules/ dist/ build/
            echo -e "${GREEN}✅ JavaScript cleaned${NC}"
            ;;
        "python")
            find . -name "*.pyc" -delete 2>/dev/null || true
            find . -name "__pycache__" -type d -exec rm -rf {} + 2>/dev/null || true
            rm -rf build/ dist/ *.egg-info/ .pytest_cache/
            echo -e "${GREEN}✅ Python cleaned${NC}"
            ;;
        "rust")
            if [ -f "Cargo.toml" ]; then
                cargo clean 2>/dev/null || true
            fi
            rm -rf target/
            echo -e "${GREEN}✅ Rust cleaned${NC}"
            ;;
        "swift")
            if [ -f "Package.swift" ]; then
                swift package clean 2>/dev/null || true
            fi
            rm -rf .build/
            echo -e "${GREEN}✅ Swift cleaned${NC}"
            ;;
        *)
            echo -e "${RED}❌ Unknown language: $lang${NC}"
            return 1
            ;;
    esac

    cd - > /dev/null
}

# List all available implementations
list_implementations() {
    echo -e "${BLUE}📋 Available implementations:${NC}"
    for impl in implementations/*/; do
        if [ -d "$impl" ]; then
            basename "$impl"
        fi
    done | sort
}

# Clean all implementations
clean_all() {
    echo -e "${BLUE}🧹 Cleaning all implementations...${NC}"
    echo ""

    for impl in implementations/*/; do
        if [ -d "$impl" ]; then
            lang=$(basename "$impl")
            clean_language "$lang"
        fi
    done

    # Clean shared build artifacts
    echo ""
    echo -e "${BLUE}🧹 Cleaning shared artifacts...${NC}"
    rm -rf shared/benchmarks/validation-results/ artifacts/ || true
    echo -e "${GREEN}✅ Shared artifacts cleaned${NC}"
}

# Main execution
if [ $# -eq 0 ]; then
    # No arguments - clean all
    clean_all
elif [ "$1" = "--list" ] || [ "$1" = "-l" ]; then
    # List available implementations
    list_implementations
elif [ "$1" = "--help" ] || [ "$1" = "-h" ]; then
    # Show help
    echo "Usage: $0 [language|--list|--help]"
    echo ""
    echo "Clean build artifacts for FF3 implementations"
    echo ""
    echo "Options:"
    echo "  (no args)    Clean all implementations"
    echo "  language     Clean specific language implementation"
    echo "  --list, -l   List available implementations"
    echo "  --help, -h   Show this help message"
    echo ""
    echo "Examples:"
    echo "  $0              # Clean all implementations"
    echo "  $0 rust         # Clean only Rust implementation"
    echo "  $0 --list       # List available implementations"
else
    # Clean specific language
    lang="$1"
    clean_language "$lang"
fi

echo ""
echo -e "${GREEN}✅ Clean operation completed${NC}"
