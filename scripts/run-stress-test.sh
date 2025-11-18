#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <language> [-- stress-test-args...]" >&2
  echo "Supported languages: ada, cpp, dotnet, erlang, go, haskell, java, javascript, python, rust, swift" >&2
  exit 1
fi

lang=$1
shift || true
args="$*"

case "$lang" in
  ada)
    (cd implementations/ada && alr build >/dev/null && alr run ff3_stresstest -- "$@")
    ;;
  cpp)
    (cd implementations/cpp && cmake -S . -B build >/dev/null && cmake --build build >/dev/null && ./build/ff3_stresstest "$@")
    ;;
  dotnet)
    (cd implementations/dotnet && dotnet run --project FF3.StressTest -- "$@")
    ;;
  erlang)
    (cd implementations/erlang && rebar3 escriptize >/dev/null && _build/default/bin/ff3_stresstest "$@")
    ;;
  go)
    (cd implementations/go && go run ./cmd/ff3-stresstest "$@")
    ;;
  haskell)
    (cd implementations/haskell && stack run ff3-stresstest -- "$@")
    ;;
  java)
    (cd implementations/java && { if [[ $# -gt 0 ]]; then ./gradlew :ff3-stresstest:run --args="$args"; else ./gradlew :ff3-stresstest:run; fi; })
    ;;
  javascript)
    (cd implementations/javascript && node bin/ff3-stresstest.js "$@")
    ;;
  python)
    (cd implementations/python && python3 cli/ff3_stresstest.py "$@")
    ;;
  rust)
    (cd implementations/rust && cargo run --bin ff3-stresstest -- "$@")
    ;;
  swift)
    (cd implementations/swift && swift run FF3StressTest -- "$@")
    ;;
  
smoke)
    for lang in ada cpp dotnet erlang go haskell java javascript python rust swift; do
      echo "==> $lang"
      case "$lang" in
        ada)        ./scripts/run-stress-test.sh ada --quick --seed 1 ;;
        cpp)        ./scripts/run-stress-test.sh cpp --quick --seed 1 ;;
        dotnet)     ./scripts/run-stress-test.sh dotnet --quick --seed 1 ;;
        erlang)     ./scripts/run-stress-test.sh erlang --quick --seed 1 ;;
        go)         ./scripts/run-stress-test.sh go --quick --seed 1 ;;
        haskell)    ./scripts/run-stress-test.sh haskell --quick --seed 1 ;;
        java)       ./scripts/run-stress-test.sh java --quick --seed 1 ;;
        javascript) ./scripts/run-stress-test.sh javascript --quick --seed 1 ;;
        python)     ./scripts/run-stress-test.sh python --quick --seed 1 ;;
        rust)       ./scripts/run-stress-test.sh rust --quick --seed 1 ;;
        swift)      ./scripts/run-stress-test.sh swift --quick --seed 1 ;;
      esac || exit 1
    done
    ;;
  *)
    echo "Unsupported language: $lang" >&2
    exit 1
    ;;
esac
