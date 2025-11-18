# Root Makefile for multi-language FF3 repo
# Usage examples:
#   make list             # show what will run
#   make build            # build everything available
#   make test             # test everything available
#   make haskell.test     # test just Haskell
#   make -j test          # parallelize across languages

SHELL := /usr/bin/env bash
ROOT  := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))

# Helpers
define if_tool
@if command -v $(1) >/dev/null 2>&1; then \
  echo "→ $(2)"; \
  (set -e; $(3)); \
else \
  echo "↷ skip: $(1) not found"; \
fi
endef

.PHONY: list build test clean ci

list:
	@echo "Targets that will run on this machine:"
	@command -v go       >/dev/null && echo "  • go"       || true
	@command -v stack    >/dev/null && echo "  • haskell"  || true
	@command -v cargo    >/dev/null && echo "  • rust"     || true
	@command -v python   >/dev/null && echo "  • python"   || true
	@command -v ./implementations/java/gradlew >/dev/null && echo "  • java (gradle)" || true
	@command -v dotnet   >/dev/null && echo "  • dotnet"   || true
	@command -v cmake    >/dev/null && echo "  • cpp"      || true
	@command -v erlc     >/dev/null && echo "  • erlang"   || true
	@command -v npm      >/dev/null && echo "  • javascript" || true

# Meta targets
build: go.build haskell.build rust.build python.build java.build dotnet.build cpp.build erlang.build js.build
test:  go.test  haskell.test  rust.test  python.test  java.test  dotnet.test  cpp.test  erlang.test  js.test
clean: go.clean haskell.clean rust.clean python.clean java.clean dotnet.clean cpp.clean erlang.clean js.clean
ci:    build test

##### Go
go.build:
	$(call if_tool,go,go build,\
	cd implementations/go && go build ./... )
go.test:
	$(call if_tool,go,go test,\
	cd implementations/go && go test ./... )
go.clean:
	$(call if_tool,go,go clean,\
	cd implementations/go && make clean )

##### Haskell (Stack)
haskell.build:
	$(call if_tool,stack,haskell build,\
	cd implementations/haskell && stack build )
haskell.test:
	$(call if_tool,stack,haskell test,\
	cd implementations/haskell && stack test )
haskell.clean:
	$(call if_tool,stack,haskell clean,\
	cd implementations/haskell && stack clean )

##### Rust (Cargo)
rust.build:
	$(call if_tool,cargo,rust build,\
	cd implementations/rust && cargo build )
rust.test:
	$(call if_tool,cargo,rust test,\
	cd implementations/rust && cargo test )
rust.clean:
	$(call if_tool,cargo,rust clean,\
	cd implementations/rust && cargo clean )

##### Python (setup.py + simple test)
python.build:
	$(call if_tool,python,python build,\
	cd implementations/python && python -m pip install -e . )
python.test:
	$(call if_tool,python,python test,\
	cd implementations/python && python -m pytest )
python.clean:
	$(call if_tool,python,python clean,\
	cd implementations/python && python - <<'PY'\nimport shutil, pathlib\nfor p in ['build','dist','*.egg-info','__pycache__']:\n  for x in pathlib.Path('.').glob(p): shutil.rmtree(x, ignore_errors=True)\nPY)

##### Java (Gradle wrapper provided)
java.build:
	$(call if_tool,./implementations/java/gradlew,java build,\
	cd implementations/java && ./gradlew --no-daemon assemble )
java.test:
	$(call if_tool,./implementations/java/gradlew,java test,\
	cd implementations/java && ./gradlew --no-daemon test )
java.clean:
	$(call if_tool,./implementations/java/gradlew,java clean,\
	cd implementations/java && ./gradlew --no-daemon clean )

##### .NET
dotnet.build:
	$(call if_tool,dotnet,dotnet build,\
	cd implementations/dotnet && dotnet build -c Release )
dotnet.test:
	$(call if_tool,dotnet,dotnet test,\
	cd implementations/dotnet && dotnet test -c Release )
dotnet.clean:
	$(call if_tool,dotnet,dotnet clean,\
	cd implementations/dotnet && dotnet clean )

##### C++ (CMake)
cpp.build:
	$(call if_tool,cmake,cpp build,\
	mkdir -p implementations/cpp/build && cd implementations/cpp/build && cmake .. && cmake --build . )
cpp.test:
	$(call if_tool,cmake,cpp test,\
	[ -d implementations/cpp/build ] && cd implementations/cpp/build && ctest --output-on-failure || echo "↷ skip: no cpp/build" )
cpp.clean:
	$(call if_tool,cmake,cpp clean,\
	rm -rf implementations/cpp/build )

##### Erlang
erlang.build:
	$(call if_tool,erlc,erlang build,\
	cd implementations/erlang && make )
erlang.test:
	$(call if_tool,erlc,erlang test,\
	cd implementations/erlang && make test )
erlang.clean:
	$(call if_tool,erlc,erlang clean,\
	cd implementations/erlang && make clean )

##### JavaScript (Node)
js.build:
	$(call if_tool,npm,js build,\
	cd implementations/javascript && npm ci )
js.test:
	$(call if_tool,npm,js test,\
	cd implementations/javascript && npm test )
js.clean:
	$(call if_tool,npm,js clean,\
	cd implementations/javascript && rm -rf node_modules )
