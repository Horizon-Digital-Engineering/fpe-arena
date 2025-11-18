# fpe-arena: Research-Grade FF3 Implementations

[![License](https://img.shields.io/badge/License-BUSL--1.1-purple.svg)](LICENSE)
![Status](https://img.shields.io/badge/Status-Research-orange)
[![CI](https://github.com/Horizon-Digital-Engineering/fpe-arena/workflows/CI/badge.svg)](https://github.com/Horizon-Digital-Engineering/fpe-arena/actions)

This repository contains clean-room, multi-language implementations of the NIST **FF3** format-preserving encryption (FPE) algorithm (SP 800-38G). The goal is to study FF3 internals across programming languages—for education, comparative research, and performance benchmarking—while documenting the trade-offs each language introduces. Each implementation is self-contained and can be built, tested, and inspected independently.

**NOTE:** The FF3 algorithm was withdrawn by NIST due to security vulnerabilities identified in research. Use these implementations for **research and educational purposes only**.

## About

fpe-arena is a curated reference suite for a withdrawn cipher. Every implementation starts from the same clean-room core so that differences in performance or readability stem from the language—not from mismatched logic. The repository focuses on:

- **Transparency** — full source plus test harnesses for reproducing the original NIST vectors.
- **Comparability** — identical CLI tooling, benchmarking scripts, and architecture diagrams per language.
- **Education** — extensive documentation on FF3’s structure, security history, and why it should stay out of production systems.
- **Interoperability** — shared alphabets, tweak handling, and format-preserving guarantees for apples-to-apples experiments.

If you need a modern, supported FPE primitive for production, consult a currently standardized alternative (e.g., FF1 or vendor-approved substitutes). fpe-arena exists so researchers can study FF3 responsibly.

## Security Notice

FF3 was withdrawn from NIST SP 800-38G due to security vulnerabilities identified in academic research (Beyne 2021). These implementations are intended for:

- Educational study of FF3 across languages
- Research analysis and cross-language comparison
- Architecture exploration of format-preserving encryption
- Performance benchmarking in a controlled environment

**Not intended for production use.**

## Supported Implementations

- **Ada:** Ada 2022 implementation with Alire build system
- **C++:** C++17 static library with CMake and OpenSSL
- **C# (.NET):** .NET 8.0 multi-module project with comprehensive CLI tools
- **Erlang:** Concurrent implementation with rebar3 build system
- **Go:** Pure Go implementation with standard toolchain
- **Haskell:** Pure functional implementation with Cabal/Stack
- **Java:** Java 25 multi-module Gradle project
- **JavaScript:** Node.js ES modules with npm
- **Python:** pip-installable package with pytest
- **Rust:** Memory-safe Cargo library and CLI
- **Swift:** Swift Package Manager with Apple hardware acceleration

All implementations pass 15/15 official NIST test vectors and include CLI tools.

## Architecture

All implementations follow a consistent **3-file core architecture**:

```
ff3_core      # Low-level FF3 cipher implementation
ff3_api       # String interface with factory methods
ff3_alphabets # Predefined character sets (digits, hex, base36, base62)
```

Plus **4 CLI tools** for testing and validation:
- `ff3-cli` - Encrypt/decrypt with flags
- `ff3-validate` - NIST test vector validation
- `ff3-benchmark` - Performance measurement
- `ff3-stresstest` - Randomized testing

See [architecture.md](docs/architecture.md) for details.

## Performance Benchmarks

Representative performance (AES-128, 9-character strings, radix 10):

| Implementation | ops/sec | Runtime |
|---------------|---------|---------|
| **C++** | 231,000 | GCC (C++17) |
| **Rust** (release) | 187,000 | rustc 1.85 |
| **Go** | 167,000 | go1.24.7 |
| **Haskell** | 114,000 | GHC 9.6.7 |
| **Erlang** | 61,000 | Erlang/OTP 25 |
| **.NET** (release) | 59,000 | .NET 8.0 |
| **Java** | 45,000 | Java 25 |
| **JavaScript** | 27,000 | Node.js v22 |
| **Ada** | 16,000 | GNAT |
| **Python** | 12,000 | Python 3.12 |

Results from WSL2 (AMD Ryzen 9 7945HX, 32 cores). Use `ff3-benchmark --quick` for testing.

## Format Preservation

Pure format preservation with no normalization:
- Exact input/output character preservation
- Alphabet defines valid characters
- Case-sensitive when applicable

All implementations pass 15/15 official NIST SP 800-38G test vectors.

## Quick Start

Each implementation has its own README with build instructions. Example:

```bash
cd implementations/go
go test ./...
go run cmd/ff3-cli/main.go -e "1234567890" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73
```

Docker development environment: see [DOCKER.md](DOCKER.md) for building and running the `ff3-arena` multi-language devbox, including benchmark/stress harness scripts.

Automated scripts for batch testing:

```bash
make build                # Build all implementations (skips languages without toolchains)
make test                 # Run each implementation's tests
```

## Research Applications

- **Algorithm Analysis** - FF3 structure across programming paradigms
- **Format Preservation Study** - FPE implementation without normalization
- **Cross-Language Comparison** - Identical core algorithms in different languages
- **Security Research** - Analyzing withdrawn algorithm properties (academic)
- **Educational Materials** - Teaching FPE concepts with clean examples

## Roadmap

- CI currently runs the benchmark and stress tools for every implementation, but artifact publishing/report generation is slated for the next release cycle.
- Additional stress-reporting docs and dashboards will land once the automation is wired up; follow the issue tracker for updates.

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for adding new language implementations.

## License

Business Source License 1.1 - see [LICENSE](LICENSE)

---

**Multi-language experimental repository by [Horizon Digital Engineering](https://horizondigital.dev)**
