# FF3 Multi-Language Implementation Architecture

This document provides an overview of the architectural design for the fpe-arena project, a multi-language implementation of FF3 Format Preserving Encryption for educational and research purposes.

## Project Goals

- **Educational**: Demonstrate FF3 implementation across different programming paradigms
- **Research**: Enable cross-language performance and behavior analysis
- **Consistency**: Maintain identical behavior and API across all 11 language implementations
- **Compliance**: All implementations pass the same 15 NIST SP 800-38G test vectors

**Security Notice**: FF3 was withdrawn by NIST due to security vulnerabilities. These implementations are for educational and research purposes only, not for production use.

## Core Library Architecture

Every language implementation follows a standardized **3-file architecture** pattern:

### 1. Core Module (`ff3_core.*`)
- **Purpose**: Pure cryptographic implementation of the FF3 algorithm
- **Responsibilities**:
  - 8-round Feistel network implementation
  - AES-ECB encryption integration
  - Proper NIST byte reversal (REVB operations)
  - Big integer arithmetic for format preservation
  - Modular arithmetic operations
- **Key Functions**: `encrypt(digits)`, `decrypt(digits)`
- **No Dependencies**: Should not depend on string/alphabet handling

### 2. API Module (`ff3_api.*`)
- **Purpose**: High-level string-based interface
- **Responsibilities**:
  - String <-> digit array conversion
  - Alphabet character mapping
  - Factory methods for common alphabets
  - Input validation and error handling
- **Key Functions**: `encrypt(string)`, `decrypt(string)`, alphabet factories
- **Depends On**: Core module + Alphabets module

### 3. Alphabets Module (`ff3_alphabets.*`)
- **Purpose**: Character set definitions and specifications
- **Responsibilities**:
  - Define standard alphabet constants (DIGITS, HEX_LOWER, HEX_UPPER, BASE36, BASE62, etc.)
  - Character-to-index mapping
  - Radix validation
- **Key Data**: Alphabet string constants and radix values
- **No Dependencies**: Pure data/constants module

### Architecture Benefits
- **Separation of Concerns**: Crypto logic isolated from string handling
- **Testability**: Each module can be tested independently
- **Reusability**: Core can work with any radix/alphabet combination
- **Consistency**: Same structure across all 11 languages makes cross-language comparison easier

## CLI Tool Suite

Each language implementation provides **4 standardized command-line tools**:

### 1. FF3-CLI - Encryption/Decryption Tool

**Purpose**: Command-line utility for encrypting and decrypting text using FF3.

**Key Requirements**:
- **Flags-only interface** (NO interactive mode)
- Standard flags: `-e`, `-d`, `-k`, `-t`, `-a`, `-c`, `-h`
- Output to stdout, errors to stderr
- Exit codes: 0 (success), 1 (error)

**Detailed Specification**: [CLI Tool Architecture](cli-architecture.md)

### 2. FF3-Validate - NIST Test Vector Validator

**Purpose**: Validates implementation correctness against all 15 official NIST SP 800-38G test vectors.

**Key Requirements**:
- Load JSON test vectors from standard location
- Test encrypt, decrypt, and round-trip for each vector
- Report pass/fail for each test case
- Exit code 0 if all pass, 1 if any fail

**Detailed Specification**: [Validation Tool Architecture](validation-architecture.md)

### 3. FF3-Benchmark - Performance Testing Tool

**Purpose**: Measures encrypt/decrypt/round-trip performance across different alphabets and input lengths.

**Key Requirements**:
- Standardized JSON output format (identical across all languages)
- Test multiple cases: encrypt, decrypt, round-trip
- Test multiple lengths: 9, 12, 16, 20 characters
- Warmup phase before measurement
- Report operations per second and nanoseconds per operation

**Detailed Specification**: [Benchmark CLI Architecture](benchmark-architecture.md)

### 4. FF3-StressTest - Randomized Testing Tool

**Purpose**: Perform high-volume randomized round-trip tests to discover edge cases and verify stability.

**Key Requirements**:
- Configurable iteration count
- Multiple alphabet testing
- Deterministic PRNG for reproducibility
- Collision detection
- Exit code 0 if no failures, 1 if any failure

**Detailed Specification**: [StressTest Tool Architecture](stresstest-architecture.md)

## Common Design Principles

All tools across all languages must follow these principles:

### 1. **No Interactive Modes**
- All tools are driven entirely by command-line flags
- No prompts, no REPL, no interactive shells
- Enables scriptability and automation

### 2. **Consistent CLI Interface**
- Same flag names across all tools and languages
- Same output formats (especially JSON for benchmarks)
- Same exit code meanings
- Help text accessible via `-h` or `--help`

### 3. **Minimal Dependencies**
- Target: ~200-300 lines per tool
- Use language standard library where possible
- Only essential external dependencies (crypto libraries, JSON parsers)

### 4. **Format Preservation**
- No normalization of input strings
- Exact case preservation
- Character-for-character format matching

### 5. **Cross-Language Consistency**
- Same behavior for same inputs across all 11 implementations
- Identical JSON schemas for structured output
- Same alphabet definitions and naming conventions

## Language Implementations

The project currently supports **11 programming languages**, demonstrating FF3 across different paradigms:

### Systems & Enterprise Languages
- **C++**: High-performance native code with OpenSSL BIGNUM
- **.NET (C#)**: Cross-platform enterprise architecture
- **Go**: Clean, concurrent, crypto-focused
- **Java**: Enterprise-grade with Gradle multi-module build
- **Rust**: Memory-safe systems programming

### Functional & Academic Languages
- **Haskell**: Pure functional programming with Hspec testing
- **Ada**: Formal verification ready (formerly SPARK)

### Dynamic & Scripting Languages
- **Python**: Educational clarity with pytest
- **JavaScript**: Modern Node.js with ES modules

### Mobile & Apple Ecosystem
- **Swift**: Swift Package Manager with hardware-accelerated AES

### Experimental
- **Erlang**: Actor model exploration

Each implementation must maintain **feature parity** with others:
- Same 3-file core architecture
- Same 4 CLI tools
- Pass all 15 NIST test vectors
- Follow same architecture specifications

## Testing & Validation

### NIST Compliance (Required)
All implementations must pass **100% of NIST test vectors** (15/15):
- 5 vectors with AES-128 (radix 10, 26)
- 5 vectors with AES-192 (radix 10, 26)
- 5 vectors with AES-256 (radix 10, 26)

### Cross-Language Verification
- Encrypt in one language, decrypt in another → must match
- Same inputs produce same outputs across all implementations
- Benchmark results should be comparable (accounting for language performance differences)

### Edge Case Testing
- Minimum/maximum length inputs
- All supported radix values (2-62)
- Different AES key sizes (128, 192, 256 bits)
- Round-trip correctness (decrypt(encrypt(x)) == x)

## Supported Alphabets

All implementations must support these standard alphabets:

| Alphabet | Character Set | Radix |
|----------|---------------|-------|
| Digits | `0123456789` | 10 |
| Hex Lower | `0123456789abcdef` | 16 |
| Hex Upper | `0123456789ABCDEF` | 16 |
| Base36 Lower | `0123456789abcdefghijklmnopqrstuvwxyz` | 36 |
| Base36 Upper | `0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ` | 36 |
| Base62 | `0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz` | 62 |
| Radix26* | `0123456789abcdefghijklmnop` | 26 |
| Custom | User-provided character set | 2-62 |

*Note: Radix26 is optional and implemented in some languages for NIST test vector compatibility.

## Adding a New Language Implementation

See [CONTRIBUTING.md](../CONTRIBUTING.md) for detailed guidance on:
1. Following the 3-file core architecture
2. Implementing all 4 CLI tools per specifications
3. Passing NIST test vectors
4. Maintaining consistency with existing implementations
5. Documentation and testing requirements

## References

- [NIST SP 800-38G](https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-38G.pdf) - FF3 Specification (Draft)
- [FF3 Vulnerability Analysis](https://eprint.iacr.org/2017/521) - Durak & Vaudenay, 2017
- [Format Preserving Encryption](https://en.wikipedia.org/wiki/Format-preserving_encryption) - Wikipedia
