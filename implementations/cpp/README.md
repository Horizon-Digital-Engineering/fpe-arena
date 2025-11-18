# FF3 – C++ Implementation

**Security Notice**: FF3 was withdrawn by NIST due to published vulnerabilities. Use this implementation for education, research, and interoperability testing only.

---

## Quick Start
```bash
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build
./build/ff3_cli --help
```

---

## CLI Usage

| Flag | Description |
|------|-------------|
| `-e, --encrypt` | Plaintext to encrypt |
| `-d, --decrypt` | Ciphertext to decrypt |
| `-k, --key`     | AES key in hex (32 / 48 / 64 chars) |
| `-t, --tweak`   | Tweak in hex (16 chars) |
| `-a, --alphabet`| digits, hex-lower, hex-upper, base36-lower, base36-upper, base62 |
| `-c, --custom`  | Custom alphabet string |
| `-h, --help`    | Show usage information |

Examples:
```bash
./build/ff3_cli -e "890121234567890000" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73
./build/ff3_cli -d "750918814058654607" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73
./build/ff3_cli -e "HELLO" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73 -c "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
```

---

## API Example
```cpp
#include <ff3.hpp>

const auto key   = ff3::hex_to_bytes("EF4359D8D580AA4F7F036D6F04FC6A94");
const auto tweak = ff3::hex_to_bytes("D8E7920AFA330A73");

auto cipher = ff3::create_digits_cipher(key, tweak);

const std::string ciphertext = cipher.encrypt("1234567890");
const std::string plaintext  = cipher.decrypt(ciphertext);
```
> Demonstrates a single round trip using the high-level FF3 string API.

---

## Supported Alphabets

| Name | Characters | Radix |
|------|------------|-------|
| `digits` | `0123456789` | 10 |
| `hex_lower` | `0123456789abcdef` | 16 |
| `hex_upper` | `0123456789ABCDEF` | 16 |
| `base36_lower` | `0123456789abcdefghijklmnopqrstuvwxyz` | 36 |
| `base36_upper` | `0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ` | 36 |
| `base62` | `0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz` | 62 |
| `custom` | User-supplied string | 2–62 |

---

## Validation & Benchmark
```bash
./build/ff3_validate --vectors ../../shared/test-vectors/nist_ff3_official_vectors.json
./build/ff3_benchmark --quick
./build/ff3_stresstest -n 1000
```

---

## Project Layout
```
implementations/cpp/
├── include/ff3/        # Public headers (core/api/alphabets)
├── src/                # Library sources
├── apps/               # CLI executables (cli/validate/benchmark/stresstest)
├── tests/              # Unit and regression tests
└── CMakeLists.txt      # Build configuration
```

---

## Notes
- Requires a C++17 compiler, CMake ≥ 3.20, and OpenSSL development headers.
- Supports AES-128/192/256 keys (32/48/64 hex chars) and 64-bit tweaks (16 hex chars).
- CLI binaries follow the shared flag interface and emit JSON or plain text aligned with the documentation.
- All 15 NIST SP 800-38G vectors pass via `ff3_validate`.

---

## References
- NIST SP 800-38G (Format-Preserving Encryption)
- OpenSSL BIGNUM and EVP APIs (AES-ECB backend)
