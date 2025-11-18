# FF3 – Go Implementation

**Security Notice**: FF3 was withdrawn by NIST due to published vulnerabilities. Use this implementation for education, research, and interoperability testing only.

---

## Quick Start
```bash
go test ./...
go run ./cmd/ff3-cli --help
go run ./cmd/ff3-cli -- -e "1234567890" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73
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
go run ./cmd/ff3-cli -- -e "890121234567890000" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73
go run ./cmd/ff3-cli -- -d "750918814058654607" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73
go run ./cmd/ff3-cli -- -e "HELLO" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73 -c "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
```

---

## API Example
```go
package main

import (
    "encoding/hex"
    "fmt"

    ff3 "go.horizondigital.dev/fpe-ff3"
)

func main() {
    key, _ := hex.DecodeString("EF4359D8D580AA4F7F036D6F04FC6A94")
    tweak, _ := hex.DecodeString("D8E7920AFA330A73")

    cipher, _ := ff3.Digits(key, tweak)

    ciphertext, _ := cipher.Encrypt("1234567890", nil)
    plaintext, _ := cipher.Decrypt(ciphertext, nil)

    fmt.Printf("%s -> %s -> %s\n", "1234567890", ciphertext, plaintext)
}
```
> Demonstrates a single round trip using the Go API.

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
go run ./cmd/ff3-validate -- --vectors ../../shared/test-vectors/nist_ff3_official_vectors.json
go run ./cmd/ff3-bench -- --quick
go run ./cmd/ff3-stresstest -- --count 1000
```

---

## Project Layout
```
implementations/go/
├── cmd/ff3-cli/         # CLI entry point
├── cmd/ff3-validate/    # NIST validator
├── cmd/ff3-bench/       # Benchmark tool
├── cmd/ff3-stresstest/  # Randomized stress tester
└── pkg/ff3/             # Library (core/api/alphabets and helpers)
```

---

## Notes
- Written in pure Go; no external dependencies beyond the standard library.
- Supports AES-128/192/256 keys (32/48/64 hex chars) and 64-bit tweaks (16 hex chars).
- CLI tools follow the shared flag interface and emit JSON or plain text aligned with the documentation.
- All 15 NIST SP 800-38G vectors pass via `ff3-validate`.

---

## References
- NIST SP 800-38G (Format-Preserving Encryption)
- `crypto/aes` and `crypto/cipher` packages in Go's standard library
