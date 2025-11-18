# FF3 – {{LANGUAGE}} Implementation

**Security Notice**: FF3 was withdrawn by NIST because of published vulnerabilities. Use this implementation for education, research, and interoperability testing only.

---

## Quick Start
```bash
{{BUILD_CMD}}   # build or compile
{{TEST_CMD}}    # run unit/integration tests
{{CLI_HELP_CMD}}   # show CLI help/usage
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
{{CLI_RUN}} -e "890121234567890000" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73
{{CLI_RUN}} -d "750918814058654607" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73
{{CLI_RUN}} -e "HELLO" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73 -c "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
```

---

## API Example
```{{CODE_LANG}}
{{API_SNIPPET}}
```
> Demonstrate a single encrypt/decrypt round trip using the public API.

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
{{VALIDATE_CMD}} --vectors {{VECTORS_PATH}}
{{BENCHMARK_CMD}}
{{STRESS_CMD}} 1000
```
> Substitute `--quick` or iteration counts as appropriate for the language tooling.

---

## Project Layout
```
{{LAYOUT}}
```
> Summarise the three core modules (core/API/alphabets) and CLI locations.

---

## Notes
- Supports AES-128/192/256 keys (32 / 48 / 64 hex chars)
- Requires a 64-bit tweak (16 hex chars)
- Input length must satisfy FF3 constraints for the chosen radix
- All 15 NIST SP 800-38G vectors pass via the validation tool

---

## References
- NIST SP 800-38G (format-preserving encryption specification)
- AES implementation or crypto backend used in {{LANGUAGE}}
