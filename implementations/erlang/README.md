# FF3 – Erlang Implementation

**Security Notice**: FF3 was withdrawn by NIST due to published vulnerabilities. Use this implementation for education, research, and interoperability testing only.

---

## Quick Start
```bash
make compile
make test
./_build/default/bin/ff3_cli --help
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
./_build/default/bin/ff3_cli -e "890121234567890000" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73
./_build/default/bin/ff3_cli -d "750918814058654607" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73
./_build/default/bin/ff3_cli -e "HELLO" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73 -c "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
```

---

## API Example
```erlang
{ok, Key} = ff3_api:hex_to_bytes("EF4359D8D580AA4F7F036D6F04FC6A94"),
{ok, Tweak} = ff3_api:hex_to_bytes("D8E7920AFA330A73"),
{ok, Cipher} = ff3_api:create_digits_cipher(Key, Tweak),
{ok, Ciphertext} = ff3_api:encrypt_string(Cipher, "1234567890"),
{ok, Plaintext} = ff3_api:decrypt_string(Cipher, Ciphertext).
```
> Demonstrates a single round trip using the OTP-style API.

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
./_build/default/bin/ff3_validate --vectors ../../shared/test-vectors/nist_ff3_official_vectors.json
./_build/default/bin/ff3_bench --quick
./_build/default/bin/ff3_stresstest 1000
```

---

## Project Layout
```
implementations/erlang/
├── rebar.config        # Umbrella configuration
├── apps/ff3/           # Core library (core/api/alphabets)
├── apps/ff3_cli/       # CLI wrapper
├── apps/ff3_validate/  # NIST validation tool
├── apps/ff3_bench/     # Benchmark tool
└── apps/ff3_stresstest/ # Randomized stress tester
```

---

## Notes
- Requires Erlang/OTP 21+ (with the `crypto` application) and optionally rebar3.
- Supports AES-128/192/256 keys (32/48/64 hex chars) and 64-bit tweaks (16 hex chars).
- CLI tools run as escripts inside `_build/default/bin`.
- All 15 NIST SP 800-38G vectors pass via `ff3_validate`.

---

## References
- NIST SP 800-38G (Format-Preserving Encryption)
- Erlang/OTP `crypto` application for AES-ECB
