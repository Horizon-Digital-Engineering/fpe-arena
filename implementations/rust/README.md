# FF3 – Rust Implementation

**Security Notice**: FF3 was withdrawn by NIST due to published vulnerabilities. Use this implementation for education, research, and interoperability testing only.

---

## Quick Start
```bash
cargo build
cargo test
cargo run -- --help
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
cargo run -- -e "890121234567890000" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73
cargo run -- -d "750918814058654607" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73
cargo run -- -e "HELLO" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73 -c ABCDEFGHIJKLMNOPQRSTUVWXYZ
```

---

## API Example
```rust
use fpe_ff3::digits;

let key = hex::decode("EF4359D8D580AA4F7F036D6F04FC6A94")?;
let tweak = hex::decode("D8E7920AFA330A73")?;

let cipher = digits(&key, &tweak)?;
let ciphertext = cipher.encrypt("1234567890", None)?;
let plaintext = cipher.decrypt(&ciphertext, None)?;
```
> Demonstrates a single round trip using the Rust crate.

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
cargo run --bin ff3-validate -- --vectors ../../shared/test-vectors/nist_ff3_official_vectors.json
cargo run --bin ff3-benchmark -- --quick
cargo run --bin ff3-stresstest -- 1000
```

---

## Project Layout
```
implementations/rust/
├── src/core.rs       # Core cipher implementation
├── src/api.rs        # High-level string API
├── src/alphabets.rs  # Alphabet definitions
├── src/main.rs       # CLI entry point
├── src/bin/          # Additional CLI binaries
└── tests/            # Integration tests
```

---

## Notes
- Requires Rust 1.74+ (edition 2021).
- Supports AES-128/192/256 keys (32/48/64 hex chars) and 64-bit tweaks (16 hex chars).
- CLI tools follow the shared flag interface and emit JSON or plain text aligned with the documentation.
- All 15 NIST SP 800-38G vectors pass via `ff3-validate`.

---

## References
- NIST SP 800-38G (Format-Preserving Encryption)
- `aes`/`cipher` crates used for AES-ECB
