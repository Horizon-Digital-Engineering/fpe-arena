# FF3 – Ada/SPARK Implementation

**Security Notice**: FF3 was withdrawn by NIST due to published vulnerabilities. Use this implementation for education, research, and interoperability testing only.

---

## Quick Start
```bash
alr build
alr test
./build/bin/ff3_cli --help
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
./build/bin/ff3_cli -e "890121234567890000" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73
./build/bin/ff3_cli -d "750918814058654607" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73
./build/bin/ff3_cli -e "HELLO" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73 -c "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
```

---

## API Example
```ada
with FF3.API, Ada.Strings.Unbounded;
use  FF3.API, Ada.Strings.Unbounded;

Key   : constant Byte_Array := Hex_To_Bytes ("EF4359D8D580AA4F7F036D6F04FC6A94");
Tweak : constant Byte_Array := Hex_To_Bytes ("D8E7920AFA330A73");
Cipher: FF3_String_Cipher := Digits_Cipher (Key, Tweak);

Ciphertext : constant String := Encrypt (Cipher, "1234567890");
Plaintext  : constant String := Decrypt (Cipher, Ciphertext);
```
> Demonstrates a single round trip via the high-level API.

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
./build/bin/ff3_validate --vectors ../../shared/test-vectors/nist_ff3_official_vectors.json
./build/bin/ff3_benchmark --quick
./build/bin/ff3_stresstest 1000
```

---

## Project Layout
```
implementations/ada/
├── alire.toml          # Alire package manifest
├── ff3_ada.gpr         # GNAT project file
├── config/             # Toolchain configuration helpers
├── src/
│   ├── ff3/            # Core library (core/api/alphabets)
│   ├── cli/            # CLI wrapper
│   ├── validate/       # NIST validation tool
│   ├── benchmark/      # Benchmark tool
│   └── stresstest/     # Randomized stress tester
└── tests/              # Unit and regression tests
```

---

## Notes
- Supports AES-128/192/256 keys (32/48/64 hex chars)
- Requires a 16-hex-character tweak (64 bits)
- CLI tools are non-interactive and flag driven
- All 15 NIST SP 800-38G vectors pass via `ff3_validate`

---

## References
- NIST SP 800-38G (Format-Preserving Encryption)
- UtilAda and GNAT AES primitives for Ada

## Contributing

This is an educational implementation. Contributions focusing on:
- Code clarity and documentation
- Additional test coverage
- Performance optimization
- Formal SPARK proofs

are welcome via pull requests.
