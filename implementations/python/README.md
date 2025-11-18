# FF3 – Python Implementation

**Security Notice**: FF3 was withdrawn by NIST due to published vulnerabilities. Use this implementation for education, research, and interoperability testing only.

---

## Quick Start
```bash
pip install -e .
pytest tests/
python3 cli/ff3_cli.py --help
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
python3 cli/ff3_cli.py -e "890121234567890000" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73
python3 cli/ff3_cli.py -d "750918814058654607" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73
python3 cli/ff3_cli.py -e "HELLO" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73 -c "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
```

---

## API Example
```python
from ff3 import digits, AlphabetSpec, from_spec

key = bytes.fromhex("EF4359D8D580AA4F7F036D6F04FC6A94")
tweak = bytes.fromhex("D8E7920AFA330A73")

cipher = digits(key, tweak)
ciphertext = cipher.encrypt("1234567890")
plaintext = cipher.decrypt(ciphertext)
```
> Demonstrates a single round trip using the Python API.

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
python3 cli/ff3_validate.py --vectors ../../shared/test-vectors/nist_ff3_official_vectors.json
python3 cli/ff3_benchmark.py --quick
python3 cli/ff3_stresstest.py 1000
```

---

## Project Layout
```
implementations/python/
├── src/ff3/        # Library modules (core/api/alphabets)
├── cli/            # CLI entry points
├── tests/          # Unit and integration tests
└── setup.py        # Packaging configuration
```

---

## Notes
- Requires Python 3.10+ and the third-party `cryptography` package for AES.
- Supports AES-128/192/256 keys (32/48/64 hex chars) and 64-bit tweaks (16 hex chars).
- CLI tools follow the shared flag interface and emit JSON or plain text aligned with the documentation.
- All 15 NIST SP 800-38G vectors pass via `ff3_validate.py`.

---

## References
- NIST SP 800-38G (Format-Preserving Encryption)
- `cryptography` package (`Cipher`, `algorithms.AES`, `modes.ECB`)
