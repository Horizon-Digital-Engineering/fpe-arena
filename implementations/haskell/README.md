# FF3 – Haskell Implementation

**Security Notice**: FF3 was withdrawn by NIST due to published vulnerabilities. Use this implementation for education, research, and interoperability testing only.

---

## Quick Start
```bash
stack build
stack test
stack exec ff3-cli -- --help
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
stack exec ff3-cli -- -e "890121234567890000" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73
stack exec ff3-cli -- -d "750918814058654607" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73
stack exec ff3-cli -- -e "HELLO" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73 -c "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
```

---

## API Example
```haskell
import FF3
import qualified Data.ByteString as BS

let key   = BS.pack [0xEF,0x43,0x59,0xD8,0xD5,0x80,0xAA,0x4F,0x7F,0x03,0x6D,0x6F,0x04,0xFC,0x6A,0x94]
let tweak = BS.pack [0xD8,0xE7,0x92,0x0A,0xFA,0x33,0x0A,0x73]

case digits key tweak of
  Left err     -> putStrLn err
  Right cipher -> do
    let Right ct = encryptText cipher "1234567890" Nothing
    let Right pt = decryptText cipher ct Nothing
    putStrLn $ ct ++ " -> " ++ pt
```
> Demonstrates a single round trip using the high-level Haskell API.

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
stack exec ff3-validate -- --vectors ../../shared/test-vectors/nist_ff3_official_vectors.json
stack exec ff3-benchmark -- --quick
stack exec ff3-stresstest -- 1000
```

---

## Project Layout
```
implementations/haskell/
├── src/FF3/            # Core library (core/api/alphabets)
├── app/CLI.hs          # CLI entry point
├── app/Validate.hs     # NIST validation tool
├── app/Benchmark.hs    # Benchmark tool
├── app/StressTest.hs   # Randomized stress tester
└── test/Main.hs        # Hspec test suite
```

---

## Notes
- Requires Stack or Cabal with GHC ≥ 9.
- Supports AES-128/192/256 keys (32/48/64 hex chars) and 64-bit tweaks (16 hex chars).
- CLI tools are non-interactive and follow the shared flag interface.
- All 15 NIST SP 800-38G vectors pass via `ff3-validate`.

---

## References
- NIST SP 800-38G (Format-Preserving Encryption)
- `cryptonite` primitives used for AES/ECB in Haskell
