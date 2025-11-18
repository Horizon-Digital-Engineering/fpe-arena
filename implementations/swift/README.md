# FF3 – Swift Implementation

**Security Notice**: FF3 was withdrawn by NIST due to published vulnerabilities. Use this implementation for education, research, and interoperability testing only.

---

## Quick Start
```bash
swift build
swift test
swift run FF3CLI --help
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
swift run FF3CLI -e 1234567890 -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73
swift run FF3CLI -d 6124200773 -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73
swift run FF3CLI -e HELLO -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73 -c ABCDEFGHIJKLMNOPQRSTUVWXYZ
```

---

## API Example
```swift
import FF3

let key = Data(hex: "EF4359D8D580AA4F7F036D6F04FC6A94")!
let tweak = Data(hex: "D8E7920AFA330A73")!

let cipher = try digits(key: key, tweak: tweak)
let ciphertext = try cipher.encrypt("1234567890")
let plaintext  = try cipher.decrypt(ciphertext)
```
> Demonstrates a single round trip using the Swift API.

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
swift run FF3Validate --vectors ../../shared/test-vectors/nist_ff3_official_vectors.json
swift run FF3Benchmark --quick
swift run FF3StressTest 1000
```

---

## Project Layout
```
implementations/swift/
├── Sources/FF3/            # Core library (core/api/alphabets)
├── Sources/FF3CLI/         # CLI wrapper
├── Sources/FF3Validate/    # NIST validation tool
├── Sources/FF3Benchmark/   # Benchmark tool
├── Sources/FF3StressTest/  # Randomized stress tester
└── Tests/FF3Tests/         # XCTest suite
```

---

## Notes
- Requires Swift 5.9+ with Swift Package Manager.
- Uses CommonCrypto on Apple platforms and CryptoSwift on Linux/Windows.
- Supports AES-128/192/256 keys (32/48/64 hex chars) and 64-bit tweaks (16 hex chars).
- All 15 NIST SP 800-38G vectors pass via `FF3Validate`.

---

## References
- NIST SP 800-38G (Format-Preserving Encryption)
- CommonCrypto / CryptoSwift AES primitives
