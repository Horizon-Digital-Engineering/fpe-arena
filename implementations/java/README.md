# FF3 – Java Implementation

**Security Notice**: FF3 was withdrawn by NIST due to published vulnerabilities. Use this implementation for education, research, and interoperability testing only.

---

## Quick Start
```bash
./gradlew build
./gradlew test
./gradlew :ff3-cli:run --args="--help"
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
./gradlew :ff3-cli:run --args="-e 890121234567890000 -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73"
./gradlew :ff3-cli:run --args="-d 750918814058654607 -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73"
./gradlew :ff3-cli:run --args="-e HELLO -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73 -c ABCDEFGHIJKLMNOPQRSTUVWXYZ"
```

---

## API Example
```java
import dev.horizondigital.ff3.*;

byte[] key = FF3API.hexToBytes("EF4359D8D580AA4F7F036D6F04FC6A94");
byte[] tweak = FF3API.hexToBytes("D8E7920AFA330A73");

FF3API cipher = FF3API.digits(key, tweak);
String ciphertext = cipher.encrypt("1234567890");
String plaintext  = cipher.decrypt(ciphertext);
```
> Demonstrates a single round trip using the high-level Java API.

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
./gradlew :ff3-validate:run --args="--vectors ../../shared/test-vectors/nist_ff3_official_vectors.json"
./gradlew :ff3-benchmark:run --args="--quick"
./gradlew :ff3-stresstest:run --args="1000"
```

---

## Project Layout
```
implementations/java/
├── ff3-core/        # Core library (core/api/alphabets)
├── ff3-cli/         # CLI wrapper
├── ff3-validate/    # NIST validation tool
├── ff3-benchmark/   # Benchmark tool
└── ff3-stresstest/  # Randomized stress tester
```

---

## Notes
- Requires JDK 21+ and Gradle (uses the wrapper contained in the repo).
- Supports AES-128/192/256 keys (32/48/64 hex chars) and 64-bit tweaks (16 hex chars).
- CLI tools are flag driven and produce output consistent with the shared specification.
- All 15 NIST SP 800-38G vectors pass via `ff3-validate`.

---

## References
- NIST SP 800-38G (Format-Preserving Encryption)
- Java Cryptography Architecture (JCA) AES/ECB provider
