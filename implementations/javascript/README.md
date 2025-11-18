# FF3 – JavaScript Implementation

**Security Notice**: FF3 was withdrawn by NIST due to published vulnerabilities. Use this implementation for education, research, and interoperability testing only.

---

## Quick Start
```bash
npm install
npm test
node bin/ff3.js --help
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
node bin/ff3.js -e "890121234567890000" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73
node bin/ff3.js -d "750918814058654607" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73
node bin/ff3.js -e "HELLO" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73 -c "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
```

---

## API Example
```javascript
import { digits, hexToBytes } from 'fpe-ff3';

const key = hexToBytes('EF4359D8D580AA4F7F036D6F04FC6A94');
const tweak = hexToBytes('D8E7920AFA330A73');

const cipher = digits(key, tweak);
const ciphertext = cipher.encrypt('1234567890');
const plaintext = cipher.decrypt(ciphertext);
```
> Demonstrates a single round trip using the ES module API.

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
node bin/ff3-validate.js --vectors ../../shared/test-vectors/nist_ff3_official_vectors.json
node bin/ff3-benchmark.js --quick
node bin/ff3-stresstest.js 1000
```

---

## Project Layout
```
implementations/javascript/
├── lib/                 # Library modules (core/api/alphabets)
├── bin/ff3.js           # CLI wrapper
├── bin/ff3-validate.js  # NIST validation tool
├── bin/ff3-benchmark.js # Benchmark tool
├── bin/ff3-stresstest.js # Randomized stress tester
└── test/                # Unit and integration tests
```

---

## Notes
- Requires Node.js 18+.
- Uses only built-in Node.js `crypto` primitives (no external runtime dependencies).
- CLI tools follow the shared flag interface and emit JSON or plain text aligned with the documentation.
- All 15 NIST SP 800-38G vectors pass via `ff3-validate.js`.

---

## References
- NIST SP 800-38G (Format-Preserving Encryption)
- Node.js `crypto` module (AES-ECB implementation)
