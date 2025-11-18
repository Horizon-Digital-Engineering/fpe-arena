# FF3 – .NET Implementation

**Security Notice**: FF3 was withdrawn by NIST due to published vulnerabilities. Use this code for education, research, and interoperability testing only.

---

## Quick Start
```bash
dotnet build
dotnet test
dotnet run --project FF3.CLI -- --help
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
dotnet run --project FF3.CLI -- -e "890121234567890000" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73
dotnet run --project FF3.CLI -- -d "750918814058654607" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73
dotnet run --project FF3.CLI -- -e "HELLO" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73 -c "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
```

---

## API Example
```csharp
using FF3.Core;

var key = Core.FF3.HexToBytes("EF4359D8D580AA4F7F036D6F04FC6A94");
var tweak = Core.FF3.HexToBytes("D8E7920AFA330A73");

using var cipher = Core.FF3.Digits(key, tweak);

string ciphertext = cipher.Encrypt("1234567890");
string plaintext = cipher.Decrypt(ciphertext);
```
> Demonstrates a single round trip using the `FF3.Core` API.

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
dotnet run --project FF3.Validate -- --vectors ../../shared/test-vectors/nist_ff3_official_vectors.json
dotnet run --project FF3.Benchmark -- --quick
dotnet run --project FF3.StressTest -- 1000
```

---

## Project Layout
```
dotnet/
├── FF3.Core/         # Library (core/api/alphabets)
├── FF3.CLI/          # Command-line wrapper
├── FF3.Validate/     # NIST validation tool
├── FF3.Benchmark/    # Benchmark tool
├── FF3.StressTest/   # Randomized stress tester
└── FF3.Tests/        # xUnit test suite
```

---

## Notes
- Supports AES-128/192/256 keys (32/48/64 hex chars)
- Requires 16-hex-character tweak (64 bits)
- CLI tools are flag-driven and non-interactive
- All 15 NIST SP 800-38G vectors pass via `FF3.Validate`

---

## References
- NIST SP 800-38G (Format-Preserving Encryption)
- `System.Security.Cryptography` (AES implementation)
