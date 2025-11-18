# FF3 Validation Tool Architecture

**Version:** 1.0
**Status:** Reference
**Last Updated:** 2024-12-01

## Purpose

Validates FF3 implementation correctness against official NIST SP 800-38G test vectors. Essential for CI/CD - must pass for valid implementation.

## Quick Example

```bash
# Run validation
ff3-validate

# Output: All 15 NIST tests pass, exit code 0
```

## Expected Output

```
FF3 NIST Validation v1.0
========================

Warning: FF3 was withdrawn by NIST; use this tool for education and research only.

Loading NIST SP 800-38G test vectors...
Loaded vectors from ../../shared/test-vectors/nist_ff3_official_vectors.json
Vector count: 15

Sample 1 (FF3-AES128)
  Radix: 10
  Key: EF4359D8D580AA4F7F036D6F04FC6A94
  Tweak: D8E7920AFA330A73
  Plaintext: 890121234567890000
  Expected ciphertext: 750918814058654607
  Computed ciphertext: 750918814058654607
  Round-trip: success

...

Summary
  Passed: 15
  Failed: 0
  Result: all NIST FF3 vectors matched expected values
```

## Standard Flags

```bash
ff3-validate [OPTIONS]

Options:
  --vectors PATH    Path to test vectors JSON (default: auto-detect)
  --quiet           Only show failures and summary
  --verbose         Show all test details (default)
  -h, --help        Show help
```

## Test Vector File

Expected JSON format (`nist_ff3_official_vectors.json`):

```json
{
  "description": "NIST SP 800-38G FF3 Official Test Vectors",
  "source": "https://csrc.nist.gov/publications/detail/sp/800-38g/final",
  "vectors": [
    {
      "sample": 1,
      "algorithm": "FF3-AES128",
      "key": "EF4359D8D580AA4F7F036D6F04FC6A94",
      "radix": 10,
      "alphabet": "0123456789",
      "plaintext": "890121234567890000",
      "tweak": "D8E7920AFA330A73",
      "ciphertext": "750918814058654607"
    },
    {
      "sample": 2,
      ...
    }
  ]
}
```

## Implementation (~200-250 lines)

```pseudocode
function main() {
    print "FF3 NIST Test Vector Validation"
    print "Warning: Educational use only"

    // Load test vectors
    vectors = loadTestVectors()
    if (!vectors) {
        error("Could not find test vectors file")
        exit(1)
    }

    print "Loaded " + vectors.count + " test vectors"

    passed = 0
    failed = 0

    // Test each vector
    for vector in vectors {
        print "Testing Sample " + vector.sample

        // Decode key/tweak
        key = hexDecode(vector.key)
        tweak = hexDecode(vector.tweak)

        // Create cipher
        cipher = createCipher(key, tweak, vector.alphabet)

        // Test encryption
        result = cipher.encrypt(vector.plaintext)

        if (result == vector.ciphertext) {
            print "  Match: ciphertext equals expected value."

            // Test round-trip
            decrypted = cipher.decrypt(result)
            if (decrypted == vector.plaintext) {
                print "  Round-trip successful."
                passed++
            } else {
                print "  Round-trip failed."
                print "     Expected: " + vector.plaintext
                print "     Got:      " + decrypted
                failed++
            }
        } else {
            print "  Ciphertext mismatch."
            print "     Expected: " + vector.ciphertext
            print "     Got:      " + result
            failed++
        }
    }

    // Summary
    print "Final Results:"
    print "Passed: " + passed + "/" + vectors.count
    print "Failed: " + failed + "/" + vectors.count

    if (failed == 0) {
        print "All tests passed."
        exit(0)
    } else {
        print "Validation failed."
        exit(1)
    }
}

function loadTestVectors() {
    // Try multiple paths
    paths = [
        env("FF3_TEST_VECTORS_PATH"),
        "../../shared/test-vectors/nist_ff3_official_vectors.json",
        "../../../shared/test-vectors/nist_ff3_official_vectors.json",
        "./nist_ff3_official_vectors.json"
    ]

    for path in paths {
        if (fileExists(path)) {
            data = readFile(path)
            return parseJSON(data)
        }
    }

    return null
}
```

## File Path Detection

Try these paths in order:

1. **Environment variable**: `FF3_TEST_VECTORS_PATH`
2. **Relative paths** from binary location:
   - `../../shared/test-vectors/nist_ff3_official_vectors.json`
   - `../../../shared/test-vectors/nist_ff3_official_vectors.json`
3. **Current directory**: `./nist_ff3_official_vectors.json`

```bash
# Set via environment variable (useful for CI)
export FF3_TEST_VECTORS_PATH=/path/to/vectors.json
ff3-validate

# Or specify directly
ff3-validate --vectors /path/to/vectors.json
```

## Exit Codes

- **0** - All tests passed
- **1** - One or more tests failed
- **2** - Could not load test vectors file

## Test Details

### What to test:
1. **Encryption** - plaintext → expected ciphertext
2. **Decryption** - ciphertext → original plaintext (round-trip)
3. **Multiple key sizes** - 128, 192, 256-bit AES
4. **Multiple radixes** - radix 10, radix 26
5. **Different lengths** - various plaintext lengths

### 15 Official NIST Vectors:
- Samples 1-5: FF3-AES128 (128-bit key)
- Samples 6-10: FF3-AES192 (192-bit key)
- Samples 11-15: FF3-AES256 (256-bit key)

## Error Handling

```
Test Sample 5 failed:
   Expected: g2pk40i992fn20cjakb
   Got:      h3ql51j003gn31dlakc

Round-trip failed for Sample 3:
   Original:  890121234567890000
   Encrypted: 750918814058654607
   Decrypted: 890121234567890001  ← MISMATCH
```

## CI/CD Integration

```yaml
# GitHub Actions example
- name: Validate NIST Vectors
  run: ff3-validate

- name: Validation with custom path
  run: ff3-validate --vectors shared/test-vectors/nist_ff3_official_vectors.json
```

## Testing

```bash
# Should pass
ff3-validate
echo $?  # 0

# Should fail (intentionally broken implementation)
ff3-validate || echo "Failed as expected"
```

## Quiet Mode

```bash
ff3-validate --quiet
```

Only shows failures and final summary:

```
Sample 5 failed: expected g2pk40i992fn20cjakb, got h3ql51j003gn31dlakc
Passed: 14/15
Failed: 1/15
```

## File Organization

Language-specific locations for validation tools:
- Validation applications in language-appropriate directories
- Single-file implementations (~200-250 lines)

---

**Target:** ~200-250 lines per language

**Critical:** Must exit with code 1 if ANY test fails (for CI/CD)!
