# FF3 Stress Test Tool Architecture

**Version:** 1.0
**Status:** Reference
**Last Updated:** 2024-12-01

## Purpose

Randomized stress testing with random keys, tweaks, and plaintexts. Complements validation by testing many random inputs to find edge cases.

## Quick Example

```bash
# Default (1000 iterations per alphabet)
ff3-stresstest

# Custom iteration count
ff3-stresstest 5000

# Quick test
ff3-stresstest --quick
```

## Expected Output

Output should be plain ASCII and suitable for logs (no emoji).

```
FF3 Stress Test v1.0
====================

Warning: FF3 was withdrawn by NIST; run for education and research only.

Test configuration
  Iterations per alphabet: 1000
  Random key/tweak generation: enabled
  String length range: 6-20 characters
  Alphabets: digits, hex-lower, base36-lower, base62

Testing digits...
  Alphabet: 0123456789 (radix 10)
  Progress: 100/1000 (10%)
  ...
  Progress: 1000/1000 (100%)
  Passed: 1000, Failed: 0

Testing hex-lower...
  Alphabet: 0123456789abcdef (radix 16)
  Progress: 100/1000 (10%)
  ...
  Passed: 1000, Failed: 0

Testing base36-lower...
  Passed: 1000, Failed: 0

Testing base62...
  Passed: 1000, Failed: 0

Summary
  Total tests: 4000
  Failures: 0
  Result: all stress tests passed
```

## Standard Flags

```bash
ff3-stresstest [OPTIONS] [ITERATIONS]

Arguments:
  ITERATIONS            Number of iterations per alphabet (default: 1000)

Options:
  --alphabets LIST      Comma-separated list; defaults to digits,hex-lower,base36-lower,base62
  --min-length N        Minimum plaintext length (default: 6)
  --max-length N        Maximum plaintext length (default: 20)
  --quick               Run 100 iterations (fast test)
  --seed N              Random seed for reproducibility
  -h, --help            Show help

Examples:
  ff3-stresstest                    # 1000 iterations
  ff3-stresstest 10000              # 10k iterations
  ff3-stresstest --quick            # 100 iterations
  ff3-stresstest --alphabets digits,hex-lower --min-length 10 --max-length 15
```

## Test Methodology

For each iteration:

1. **Generate random 128-bit key** (crypto-random)
2. **Generate random 64-bit tweak** (crypto-random)
3. **Generate random length** between min-length and max-length
4. **Generate random plaintext** of that length using alphabet
5. **Create cipher** with random key/tweak
6. **Encrypt** plaintext → ciphertext
7. **Decrypt** ciphertext → decrypted
8. **Verify** decrypted == plaintext
9. **Report failure** immediately if mismatch

## Implementation (~200-250 lines)

```pseudocode
function main(args) {
    iterations = parseIterations(args) || 1000
    alphabets = parseAlphabets(args) || ["digits", "hex-lower", "base36-lower", "base62"]
    minLength = parseMinLength(args) || 6
    maxLength = parseMaxLength(args) || 20

    if (hasFlag(args, "--quick")) {
        iterations = 100
    }

    print "FF3 Stress Test"
    print "Configuration:"
    print "  Iterations: " + iterations
    print "  Alphabets: " + alphabets.join(", ")
    print "  Lengths: " + minLength + "-" + maxLength

    totalPassed = 0
    totalFailed = 0

    for alphabet in alphabets {
        result = testAlphabet(alphabet, iterations, minLength, maxLength)
        totalPassed += result.passed
        totalFailed += result.failed
    }

    // Summary
    total = totalPassed + totalFailed
    successRate = (totalPassed / total) * 100

    print "Final Results:"
    print "Total: " + totalPassed + "/" + total + " (" + successRate + "%)"

    if (totalFailed == 0) {
        print "All tests passed."
        exit(0)
    } else {
        print "Failures: " + totalFailed
        exit(1)
    }
}

function testAlphabet(alphabetName, iterations, minLen, maxLen) {
    print "Testing " + alphabetName + "..."

    alphabet = getAlphabetChars(alphabetName)
    print "   Alphabet: " + alphabet
    print "   Radix: " + alphabet.length

    passed = 0
    failed = 0

    for i in 0..iterations {
        // Generate random test case
        key = randomBytes(16)
        tweak = randomBytes(8)
        length = randomInt(minLen, maxLen)
        plaintext = randomString(alphabet, length)

        // Test
        cipher = createCipher(key, tweak, alphabetName)
        ciphertext = cipher.encrypt(plaintext)
        decrypted = cipher.decrypt(ciphertext)

        if (decrypted == plaintext) {
            passed++

            // Show progress every 10%
            if ((i + 1) % (iterations / 10) == 0) {
                percent = ((i + 1) / iterations) * 100
                print "   Progress: " + (i + 1) + "/" + iterations + " (" + percent + "%)"
            }
        } else {
            // Immediate failure report
            print "   Round-trip failed:"
            print "      Key: " + hexEncode(key)
            print "      Tweak: " + hexEncode(tweak)
            print "      Plaintext: \"" + plaintext + "\""
            print "      Ciphertext: \"" + ciphertext + "\""
            print "      Decrypted: \"" + decrypted + "\" (mismatch)"
            failed++
        }
    }

    print "   Passed: " + passed + "/" + (passed + failed)
    print "   Failed: " + failed + "/" + (passed + failed)
    print ""

    return {passed: passed, failed: failed}
}

function randomString(alphabet, length) {
    result = ""
    for i in 0..length {
        idx = randomInt(0, alphabet.length - 1)
        result += alphabet[idx]
    }
    return result
}
```

## Random Generation

Use language-appropriate cryptographically secure random for keys/tweaks.
Standard random acceptable for plaintexts (ensure characters match alphabet).

## Progress Reporting

Show progress every 10% to avoid spam:

```
Progress: 100/1000 (10%)
Progress: 200/1000 (20%)
...
Progress: 1000/1000 (100%)
```

Or every 100 iterations if count < 1000.

## Failure Details

When a test fails, show ALL the details:

```
Round-trip failed:
   Key: a3b5c7d9e1f3a5b7c9d1e3f5a7b9c1d3
   Tweak: 1234567890abcdef
   Alphabet: digits (radix 10)
   Plaintext: "12345678901234"
   Ciphertext: "98765432109876"
   Decrypted: "12345678901235"  ← MISMATCH at position 13
```

## Exit Codes

- **0** - All tests passed
- **1** - One or more tests failed

## Default Alphabets

Test these by default:

1. **digits** - `0123456789` (radix 10)
2. **hex-lower** - `0123456789abcdef` (radix 16)
3. **base36-lower** - `0123456789abcdefghijklmnopqrstuvwxyz` (radix 36)
4. **base62** - `0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz` (radix 62)

## String Lengths

Default: 6-20 characters

- **Min 6** - FF3 typically requires at least 4-6 characters
- **Max 20** - Reasonable for stress testing without being too slow
- Configurable via `--min-length` and `--max-length`

## Reproducibility

```bash
# Use seed for reproducible tests
ff3-stresstest --seed 12345

# Run again with same seed = same results
ff3-stresstest --seed 12345
```

## CI/CD Integration

```yaml
# GitHub Actions
- name: Quick Stress Test
  run: ff3-stresstest --quick

- name: Full Stress Test
  run: ff3-stresstest 10000
```

## Testing

```bash
# Quick test (should pass)
ff3-stresstest --quick
echo $?  # 0

# Full test
ff3-stresstest 1000

# Specific alphabets
ff3-stresstest --alphabets digits,hex-lower 500

# Custom length range
ff3-stresstest --min-length 8 --max-length 16 1000
```

## File Organization

Language-specific locations for stress test tools:
- Stress test applications in language-appropriate directories
- Single-file implementations (~200-250 lines)

---

**Target:** ~200-250 lines per language

**Note:** This tool finds bugs that NIST vectors miss! Always run both validation and stress testing.

## Helper Script

Use `scripts/run-stress-test.sh <language> [options]` to execute a specific implementation without rebuilding everything. Example:

```bash
./scripts/run-stress-test.sh python --quick --seed 123
```
