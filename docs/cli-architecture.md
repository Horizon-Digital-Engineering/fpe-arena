# FF3 CLI Tool Architecture

**Version:** 1.0
**Status:** Reference
**Last Updated:** 2024-12-01

## Purpose

Command-line tool for encrypt/decrypt operations with flags. **No interactive mode**.

## Quick Example

```bash
# Encrypt
ff3-cli --encrypt "1234567890" \
        --key EF4359D8D580AA4F7F036D6F04FC6A94 \
        --tweak D8E7920AFA330A73

# Decrypt
ff3-cli --decrypt "6572130655" \
        --key EF4359D8D580AA4F7F036D6F04FC6A94 \
        --tweak D8E7920AFA330A73

# Different alphabet
ff3-cli --encrypt "Hello123" \
        --alphabet base62 \
        --key EF4359D8D580AA4F7F036D6F04FC6A94 \
        --tweak D8E7920AFA330A73
```

## Standard Flags

```bash
ff3-cli [OPTIONS]

Required (one of):
  -e, --encrypt TEXT      Encrypt the given text
  -d, --decrypt TEXT      Decrypt the given text

Required:
  -k, --key HEX           AES key in hex (32/48/64 hex chars)
  -t, --tweak HEX         Tweak in hex (16 hex chars)

Optional:
  -a, --alphabet TYPE     Alphabet type (default: digits)
                            digits, hex-lower, hex-upper,
                            base36-lower, base36-upper, base62
  -c, --custom CHARSET    Custom alphabet (overrides --alphabet)
  -h, --help              Show help

Examples:
  ff3-cli -e "1234567890" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73
  ff3-cli -d "6572130655" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73
  ff3-cli -e "ABC123" -a hex-upper -k EF... -t D8...
```

## Expected Output

### Encrypt
```bash
$ ff3-cli -e "1234567890" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73
6572130655
```

### Decrypt
```bash
$ ff3-cli -d "6572130655" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73
1234567890
```

### Error
```bash
$ ff3-cli -e "ABC" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73
Error: invalid character 'A' for alphabet 'digits' (radix 10)
```

## Implementation (~150-200 lines)

```pseudocode
function main(args) {
    // Parse flags
    encryptText = getFlag(args, "--encrypt", "-e")
    decryptText = getFlag(args, "--decrypt", "-d")
    keyHex = getFlag(args, "--key", "-k")
    tweakHex = getFlag(args, "--tweak", "-t")
    alphabet = getFlag(args, "--alphabet", "-a") || "digits"
    customCharset = getFlag(args, "--custom", "-c")

    // Validate
    if (!encryptText && !decryptText) {
        printUsage()
        exit(2)
    }
    if (!keyHex || !tweakHex) {
        printUsage()
        exit(2)
    }

    // Decode hex
    key = hexDecode(keyHex)
    tweak = hexDecode(tweakHex)

    if (key.length not in [16, 24, 32]) {
        error("Invalid key length")
        exit(1)
    }
    if (tweak.length != 8) {
        error("Invalid tweak length")
        exit(1)
    }

    // Create cipher
    cipher = createCipher(key, tweak, alphabet, customCharset)

    // Encrypt or decrypt
    if (encryptText) {
        result = cipher.encrypt(encryptText)
        print(result)
    } else {
        result = cipher.decrypt(decryptText)
        print(result)
    }
}

function createCipher(key, tweak, alphabet, customCharset) {
    if (customCharset) {
        return newCipherFromSpec(key, tweak, customCharset)
    }

    switch (alphabet) {
        case "digits": return newDigitsCipher(key, tweak)
        case "hex-lower": return newHexLowerCipher(key, tweak)
        case "hex-upper": return newHexUpperCipher(key, tweak)
        case "base36-lower": return newBase36LowerCipher(key, tweak)
        case "base36-upper": return newBase36UpperCipher(key, tweak)
        case "base62": return newBase62Cipher(key, tweak)
        default: error("Unknown alphabet: " + alphabet)
    }
}
```

## Exit Codes

- **0** - Success
- **1** - Error (invalid input, cipher error)
- **2** - Usage error (missing flags, invalid flags)

## Error Messages

Be clear and helpful:

```
Error: missing required flag --key
Error: invalid key length (expected 32/48/64 hex chars, got 30)
Error: invalid character 'X' for alphabet 'digits' (valid: 0-9)
Error: plaintext length 5 is too short (minimum: 6)
```

## Testing

```bash
# Test encryption
OUT=$(ff3-cli -e "1234567890" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73)
echo "Encrypted: $OUT"

# Test round-trip
CT=$(ff3-cli -e "1234567890" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73)
PT=$(ff3-cli -d "$CT" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73)
[[ "$PT" == "1234567890" ]] && echo "Round-trip success"

# Test error handling
ff3-cli -e "ABC" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73 2>&1 | grep "Error"
```

## File Organization

Language-specific locations for CLI tools:
- Command-line applications in language-appropriate directories
- Single-file implementations (~150-200 lines)

## What Not to Include

- Interactive mode
- Multiple operations in one run
- External config files
- Elaborate output formatting

## What to Include

- Command-line flag parsing
- Hex key and tweak decoding
- Support for all standard alphabets
- Custom alphabet support
- Clear error messages
- A help message

---

**Target:** ~150-200 lines per language
