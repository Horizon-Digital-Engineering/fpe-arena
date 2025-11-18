#!/usr/bin/env node

/**
 * FF3 Format Preserving Encryption - CLI Tool
 *
 * Command-line interface for FF3 encryption/decryption operations.
 *
 * ⚠️ FF3 was withdrawn by NIST due to security vulnerabilities.
 * This implementation is for educational and research purposes only.
 */

import {
    digits,
    hexLower,
    hexUpper,
    base36Lower,
    base36Upper,
    base62,
    radix26,
    fromSpec,
    AlphabetSpec,
    hexToBytes,
    VERSION
} from '../lib/index.js';

/**
 * Display security warning
 */
function showSecurityWarning() {
    console.log(`
FF3 Format Preserving Encryption CLI v${VERSION}

    FF3 was WITHDRAWN by NIST due to security vulnerabilities.
    This implementation is for EDUCATIONAL and RESEARCH purposes only.

    DO NOT use in production systems.

    Research reference: NIST SP 800-38G (withdrawn)
`);
}

/**
 * Display usage information
 */
function showUsage() {
    console.log(`
Usage: ff3 [options]

Options:
  -h, --help                 Show this help message
  -e, --encrypt TEXT         Encrypt the given text
  -d, --decrypt TEXT         Decrypt the given text
  -k, --key HEX             Key in hexadecimal format (required)
  -t, --tweak HEX           Tweak in hexadecimal format (required)
  -a, --alphabet TYPE       Alphabet type (default: digits)
  -c, --custom CHARSET      Custom alphabet characters
  -v, --version             Show version

Alphabet Types:
  digits        0123456789
  hex, hex-lower 0123456789abcdef
  hex-upper     0123456789ABCDEF
  base36-lower  0123456789abcdefghijklmnopqrstuvwxyz
  base36-upper  0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ
  base62        0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz
  radix26       0123456789abcdefghijklmnop
  custom        Use -c to specify custom alphabet

Examples:
  ff3 -e "1234567890" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73
  ff3 -d "750918814058654607" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73
  ff3 -e "ABC123" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73 -a hex-upper
`);
}

/**
 * Create cipher based on alphabet type
 */
function createCipher(key, tweak, alphabetType, customCharset) {
    switch (alphabetType) {
        case 'digits':
            return digits(key, tweak);
        case 'hex':
        case 'hex-lower':
            return hexLower(key, tweak);
        case 'hex-upper':
            return hexUpper(key, tweak);
        case 'base36-lower':
            return base36Lower(key, tweak);
        case 'base36-upper':
            return base36Upper(key, tweak);
        case 'base62':
            return base62(key, tweak);
        case 'radix26':
            return radix26(key, tweak);
        case 'custom': {
            if (!customCharset) {
                throw new Error('Custom alphabet requires -c option with charset');
            }
            const spec = new AlphabetSpec(customCharset);
            return fromSpec(key, tweak, spec);
        }
        default:
            throw new Error(`Unknown alphabet type: ${alphabetType}`);
    }
}

/**
 * Command line mode
 */
function commandLineMode(args) {
    let encrypt = null;
    let decrypt = null;
    let keyHex = null;
    let tweakHex = null;
    let alphabetType = 'digits';
    let customCharset = null;

    // Parse arguments
    for (let i = 0; i < args.length; i++) {
        const arg = args[i];
        switch (arg) {
            case '-h':
            case '--help':
                showUsage();
                process.exit(0);
                break;
            case '-v':
            case '--version':
                console.log(`FF3 CLI v${VERSION}`);
                process.exit(0);
                break;
            case '-e':
            case '--encrypt':
                i++;
                encrypt = args[i];
                break;
            case '-d':
            case '--decrypt':
                i++;
                decrypt = args[i];
                break;
            case '-k':
            case '--key':
                i++;
                keyHex = args[i];
                break;
            case '-t':
            case '--tweak':
                i++;
                tweakHex = args[i];
                break;
            case '-a':
            case '--alphabet':
                i++;
                alphabetType = args[i];
                break;
            case '-c':
            case '--custom':
                i++;
                customCharset = args[i];
                break;
            default:
                console.error(`Unknown option: ${arg}`);
                showUsage();
                process.exit(1);
        }
    }

    // Validate required parameters
    if (!keyHex || !tweakHex) {
        console.error('Error: Key (-k) and tweak (-t) are required');
        showUsage();
        process.exit(1);
    }

    if (!encrypt && !decrypt) {
        console.error('Error: Either --encrypt or --decrypt must be specified');
        showUsage();
        process.exit(1);
    }

    try {
        const key = hexToBytes(keyHex);
        const tweak = hexToBytes(tweakHex);
        const cipher = createCipher(key, tweak, alphabetType, customCharset);

        if (encrypt) {
            const result = cipher.encrypt(encrypt);
            console.log(result);
        }

        if (decrypt) {
            const result = cipher.decrypt(decrypt);
            console.log(result);
        }

    } catch (error) {
        console.error('Error:', error.message);
        process.exit(1);
    }
}

/**
 * Main function
 */
function main() {
    showSecurityWarning();

    const args = process.argv.slice(2);

    if (args.length === 0) {
        showUsage();
        process.exit(0);
    }

    commandLineMode(args);
}

// Run main function
try {
    main();
} catch (error) {
    console.error('Fatal error:', error.message);
    process.exit(1);
}