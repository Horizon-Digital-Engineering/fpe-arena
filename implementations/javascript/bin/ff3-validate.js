#!/usr/bin/env node

/**
 * FF3 NIST Test Vector Validation Tool
 */

import { readFileSync, existsSync } from 'fs';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';
import {
    digits,
    hexLower,
    base36Lower,
    base62,
    radix26,
    hexToBytes,
    VERSION
} from '../lib/index.js';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

function showUsage() {
    console.log('FF3 NIST Test Vector Validation Tool\n');
    console.log('Usage: ff3-validate [OPTIONS]\n');
    console.log('Options:');
    console.log('  --vectors PATH    Path to test vectors JSON file');
    console.log('  --verbose         Show detailed test output');
    console.log('  --quiet           Only show failures and summary');
    console.log('  -h, --help        Show this help message\n');
}

function parseArgs(args) {
    const opts = {
        vectorsPath: null,
        verbose: false,
        quiet: false,
        help: false
    };

    for (let i = 0; i < args.length; i++) {
        switch (args[i]) {
            case '-h':
            case '--help':
                opts.help = true;
                return opts;
            case '--vectors':
                if (i + 1 >= args.length) {
                    throw new Error('Missing value for --vectors');
                }
                opts.vectorsPath = args[++i];
                break;
            case '--verbose':
                opts.verbose = true;
                break;
            case '--quiet':
                opts.quiet = true;
                break;
            default:
                throw new Error(`Unknown option: ${args[i]}`);
        }
    }

    return opts;
}

function findVectorsFile(customPath) {
    if (customPath) {
        if (existsSync(customPath)) {
            return customPath;
        }
        console.error(`Error: Vectors file not found: ${customPath}`);
        process.exit(2);
    }

    const paths = [
        join(__dirname, '../../../shared/test-vectors/nist_ff3_official_vectors.json'),
        join(__dirname, '../../shared/test-vectors/nist_ff3_official_vectors.json'),
        './nist_ff3_official_vectors.json'
    ];

    for (const path of paths) {
        if (existsSync(path)) {
            return path;
        }
    }

    console.error('Error: Could not find NIST test vectors file');
    console.error('Try: ff3-validate --vectors /path/to/vectors.json');
    process.exit(2);
}

function createCipherForRadix(key, tweak, radix) {
    switch (radix) {
        case 10:
            return digits(key, tweak);
        case 16:
            return hexLower(key, tweak);
        case 26:
            return radix26(key, tweak);
        case 36:
            return base36Lower(key, tweak);
        case 62:
            return base62(key, tweak);
        default:
            throw new Error(`Unsupported radix ${radix} in test vectors`);
    }
}

function main() {
    try {
        const opts = parseArgs(process.argv.slice(2));

        if (opts.help) {
            showUsage();
            process.exit(0);
        }

        if (!opts.quiet) {
            console.log('FF3 NIST Test Vector Validation Tool');
            console.log('========================================\n');
        }

        const vectorsPath = findVectorsFile(opts.vectorsPath);

        if (!opts.quiet) {
            console.log(`Vector file: ${vectorsPath}\n`);
        }

        const data = readFileSync(vectorsPath, 'utf8');
        const json = JSON.parse(data);
        const vectors = json.vectors;

        if (!opts.quiet) {
            console.log(`Testing ${vectors.length} NIST FF3 vectors...\n`);
        }

        let passed = 0;
        let failed = 0;

        for (const vector of vectors) {
            try {
                const key = hexToBytes(vector.key);
                const tweak = hexToBytes(vector.tweak);
                const cipher = createCipherForRadix(key, tweak, vector.radix);

                const encrypted = cipher.encrypt(vector.plaintext);
                const encryptPassed = encrypted === vector.ciphertext;

                let roundtripPassed = true;
                if (encryptPassed) {
                    const decrypted = cipher.decrypt(encrypted);
                    roundtripPassed = decrypted === vector.plaintext;
                }

                const testPassed = encryptPassed && roundtripPassed;

                if (testPassed) {
                    passed++;
                    if (opts.verbose) {
                        console.log(`Sample ${vector.sample}: PASS`);
                    }
                } else {
                    failed++;
                    if (!opts.quiet) {
                        console.log(`Sample ${vector.sample}: FAIL`);
                        if (!encryptPassed) {
                            console.log(`  Expected: ${vector.ciphertext}`);
                            console.log(`  Got:      ${encrypted}`);
                        }
                        if (!roundtripPassed) {
                            console.log('  Round-trip failed');
                        }
                    }
                }
            } catch (error) {
                failed++;
                if (!opts.quiet) {
                    console.log(`Sample ${vector.sample}: ERROR - ${error.message}`);
                }
            }
        }

        if (!opts.quiet) {
            console.log('\n========================================');
            console.log(`Results: ${passed}/${vectors.length} passed\n`);
        }

        if (failed === 0) {
            if (!opts.quiet) {
                console.log('ALL NIST TEST VECTORS PASSED!\n');
                console.log('WARNING: FF3 was WITHDRAWN by NIST due to security vulnerabilities.');
                console.log('This implementation is for EDUCATIONAL and RESEARCH purposes only.');
                console.log('DO NOT use in production systems.\n');
            }
            process.exit(0);
        } else {
            if (!opts.quiet) {
                console.log('VALIDATION FAILED\n');
            }
            process.exit(1);
        }

    } catch (error) {
        console.error(`Error: ${error.message}`);
        process.exit(2);
    }
}

main();
