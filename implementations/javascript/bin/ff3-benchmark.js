#!/usr/bin/env node

/**
 * FF3 Performance Benchmark Tool
 *
 * Measures FF3 encryption/decryption performance across different string lengths.
 *
 * ⚠️ FF3 was withdrawn by NIST due to security vulnerabilities.
 * This implementation is for educational and research purposes only.
 */

import fs from 'fs';
import os from 'os';
import {
    digits,
    hexLower,
    base36Lower,
    base62,
    hexToBytes,
    VERSION
} from '../lib/index.js';

/**
 * Default configuration
 */
const DEFAULT_CONFIG = {
    alphabet: 'digits',
    radix: 10,
    lengths: [9, 12, 16],
    cases: ['enc', 'dec', 'roundtrip'],
    iterations: 100000,
    warmup: 10000,
    key: 'EF4359D8D580AA4F7F036D6F04FC6A94',
    tweak: 'D8E7920AFA330A73',
    seed: 42,
    verbose: false,
    jsonOut: null
};

/**
 * Parse CSV string to integer array
 */
function parseCsvInts(csv) {
    return csv.split(',').map(s => parseInt(s.trim(), 10));
}

/**
 * Parse CSV string to string array
 */
function parseCsvStrings(csv) {
    return csv.split(',').map(s => s.trim());
}

/**
 * Simple seeded random number generator (LCG)
 */
function createSeededRandom(seed) {
    let state = seed;
    return function() {
        state = (state * 1103515245 + 12345) & 0x7fffffff;
        return state / 0x7fffffff;
    };
}

/**
 * Generate deterministic test inputs with seed
 */
function generateInputs(alphabet, length, count, seed) {
    const random = createSeededRandom(seed);
    const inputs = [];
    const chars = alphabet.split('');

    for (let i = 0; i < count; i++) {
        let input = '';
        for (let j = 0; j < length; j++) {
            const idx = Math.floor(random() * chars.length);
            input += chars[idx];
        }
        inputs.push(input);
    }

    return inputs;
}

/**
 * Build cipher based on alphabet name
 */
function buildCipher(alphabetName, key, tweak) {
    const keyBytes = hexToBytes(key);
    const tweakBytes = hexToBytes(tweak);

    switch (alphabetName) {
        case 'digits':
            return digits(keyBytes, tweakBytes);
        case 'hex':
            return hexLower(keyBytes, tweakBytes);
        case 'base36':
            return base36Lower(keyBytes, tweakBytes);
        case 'base62':
            return base62(keyBytes, tweakBytes);
        default:
            throw new Error(`Unknown alphabet: ${alphabetName}`);
    }
}

/**
 * Get alphabet character set for input generation
 */
function getAlphabetChars(alphabetName) {
    switch (alphabetName) {
        case 'digits':
            return '0123456789';
        case 'hex':
            return '0123456789abcdef';
        case 'base36':
            return '0123456789abcdefghijklmnopqrstuvwxyz';
        case 'base62':
            return '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';
        default:
            throw new Error(`Unknown alphabet: ${alphabetName}`);
    }
}

/**
 * Get radix for alphabet
 */
function getRadixForAlphabet(alphabetName) {
    const chars = getAlphabetChars(alphabetName);
    return chars.length;
}

/**
 * Run benchmark for a specific case
 */
function runBenchmark(cipher, inputs, caseType, iterations, verbose) {
    const times = [];
    let checksum = 0;

    // Precompute ciphertexts for decrypt benchmarks
    const ciphertexts = inputs.map(pt => cipher.encrypt(pt));

    for (let i = 0; i < iterations; i++) {
        const input = inputs[i % inputs.length];
        const ciphertext = ciphertexts[i % ciphertexts.length];

        const start = process.hrtime.bigint();

        let result;
        switch (caseType) {
            case 'enc':
                result = cipher.encrypt(input);
                break;
            case 'dec':
                result = cipher.decrypt(ciphertext);
                break;
            case 'roundtrip':
                const ct = cipher.encrypt(input);
                result = cipher.decrypt(ct);
                if (result !== input) {
                    throw new Error(`Round-trip verification failed: ${input} != ${result}`);
                }
                break;
        }

        const end = process.hrtime.bigint();
        times.push(Number(end - start));

        // XOR checksum to prevent dead code elimination
        for (let j = 0; j < result.length; j++) {
            checksum ^= result.charCodeAt(j);
        }

        if (verbose && (i + 1) % 10000 === 0) {
            console.error(`  Progress: ${i + 1}/${iterations}`);
        }
    }

    const totalNs = times.reduce((a, b) => a + b, 0);
    const avgNs = totalNs / times.length;
    const opsPerSec = Math.round(1e9 / avgNs);

    return {
        elapsed_ns: Math.round(totalNs),
        ns_per_op: Math.round(avgNs),
        ops_per_sec: opsPerSec,
        checksum: checksum.toString(16).padStart(8, '0')
    };
}

/**
 * Parse command-line arguments
 */
function parseArgs(args) {
    const config = { ...DEFAULT_CONFIG };

    for (let i = 0; i < args.length; i++) {
        const arg = args[i];

        switch (arg) {
            case '-h':
            case '--help':
                showHelp();
                process.exit(0);
                break;

            case '-v':
            case '--version':
                console.log(`FF3 Benchmark Tool v${VERSION}`);
                process.exit(0);
                break;

            case '--config':
                if (i + 1 >= args.length) {
                    console.error('ERR: --config requires a file path');
                    process.exit(2);
                }
                const configPath = args[++i];
                try {
                    const fileContent = fs.readFileSync(configPath, 'utf8');
                    const fileConfig = JSON.parse(fileContent);
                    Object.assign(config, fileConfig);
                } catch (err) {
                    console.error(`ERR: Failed to load config file: ${err.message}`);
                    process.exit(2);
                }
                break;

            case '--alphabet':
                if (i + 1 >= args.length) {
                    console.error('ERR: --alphabet requires a value');
                    process.exit(2);
                }
                config.alphabet = args[++i];
                break;

            case '--radix':
                if (i + 1 >= args.length) {
                    console.error('ERR: --radix requires a value');
                    process.exit(2);
                }
                config.radix = parseInt(args[++i], 10);
                break;

            case '--lengths':
                if (i + 1 >= args.length) {
                    console.error('ERR: --lengths requires a value');
                    process.exit(2);
                }
                config.lengths = parseCsvInts(args[++i]);
                break;

            case '--cases':
                if (i + 1 >= args.length) {
                    console.error('ERR: --cases requires a value');
                    process.exit(2);
                }
                config.cases = parseCsvStrings(args[++i]);
                break;

            case '--iterations':
                if (i + 1 >= args.length) {
                    console.error('ERR: --iterations requires a value');
                    process.exit(2);
                }
                config.iterations = parseInt(args[++i], 10);
                break;

            case '--warmup':
                if (i + 1 >= args.length) {
                    console.error('ERR: --warmup requires a value');
                    process.exit(2);
                }
                config.warmup = parseInt(args[++i], 10);
                break;

            case '--key':
                if (i + 1 >= args.length) {
                    console.error('ERR: --key requires a value');
                    process.exit(2);
                }
                config.key = args[++i];
                break;

            case '--tweak':
                if (i + 1 >= args.length) {
                    console.error('ERR: --tweak requires a value');
                    process.exit(2);
                }
                config.tweak = args[++i];
                break;

            case '--seed':
                if (i + 1 >= args.length) {
                    console.error('ERR: --seed requires a value');
                    process.exit(2);
                }
                config.seed = parseInt(args[++i], 10);
                break;

            case '--verbose':
                config.verbose = true;
                break;

            case '--json-out':
                if (i + 1 >= args.length) {
                    console.error('ERR: --json-out requires a file path');
                    process.exit(2);
                }
                config.jsonOut = args[++i];
                break;

            case '--quick':
                config.iterations = 10000;
                config.warmup = 1000;
                config.lengths = [9, 12];
                config.cases = ['enc', 'roundtrip'];
                break;

            default:
                console.error(`ERR: Unknown option: ${arg}`);
                showHelp();
                process.exit(1);
        }
    }

    return config;
}

/**
 * Display help information
 */
function showHelp() {
    console.log(`
FF3 Performance Benchmark Tool v${VERSION}

Usage: ff3-benchmark [options]

Options:
  --config FILE         Load configuration from JSON file
  --alphabet NAME       Alphabet to use (digits, hex, base36, base62)
  --radix N             Radix (2-62)
  --lengths CSV         Comma-separated input lengths (e.g., "9,12,16")
  --cases CSV           Comma-separated cases (enc,dec,roundtrip)
  --iterations N        Number of iterations (default: 100000)
  --warmup N            Warmup iterations (default: 10000)
  --key HEX             Encryption key (hex string)
  --tweak HEX           Tweak value (hex string)
  --seed N              Random seed for input generation
  --verbose             Show progress during benchmarks
  --json-out FILE       Write JSON results to file
  --quick               Quick benchmark (fewer iterations)
  -h, --help            Show this help message
  -v, --version         Show version

This tool measures FF3 encryption/decryption performance with different
string lengths and outputs detailed timing statistics.

⚠️  FF3 was withdrawn by NIST due to security vulnerabilities.
   This tool is for educational and research purposes only.
`);
}

/**
 * Main function
 */
function main() {
    const config = parseArgs(process.argv.slice(2));

    if (config.verbose) {
        console.error(`
🚀 FF3 Performance Benchmark Tool v${VERSION}
============================================

⚠️  FF3 was WITHDRAWN by NIST due to security vulnerabilities.
    This benchmark is for EDUCATIONAL and RESEARCH purposes only.

🔧 Configuration:
   Alphabet: ${config.alphabet}
   Radix: ${config.radix}
   Key: ${config.key.substring(0, 16)}...
   Tweak: ${config.tweak}
   Lengths: ${config.lengths.join(', ')}
   Cases: ${config.cases.join(', ')}
   Iterations: ${config.iterations}
   Warmup: ${config.warmup}
   Seed: ${config.seed}

Starting performance benchmarks...
`);
    }

    try {
        // Build cipher
        const cipher = buildCipher(config.alphabet, config.key, config.tweak);
        const alphabetChars = getAlphabetChars(config.alphabet);
        const radix = getRadixForAlphabet(config.alphabet);

        const benchmarks = [];

        // Run benchmarks for each length
        for (const length of config.lengths) {
            // Generate ring buffer of 64 varied inputs
            const inputs = generateInputs(alphabetChars, length, 64, config.seed);

            // Warmup phase
            if (config.warmup > 0) {
                if (config.verbose) {
                    console.error(`\n📊 Warmup for length ${length}...`);
                }
                runBenchmark(cipher, inputs, 'enc', config.warmup, false);
            }

            // Benchmark each case
            for (const caseType of config.cases) {
                if (config.verbose) {
                    console.error(`\n📊 Benchmarking ${caseType}_length${length}_radix${radix}...`);
                }

                const result = runBenchmark(cipher, inputs, caseType, config.iterations, config.verbose);

                benchmarks.push({
                    name: `${caseType}_length${length}_radix${radix}`,
                    n: config.iterations,
                    elapsed_ns: result.elapsed_ns,
                    ns_per_op: result.ns_per_op,
                    ops_per_sec: result.ops_per_sec,
                    checksum: result.checksum
                });

                if (config.verbose) {
                    console.error(`   ${result.ns_per_op} ns/op (${result.ops_per_sec.toLocaleString()} ops/sec)`);
                    console.error(`   Checksum: ${result.checksum}`);
                }
            }
        }

        // Build spec-compliant JSON output
        const totalDuration = benchmarks.reduce((sum, b) => sum + b.elapsed_ns, 0) / 1e9;
        const combinedChecksum = benchmarks.reduce((xor, b) => xor ^ (parseInt(b.checksum || '0', 16)), 0);

        const output = {
            metadata: {
                version: '1.0',
                timestamp: new Date().toISOString(),
                language: 'javascript',
                runtime: `Node.js ${process.version}`,
                platform: {
                    os: os.platform(),
                    arch: os.arch(),
                    cpu: os.cpus()[0]?.model || 'unknown',
                    cores: os.cpus().length
                }
            },
            configuration: {
                seed: config.seed,
                warmup_iterations: config.warmup
            },
            benchmarks: benchmarks.map(b => {
                const nameParts = b.name.split('_');
                const testCase = nameParts[0];
                const lengthStr = nameParts[1];
                const length = parseInt(lengthStr.replace('length', ''));

                return {
                    name: b.name.replace('length', 'len'),
                    test_case: testCase,
                    parameters: {
                        alphabet: config.alphabet,
                        radix: radix,
                        length: length,
                        key_bits: config.key.length * 4,
                        key_fingerprint: config.key.substring(0, 8).toUpperCase(),
                        tweak: config.tweak.toUpperCase()
                    },
                    iterations: b.n,
                    elapsed_ns: b.elapsed_ns,
                    ns_per_op: b.ns_per_op,
                    ops_per_sec: b.ops_per_sec,
                    checksum: b.checksum || '00000000'
                };
            }),
            summary: {
                total_tests: benchmarks.length,
                total_duration_sec: totalDuration,
                checksum: combinedChecksum.toString(16).padStart(8, '0')
            }
        };

        const jsonOutput = JSON.stringify(output, null, 2);

        if (config.jsonOut) {
            fs.writeFileSync(config.jsonOut, jsonOutput);
            if (config.verbose) {
                console.error(`\n✅ Results written to ${config.jsonOut}`);
            }
        } else {
            console.log(jsonOutput);
        }

        if (config.verbose) {
            console.error(`
⚠️  FF3 Format Preserving Encryption Library v${VERSION}

    FF3 was WITHDRAWN by NIST due to security vulnerabilities.
    This implementation is for EDUCATIONAL and RESEARCH purposes only.

    DO NOT use in production systems.
`);
        }

    } catch (error) {
        console.error('❌ Benchmark failed:', error.message);
        if (config.verbose) {
            console.error(error.stack);
        }
        process.exit(1);
    }
}

// Run main function
main();