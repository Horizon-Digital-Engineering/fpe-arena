#!/usr/bin/env node

import { randomBytes } from 'crypto';
import {
  digits,
  hexLower,
  hexUpper,
  base36Lower,
  base36Upper,
  base62
} from '../lib/index.js';

const DEFAULT_ITERATIONS = 1000;
const DEFAULT_MIN_LENGTH = 6;
const DEFAULT_MAX_LENGTH = 20;
const DEFAULT_ALPHABETS = ['digits', 'hex-lower', 'base36-lower', 'base62'];

const ALPHABETS = new Map([
  ['digits', '0123456789'],
  ['hex-lower', '0123456789abcdef'],
  ['hex-upper', '0123456789ABCDEF'],
  ['base36-lower', '0123456789abcdefghijklmnopqrstuvwxyz'],
  ['base36-upper', '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'],
  ['base62', '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz']
]);

function printUsage() {
  console.log('FF3 Stress Test Tool\n');
  console.log('Usage: ff3-stresstest [OPTIONS] [ITERATIONS]\n');
  console.log('Options:');
  console.log('  --alphabets LIST      Comma-separated list (default: digits,hex-lower,base36-lower,base62)');
  console.log('  --min-length N        Minimum plaintext length (default: 6)');
  console.log('  --max-length N        Maximum plaintext length (default: 20)');
  console.log('  --quick               Run 100 iterations (fast test)');
  console.log('  --seed N              Random seed for reproducibility');
}

function parseArgs(argv) {
  const opts = {
    iterations: DEFAULT_ITERATIONS,
    alphabets: [...DEFAULT_ALPHABETS],
    minLength: DEFAULT_MIN_LENGTH,
    maxLength: DEFAULT_MAX_LENGTH,
    quick: false,
    seed: undefined,
    help: false
  };

  for (let i = 0; i < argv.length; i++) {
    const arg = argv[i];
    switch (arg) {
      case '--help':
      case '-h':
        opts.help = true;
        return opts;
      case '--alphabets':
        if (i + 1 >= argv.length) throw new Error('--alphabets requires a value');
        opts.alphabets = argv[++i].split(',').map(s => s.trim()).filter(Boolean);
        if (opts.alphabets.length === 0) throw new Error('--alphabets list cannot be empty');
        break;
      case '--min-length':
        if (i + 1 >= argv.length) throw new Error('--min-length requires a value');
        opts.minLength = parseInt(argv[++i], 10);
        break;
      case '--max-length':
        if (i + 1 >= argv.length) throw new Error('--max-length requires a value');
        opts.maxLength = parseInt(argv[++i], 10);
        break;
      case '--quick':
        opts.quick = true;
        break;
      case '--seed':
        if (i + 1 >= argv.length) throw new Error('--seed requires a value');
        opts.seed = BigInt(parseInt(argv[++i], 10));
        break;
      default:
        if (arg.startsWith('--')) {
          throw new Error(`Unknown option: ${arg}`);
        }
        const iters = parseInt(arg, 10);
        if (!Number.isInteger(iters) || iters <= 0) {
          throw new Error(`Invalid iterations value: ${arg}`);
        }
        opts.iterations = iters;
    }
  }

  if (opts.quick) {
    opts.iterations = 100;
  }

  return opts;
}

function validateOptions(opts) {
  if (opts.iterations <= 0) throw new Error('iterations must be greater than 0');
  if (opts.minLength < 2) throw new Error('--min-length must be at least 2');
  if (opts.maxLength < opts.minLength) throw new Error('--max-length must be greater than or equal to --min-length');
  for (const name of opts.alphabets) {
    if (!ALPHABETS.has(name)) {
      throw new Error(`Unknown alphabet: ${name}`);
    }
  }
}

function createCipherFactory(name) {
  switch (name) {
    case 'digits': return digits;
    case 'hex-lower': return hexLower;
    case 'hex-upper': return hexUpper;
    case 'base36-lower': return base36Lower;
    case 'base36-upper': return base36Upper;
    case 'base62': return base62;
    default: throw new Error(`Unknown alphabet: ${name}`);
  }
}

function toHex(buffer) {
  return Buffer.from(buffer).toString('hex');
}

function createRng(seed) {
  if (seed === undefined) {
    return {
      bytes: n => randomBytes(n),
      int: (min, max) => Math.floor(Math.random() * (max - min + 1)) + min
    };
  }

  let state = seed & ((1n << 48n) - 1n);
  const modulus = 1n << 48n;
  const multiplier = 25214903917n;
  const increment = 11n;

  function next() {
    state = (state * multiplier + increment) % modulus;
    return Number(state >> 16n) & 0x7fffff;
  }

  return {
    bytes: n => {
      const buf = Buffer.allocUnsafe(n);
      for (let i = 0; i < n; i++) {
        buf[i] = next() & 0xff;
      }
      return buf;
    },
    int: (min, max) => {
      const range = max - min + 1;
      return min + (next() % range);
    }
  };
}

function stressAlphabet(name, charset, factory, opts, rng) {
  console.log(`Testing ${name}...`);
  console.log(`  Alphabet: ${charset} (radix ${charset.length})`);

  let passed = 0;
  let failed = 0;
  const interval = Math.max(1, Math.floor(opts.iterations / 10));

  for (let i = 0; i < opts.iterations; i++) {
    const key = rng.bytes(16);
    const tweak = rng.bytes(8);
    const length = rng.int(opts.minLength, opts.maxLength);
    const plaintext = generatePlaintext(charset, length, rng);

    try {
      const cipher = factory(key, tweak);
      const ciphertext = cipher.encrypt(plaintext);
      const decrypted = cipher.decrypt(ciphertext);
      if (decrypted !== plaintext) {
        failed += 1;
        printFailure(key, tweak, plaintext, ciphertext, decrypted, 'round-trip mismatch');
      } else {
        passed += 1;
      }
    } catch (err) {
      failed += 1;
      printFailure(key, tweak, plaintext, '', null, err.message);
    }

    if ((i + 1) % interval === 0 || i + 1 === opts.iterations) {
      const percent = Math.floor(((i + 1) * 100) / opts.iterations);
      console.log(`  Progress: ${i + 1}/${opts.iterations} (${percent}%)`);
    }
  }

  console.log(`  Passed: ${passed}/${opts.iterations}`);
  console.log(`  Failed: ${failed}/${opts.iterations}\n`);
  return { tests: opts.iterations, failures: failed };
}

function generatePlaintext(charset, length, rng) {
  const chars = [];
  for (let i = 0; i < length; i++) {
    const idx = rng.int(0, charset.length - 1);
    chars.push(charset[idx]);
  }
  return chars.join('');
}

function printFailure(key, tweak, plain, cipher, decrypted, detail) {
  console.log('  Round-trip failed:');
  console.log(`    Key: ${toHex(key)}`);
  console.log(`    Tweak: ${toHex(tweak)}`);
  console.log(`    Plaintext: "${plain}"`);
  if (cipher) {
    console.log(`    Ciphertext: "${cipher}"`);
  }
  if (decrypted) {
    console.log(`    Decrypted: "${decrypted}"`);
  }
  if (detail) {
    console.log(`    Detail: ${detail}`);
  }
}

function main() {
  let opts;
  try {
    opts = parseArgs(process.argv.slice(2));
  } catch (err) {
    console.error(`Error: ${err.message}`);
    process.exit(1);
  }

  if (opts.help) {
    printUsage();
    return;
  }

  try {
    validateOptions(opts);
  } catch (err) {
    console.error(`Error: ${err.message}`);
    process.exit(1);
  }

  const rng = createRng(opts.seed);

  console.log('FF3 Stress Test v1.0');
  console.log('====================\n');
  console.log('Warning: FF3 was withdrawn by NIST; run for education and research only.\n');
  console.log('Test configuration');
  console.log(`  Iterations per alphabet: ${opts.iterations}`);
  console.log('  Random key/tweak generation: enabled');
  console.log(`  String length range: ${opts.minLength}-${opts.maxLength} characters`);
  console.log(`  Alphabets: ${opts.alphabets.join(', ')}\n`);

  const start = Date.now();
  let totalTests = 0;
  let totalFailures = 0;

  for (const name of opts.alphabets) {
    const charset = ALPHABETS.get(name);
    const factory = createCipherFactory(name);
    const result = stressAlphabet(name, charset, factory, opts, rng);
    totalTests += result.tests;
    totalFailures += result.failures;
  }

  const duration = Date.now() - start;
  console.log('Summary');
  console.log(`  Total tests: ${totalTests}`);
  console.log(`  Failures: ${totalFailures}`);
  console.log(`  Duration: ${duration} ms`);
  if (duration > 0) {
    const throughput = (totalTests * 1000) / duration;
    console.log(`  Throughput: ${throughput.toFixed(2)} tests/sec`);
  }
  if (totalFailures === 0) {
    console.log('  Result: all stress tests passed');
  } else {
    console.log('  Result: failures detected');
    process.exit(1);
  }
}

main();
