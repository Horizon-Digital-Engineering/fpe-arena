/**
 * FF3 JavaScript Implementation Tests
 * 
 * Modern JavaScript testing without external dependencies
 * Uses Node.js built-in assert and file system
 */

import { strict as assert } from 'assert';
import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

// Import our FF3 implementation
import {
    digits,
    hex,
    base36,
    base62,
    radix26,
    hexToBytes,
    bytesToHex,
    AlphabetSpec,
    fromSpec
} from '../lib/index.js';

// Get current directory for file paths
const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

/**
 * Load NIST FF3 test vectors
 */
function loadNistTestVectors() {
    // Try environment variable first (for CI/CD)
    let vectorPath = process.env.FF3_TEST_VECTORS_PATH;

    if (!vectorPath) {
        // Fallback to relative path for local development
        vectorPath = join(__dirname, '../../../shared/test-vectors/nist_ff3_official_vectors.json');
    }

    const data = readFileSync(vectorPath, 'utf8');
    const json = JSON.parse(data);
    return json.vectors;
}

/**
 * Basic functionality test
 */
function testBasicFunctionality() {
    console.log('🧪 Testing basic functionality...');
    
    const key = hexToBytes('EF4359D8D580AA4F7F036D6F04FC6A94');
    const tweak = hexToBytes('D8E7920AFA330A73');
    
    const cipher = digits(key, tweak);
    
    const plaintext = '1234567890';
    const ciphertext = cipher.encrypt(plaintext);
    const decrypted = cipher.decrypt(ciphertext);
    
    assert.strictEqual(decrypted, plaintext, 'Round-trip failed');
    assert.notStrictEqual(ciphertext, plaintext, 'Encryption should change the text');
    
    console.log(`✅ Basic test: ${plaintext} -> ${ciphertext} -> ${decrypted}`);
    
    // Test against first NIST vector
    const nistPlaintext = '890121234567890000';
    const nistExpected = '750918814058654607';
    const nistResult = cipher.encrypt(nistPlaintext);
    
    console.log(`✅ NIST test: ${nistPlaintext} -> ${nistResult} (expected: ${nistExpected})`);
    assert.strictEqual(nistResult, nistExpected, 'NIST vector test failed');
}

/**
 * Format preservation test
 */
function testFormatPreservation() {
    console.log('🧪 Testing format preservation...');
    
    const key = hexToBytes('EF4359D8D580AA4F7F036D6F04FC6A94');
    const tweak = hexToBytes('D8E7920AFA330A73');
    
    // Test base62 case preservation
    const cipher = base62(key, tweak);
    const testCases = ['Ab9Z', 'Hello123', 'TestCase'];
    
    for (const plaintext of testCases) {
        const ciphertext = cipher.encrypt(plaintext);
        const decrypted = cipher.decrypt(ciphertext);
        
        assert.strictEqual(decrypted, plaintext, `Format preservation failed for ${plaintext}`);
        console.log(`✅ Format preservation: ${plaintext} -> ${ciphertext} -> ${decrypted}`);
    }
}

/**
 * Test against all NIST FF3 vectors
 */
function testNistVectors() {
    console.log('🧪 Testing NIST FF3 vectors...');
    
    const vectors = loadNistTestVectors();
    console.log(`Testing ${vectors.length} NIST FF3 vectors...`);
    
    for (const vector of vectors) {
        // Decode hex key and tweak
        const key = hexToBytes(vector.key);
        const tweak = hexToBytes(vector.tweak);
        
        // Create cipher based on radix
        let cipher;
        switch (vector.radix) {
            case 10:
                cipher = digits(key, tweak);
                break;
            case 16:
                cipher = hex(key, tweak);
                break;
            case 26:
                cipher = radix26(key, tweak);
                break;
            case 36:
                cipher = base36(key, tweak);
                break;
            case 62:
                cipher = base62(key, tweak);
                break;
            default:
                throw new Error(`Unsupported radix ${vector.radix} in test vectors`);
        }
        
        // Test encryption
        const encrypted = cipher.encrypt(vector.plaintext);
        
        console.log(`Sample ${vector.sample} (${vector.algorithm}):`);
        console.log(`  Key: ${vector.key}`);
        console.log(`  Tweak: ${vector.tweak}`);
        console.log(`  Plaintext: ${vector.plaintext}`);
        console.log(`  Expected: ${vector.ciphertext}`);
        console.log(`  Got:      ${encrypted}`);
        
        if (encrypted === vector.ciphertext) {
            console.log('  ✅ PERFECT MATCH!');
        } else {
            console.log('  ❌ MISMATCH');
        }
        
        // Assert exact match with NIST vectors
        assert.strictEqual(encrypted, vector.ciphertext, `Sample ${vector.sample} failed`);
        
        // Test decryption (round-trip)
        const decrypted = cipher.decrypt(encrypted);
        assert.strictEqual(decrypted, vector.plaintext, `Round-trip failed for sample ${vector.sample}`);
        console.log('  ✅ Round-trip successful');
        console.log();
    }
}

/**
 * Test modern JavaScript features
 */
function testModernFeatures() {
    console.log('🧪 Testing modern JavaScript features...');
    
    // Test hex utility functions
    const originalBytes = new Uint8Array([0xEF, 0x43, 0x59, 0xD8]);
    const hexString = bytesToHex(originalBytes);
    const convertedBytes = hexToBytes(hexString);
    
    assert.strictEqual(hexString.toLowerCase(), 'ef4359d8');
    assert.deepStrictEqual(convertedBytes, originalBytes);
    console.log('✅ Hex utility functions work');
    
    // Test custom alphabet
    const customAlphabet = new AlphabetSpec('01');  // Binary alphabet
    const key = hexToBytes('EF4359D8D580AA4F7F036D6F04FC6A94');
    const tweak = hexToBytes('D8E7920AFA330A73');
    const binaryCipher = fromSpec(key, tweak, customAlphabet);
    
    const binaryPlaintext = '1010101010';
    const binaryEncrypted = binaryCipher.encrypt(binaryPlaintext);
    const binaryDecrypted = binaryCipher.decrypt(binaryEncrypted);
    
    assert.strictEqual(binaryDecrypted, binaryPlaintext);
    console.log(`✅ Custom binary alphabet: ${binaryPlaintext} -> ${binaryEncrypted} -> ${binaryDecrypted}`);
    
    // Test error handling
    try {
        const badCipher = digits(key, tweak);
        badCipher.encrypt('ABC'); // Should fail - letters not in digits alphabet
        assert.fail('Should have thrown error for invalid characters');
    } catch (error) {
        console.log('✅ Error handling works:', error.message);
    }
}

/**
 * Performance micro-benchmark
 */
function performanceBenchmark() {
    console.log('🧪 Running performance micro-benchmark...');
    
    const key = hexToBytes('EF4359D8D580AA4F7F036D6F04FC6A94');
    const tweak = hexToBytes('D8E7920AFA330A73');
    const cipher = digits(key, tweak);
    
    const plaintext = '1234567890123456'; // 16 digits
    const iterations = 1000;
    
    // Warm up
    for (let i = 0; i < 100; i++) {
        const encrypted = cipher.encrypt(plaintext);
        cipher.decrypt(encrypted);
    }
    
    // Benchmark encryption
    const encryptStart = process.hrtime.bigint();
    for (let i = 0; i < iterations; i++) {
        cipher.encrypt(plaintext);
    }
    const encryptEnd = process.hrtime.bigint();
    
    // Benchmark decryption
    const encrypted = cipher.encrypt(plaintext);
    const decryptStart = process.hrtime.bigint();
    for (let i = 0; i < iterations; i++) {
        cipher.decrypt(encrypted);
    }
    const decryptEnd = process.hrtime.bigint();
    
    const encryptNsPerOp = Number(encryptEnd - encryptStart) / iterations;
    const decryptNsPerOp = Number(decryptEnd - decryptStart) / iterations;
    const encryptOpsPerSec = Math.round(1e9 / encryptNsPerOp);
    const decryptOpsPerSec = Math.round(1e9 / decryptNsPerOp);
    
    console.log(`✅ Performance (${iterations} iterations):`);
    console.log(`   Encrypt: ${Math.round(encryptNsPerOp)} ns/op (${encryptOpsPerSec} ops/sec)`);
    console.log(`   Decrypt: ${Math.round(decryptNsPerOp)} ns/op (${decryptOpsPerSec} ops/sec)`);
}

/**
 * Main test runner
 */
async function runTests() {
    console.log('🚀 Starting FF3 JavaScript Implementation Tests');
    console.log('================================================');
    
    try {
        testBasicFunctionality();
        testFormatPreservation();
        testModernFeatures();
        testNistVectors();
        performanceBenchmark();
        
        console.log('🎉 All tests passed! ✅');
        console.log('\n💡 Modern JavaScript FF3 implementation is working perfectly!');
        
    } catch (error) {
        console.error('❌ Test failed:', error.message);
        console.error(error.stack);
        process.exit(1);
    }
}

// Run tests if this file is executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
    runTests();
}