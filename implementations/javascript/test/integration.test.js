/**
 * FF3 JavaScript Integration Tests
 *
 * End-to-end integration tests including NIST vector validation.
 */

import { strict as assert } from 'assert';
import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';
import {
    digits,
    hexLower,
    base36Lower,
    base62,
    radix26,
    fromSpec,
    AlphabetSpec,
    hexToBytes
} from '../lib/index.js';

// Get current directory for file paths
const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

/**
 * Load NIST FF3 test vectors
 */
function loadNistTestVectors() {
    try {
        // Try environment variable first (for CI/CD)
        let vectorPath = process.env.FF3_TEST_VECTORS_PATH;

        if (!vectorPath) {
            // Fallback to relative path for local development
            vectorPath = join(__dirname, '../../../shared/test-vectors/nist_ff3_official_vectors.json');
        }

        const data = readFileSync(vectorPath, 'utf8');
        const json = JSON.parse(data);
        return json.vectors;
    } catch (error) {
        console.log('⚠️  NIST test vectors not found - skipping NIST tests');
        console.log(`     Tried: ${process.env.FF3_TEST_VECTORS_PATH || 'relative path'}`);
        return [];
    }
}

/**
 * Test complete public API integration
 */
function testPublicApiIntegration() {
    console.log('🧪 Testing public API integration...');

    const key = hexToBytes('EF4359D8D580AA4F7F036D6F04FC6A94');
    const tweak = hexToBytes('D8E7920AFA330A73');

    // Test multiple alphabet types in integration
    const testCases = [
        { name: 'digits', factory: digits, input: '1234567890123456' },
        { name: 'hexLower', factory: hexLower, input: '1234abcdef567890' },
        { name: 'base62', factory: base62, input: 'Hello123World456' },
        { name: 'radix26', factory: radix26, input: '0123456789abcdef' }
    ];

    for (const testCase of testCases) {
        const cipher = testCase.factory(key, tweak);
        const ciphertext = cipher.encrypt(testCase.input);
        const decrypted = cipher.decrypt(ciphertext);

        assert.strictEqual(decrypted, testCase.input, `${testCase.name} round-trip failed`);
        assert.notStrictEqual(ciphertext, testCase.input, `${testCase.name} should encrypt`);

        console.log(`   ✅ ${testCase.name}: ${testCase.input} -> ${ciphertext} -> ${decrypted}`);
    }

    console.log('✅ Public API integration tests passed');
}

/**
 * Test custom alphabet integration
 */
function testCustomAlphabetIntegration() {
    console.log('🧪 Testing custom alphabet integration...');

    const key = hexToBytes('EF4359D8D580AA4F7F036D6F04FC6A94');
    const tweak = hexToBytes('D8E7920AFA330A73');

    // Test various custom alphabets
    const customTests = [
        { name: 'binary', alphabet: '01', input: '1010101010' },
        { name: 'symbols', alphabet: '!@#$%^&*()', input: '!@#$%^&*' },
        { name: 'vowels', alphabet: 'aeiou', input: 'aeiouaeiou' }
    ];

    for (const test of customTests) {
        const spec = new AlphabetSpec(test.alphabet);
        const cipher = fromSpec(key, tweak, spec);
        const ciphertext = cipher.encrypt(test.input);
        const decrypted = cipher.decrypt(ciphertext);

        assert.strictEqual(decrypted, test.input, `${test.name} custom alphabet round-trip failed`);
        console.log(`   ✅ ${test.name}: ${test.input} -> ${ciphertext} -> ${decrypted}`);
    }

    console.log('✅ Custom alphabet integration tests passed');
}

/**
 * Test NIST FF3 vectors integration
 */
function testNistVectorsIntegration() {
    console.log('🧪 Testing NIST FF3 vectors integration...');

    const vectors = loadNistTestVectors();

    if (vectors.length === 0) {
        console.log('⚠️  Skipping NIST tests - vectors not available');
        return;
    }

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
                cipher = hexLower(key, tweak); // NIST uses lowercase hex
                break;
            case 26:
                cipher = radix26(key, tweak);
                break;
            case 36:
                cipher = base36Lower(key, tweak);
                break;
            case 62:
                cipher = base62(key, tweak);
                break;
            default:
                throw new Error(`Unsupported radix ${vector.radix} in test vectors`);
        }

        // Test encryption matches NIST expected result
        const encrypted = cipher.encrypt(vector.plaintext);
        assert.strictEqual(encrypted, vector.ciphertext, `NIST Sample ${vector.sample} encryption failed`);

        // Test decryption (round-trip)
        const decrypted = cipher.decrypt(encrypted);
        assert.strictEqual(decrypted, vector.plaintext, `NIST Sample ${vector.sample} round-trip failed`);

        console.log(`   ✅ Sample ${vector.sample}: "${vector.plaintext}" -> "${encrypted}" -> "${decrypted}"`);
    }

    console.log(`✅ All ${vectors.length} NIST vectors passed!`);
}

/**
 * Test cross-alphabet consistency
 */
function testCrossAlphabetConsistency() {
    console.log('🧪 Testing cross-alphabet consistency...');

    const key = hexToBytes('EF4359D8D580AA4F7F036D6F04FC6A94');
    const tweak = hexToBytes('D8E7920AFA330A73');

    // Test that same input produces different outputs across different alphabets
    const input = '12345678';
    const ciphers = [
        { name: 'digits', cipher: digits(key, tweak) },
        { name: 'radix26', cipher: radix26(key, tweak) }
    ];

    const results = [];
    for (const { name, cipher } of ciphers) {
        const encrypted = cipher.encrypt(input);
        const decrypted = cipher.decrypt(encrypted);

        assert.strictEqual(decrypted, input, `${name} round-trip failed`);
        results.push({ name, encrypted });
        console.log(`   ${name}: ${input} -> ${encrypted}`);
    }

    // Different alphabets should produce different encryptions
    assert.notStrictEqual(results[0].encrypted, results[1].encrypted, 'Different alphabets should produce different outputs');

    console.log('✅ Cross-alphabet consistency tests passed');
}

/**
 * Test performance and stability
 */
function testPerformanceStability() {
    console.log('🧪 Testing performance and stability...');

    const key = hexToBytes('EF4359D8D580AA4F7F036D6F04FC6A94');
    const tweak = hexToBytes('D8E7920AFA330A73');
    const cipher = digits(key, tweak);

    const testStrings = [
        '123456',      // Minimum length
        '1234567890',  // Common length
        '12345678901234567890' // Longer string
    ];

    const iterations = 100;

    for (const testString of testStrings) {
        console.log(`   Testing ${testString.length}-char string (${iterations} iterations)...`);

        for (let i = 0; i < iterations; i++) {
            const encrypted = cipher.encrypt(testString);
            const decrypted = cipher.decrypt(encrypted);

            assert.strictEqual(decrypted, testString, `Stability test failed at iteration ${i + 1}`);

            // Every encryption of the same input should be identical (deterministic)
            const encrypted2 = cipher.encrypt(testString);
            assert.strictEqual(encrypted, encrypted2, `Determinism test failed at iteration ${i + 1}`);
        }

        console.log(`     ✅ ${iterations} iterations successful`);
    }

    console.log('✅ Performance and stability tests passed');
}

/**
 * Main integration test runner
 */
function runIntegrationTests() {
    console.log('🚀 Starting FF3 JavaScript Integration Tests');
    console.log('===========================================');

    try {
        testPublicApiIntegration();
        testCustomAlphabetIntegration();
        testNistVectorsIntegration();
        testCrossAlphabetConsistency();
        testPerformanceStability();

        console.log('\n🎉 All integration tests passed! ✅');
        console.log('\n💡 FF3 JavaScript implementation is ready for use!');
        console.log('⚠️  Remember: For educational and research purposes only.');

    } catch (error) {
        console.error('❌ Integration test failed:', error.message);
        console.error(error.stack);
        process.exit(1);
    }
}

// Run tests if this file is executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
    runIntegrationTests();
}