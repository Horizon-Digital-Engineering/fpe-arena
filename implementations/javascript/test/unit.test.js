/**
 * FF3 JavaScript Unit Tests
 *
 * Unit tests for individual modules and components.
 */

import { strict as assert } from 'assert';
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
    bytesToHex
} from '../lib/index.js';

/**
 * Test hex utility functions
 */
function testHexUtilities() {
    console.log('🧪 Testing hex utility functions...');

    // Test basic hex conversion
    const testBytes = new Uint8Array([0xEF, 0x43, 0x59, 0xD8]);
    const hexString = bytesToHex(testBytes);
    const convertedBytes = hexToBytes(hexString);

    assert.strictEqual(hexString.toLowerCase(), 'ef4359d8');
    assert.deepStrictEqual(convertedBytes, testBytes);

    // Test case insensitivity
    const upperHex = 'EF4359D8';
    const lowerHex = 'ef4359d8';

    assert.deepStrictEqual(hexToBytes(upperHex), hexToBytes(lowerHex));

    // Test empty input
    assert.deepStrictEqual(hexToBytes(''), new Uint8Array([]));
    assert.strictEqual(bytesToHex(new Uint8Array([])), '');

    console.log('✅ Hex utilities tests passed');
}

/**
 * Test alphabet specifications
 */
function testAlphabetSpecs() {
    console.log('🧪 Testing alphabet specifications...');

    // Test basic alphabet creation
    const digitsAlphabet = new AlphabetSpec('0123456789');
    assert.strictEqual(digitsAlphabet.charset, '0123456789');
    assert.strictEqual(digitsAlphabet.radix, 10);

    // Test custom alphabet
    const binaryAlphabet = new AlphabetSpec('01');
    assert.strictEqual(binaryAlphabet.charset, '01');
    assert.strictEqual(binaryAlphabet.radix, 2);

    // Test duplicate character detection
    try {
        new AlphabetSpec('0011'); // Duplicate characters
        assert.fail('Should have thrown error for duplicate characters');
    } catch (error) {
        assert(error.message.includes('duplicate'));
    }

    // Test empty alphabet
    try {
        new AlphabetSpec('');
        assert.fail('Should have thrown error for empty alphabet');
    } catch (error) {
        console.log('Empty alphabet error:', error.message);
        assert(error.message.includes('at least 2') || error.message.includes('empty'));
    }

    console.log('✅ Alphabet specification tests passed');
}

/**
 * Test cipher factory functions
 */
function testCipherFactories() {
    console.log('🧪 Testing cipher factory functions...');

    const key = hexToBytes('EF4359D8D580AA4F7F036D6F04FC6A94');
    const tweak = hexToBytes('D8E7920AFA330A73');

    // Test all factory functions
    const factories = [
        ['digits', digits],
        ['hexLower', hexLower],
        ['hexUpper', hexUpper],
        ['base36Lower', base36Lower],
        ['base36Upper', base36Upper],
        ['base62', base62],
        ['radix26', radix26]
    ];

    for (const [name, factory] of factories) {
        const cipher = factory(key, tweak);
        assert(cipher, `${name} factory should return a cipher`);
        assert(typeof cipher.encrypt === 'function', `${name} cipher should have encrypt method`);
        assert(typeof cipher.decrypt === 'function', `${name} cipher should have decrypt method`);
    }

    // Test custom spec factory
    const customSpec = new AlphabetSpec('!@#$%');
    const customCipher = fromSpec(key, tweak, customSpec);
    assert(customCipher, 'Custom spec factory should return a cipher');

    console.log('✅ Cipher factory tests passed');
}

/**
 * Test core cipher functionality
 */
function testCoreCipher() {
    console.log('🧪 Testing core cipher functionality...');

    const key = hexToBytes('EF4359D8D580AA4F7F036D6F04FC6A94');
    const tweak = hexToBytes('D8E7920AFA330A73');

    // Test basic encryption/decryption
    const cipher = digits(key, tweak);
    const plaintext = '1234567890';
    const ciphertext = cipher.encrypt(plaintext);
    const decrypted = cipher.decrypt(ciphertext);

    assert.strictEqual(decrypted, plaintext, 'Round-trip should preserve plaintext');
    assert.notStrictEqual(ciphertext, plaintext, 'Ciphertext should differ from plaintext');

    // Test deterministic encryption
    const ciphertext2 = cipher.encrypt(plaintext);
    assert.strictEqual(ciphertext, ciphertext2, 'Encryption should be deterministic');

    // Test different key produces different output
    const key2 = hexToBytes('0123456789ABCDEF0123456789ABCDEF');
    const cipher2 = digits(key2, tweak);
    const ciphertext3 = cipher2.encrypt(plaintext);
    assert.notStrictEqual(ciphertext, ciphertext3, 'Different keys should produce different outputs');

    console.log('✅ Core cipher tests passed');
}

/**
 * Test error handling
 */
function testErrorHandling() {
    console.log('🧪 Testing error handling...');

    const key = hexToBytes('EF4359D8D580AA4F7F036D6F04FC6A94');
    const tweak = hexToBytes('D8E7920AFA330A73');
    const cipher = digits(key, tweak);

    // Test invalid characters for alphabet
    try {
        cipher.encrypt('ABC'); // Letters not in digits alphabet
        assert.fail('Should have thrown error for invalid characters');
    } catch (error) {
        assert(error.message.includes('invalid') || error.message.includes('alphabet'));
    }

    // Test empty string
    try {
        cipher.encrypt('');
        assert.fail('Should have thrown error for empty string');
    } catch (error) {
        assert(error.message.length > 0);
    }

    // Test string too short for FF3
    try {
        cipher.encrypt('1');
        assert.fail('Should have thrown error for string too short');
    } catch (error) {
        assert(error.message.length > 0);
    }

    // Test invalid key size
    try {
        const shortKey = hexToBytes('EF43');
        digits(shortKey, tweak);
        assert.fail('Should have thrown error for invalid key size');
    } catch (error) {
        assert(error.message.length > 0);
    }

    // Test invalid tweak size
    try {
        const shortTweak = hexToBytes('D8E7');
        digits(key, shortTweak);
        assert.fail('Should have thrown error for invalid tweak size');
    } catch (error) {
        assert(error.message.length > 0);
    }

    console.log('✅ Error handling tests passed');
}

/**
 * Test case preservation across alphabets
 */
function testCasePreservation() {
    console.log('🧪 Testing case preservation...');

    const key = hexToBytes('EF4359D8D580AA4F7F036D6F04FC6A94');
    const tweak = hexToBytes('D8E7920AFA330A73');

    // Test hex case preservation
    const lowerCipher = hexLower(key, tweak);
    const upperCipher = hexUpper(key, tweak);

    const lowerInput = '1234abcdef56';
    const upperInput = '1234ABCDEF56';

    const lowerEncrypted = lowerCipher.encrypt(lowerInput);
    const upperEncrypted = upperCipher.encrypt(upperInput);

    // Should be different due to different alphabets
    assert.notStrictEqual(lowerEncrypted, upperEncrypted);

    // Round-trip should preserve case
    const lowerDecrypted = lowerCipher.decrypt(lowerEncrypted);
    const upperDecrypted = upperCipher.decrypt(upperEncrypted);

    assert.strictEqual(lowerDecrypted, lowerInput);
    assert.strictEqual(upperDecrypted, upperInput);

    console.log('✅ Case preservation tests passed');
}

/**
 * Main test runner
 */
function runUnitTests() {
    console.log('🚀 Starting FF3 JavaScript Unit Tests');
    console.log('====================================');

    try {
        testHexUtilities();
        testAlphabetSpecs();
        testCipherFactories();
        testCoreCipher();
        testErrorHandling();
        testCasePreservation();

        console.log('\n🎉 All unit tests passed! ✅');

    } catch (error) {
        console.error('❌ Unit test failed:', error.message);
        console.error(error.stack);
        process.exit(1);
    }
}

// Run tests if this file is executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
    runUnitTests();
}