/**
 * FF3 API - Public interface with string ↔ digits conversion
 * 
 * Modern JavaScript API featuring:
 * - Clean class-based design
 * - Private fields for encapsulation  
 * - Comprehensive error handling
 * - JSDoc types for IDE support
 * - Promise-ready architecture
 */

import { FF3Cipher } from './core.js';
import { 
    AlphabetSpec,
    SPEC_DIGITS, SPEC_HEX_LOWER, SPEC_HEX_UPPER,
    SPEC_BASE36_LOW, SPEC_BASE36_UP, SPEC_BASE62, SPEC_RADIX26
} from './alphabets.js';

/**
 * Lightweight cipher with alphabet-specific string conversion
 * 
 * Modern features:
 * - Private fields for data encapsulation
 * - Method chaining support
 * - Async-ready design
 * - Built-in validation
 */
export class FF3 {
    #core;
    #spec;

    /**
     * Create a new FF3 instance
     * @param {FF3Cipher} core - Core FF3 cipher
     * @param {AlphabetSpec} spec - Alphabet specification
     * @private
     */
    constructor(core, spec) {
        this.#core = core;
        this.#spec = spec;
    }

    /**
     * Get the alphabet specification
     * @returns {AlphabetSpec} Alphabet specification
     */
    get alphabetSpec() {
        return this.#spec;
    }

    /**
     * Get the radix (alphabet size)
     * @returns {number} Radix value
     */
    get radix() {
        return this.#spec.radix;
    }

    /**
     * Encrypt string preserving exact format
     * @param {string} plaintext - Text to encrypt
     * @param {Uint8Array} [additionalTweak] - Optional additional tweak
     * @returns {string} Encrypted text
     * @throws {Error} If input is invalid
     */
    encrypt(plaintext, additionalTweak = null) {
        if (typeof plaintext !== 'string') {
            throw new Error('Plaintext must be a string');
        }

        const digits = this.#stringToDigits(plaintext);
        const encryptedDigits = this.#core.encryptDigits(digits, additionalTweak);
        return this.#digitsToString(encryptedDigits);
    }

    /**
     * Decrypt string preserving exact format
     * @param {string} ciphertext - Text to decrypt
     * @param {Uint8Array} [additionalTweak] - Optional additional tweak
     * @returns {string} Decrypted text
     * @throws {Error} If input is invalid
     */
    decrypt(ciphertext, additionalTweak = null) {
        if (typeof ciphertext !== 'string') {
            throw new Error('Ciphertext must be a string');
        }

        const digits = this.#stringToDigits(ciphertext);
        const decryptedDigits = this.#core.decryptDigits(digits, additionalTweak);
        return this.#digitsToString(decryptedDigits);
    }

    /**
     * Convert string to digit array using alphabet
     * @param {string} str - String to convert
     * @returns {number[]} Digit array
     * @throws {Error} If string contains invalid characters
     * @private
     */
    #stringToDigits(str) {
        const digits = [];
        for (let i = 0; i < str.length; i++) {
            const char = str[i];
            const digit = this.#spec.charToInt(char);
            if (digit === undefined) {
                throw new Error(`Invalid character '${char}' at position ${i} for this alphabet`);
            }
            digits.push(digit);
        }
        return digits;
    }

    /**
     * Convert digit array to string using alphabet
     * @param {number[]} digits - Digit array to convert
     * @returns {string} Resulting string
     * @throws {Error} If digits are out of range
     * @private
     */
    #digitsToString(digits) {
        return digits.map((digit, i) => {
            const char = this.#spec.intToChar(digit);
            if (char === undefined) {
                throw new Error(`Digit ${digit} out of range for radix ${this.#spec.radix} at position ${i}`);
            }
            return char;
        }).join('');
    }
}

/**
 * Create cipher from alphabet specification
 * @param {Uint8Array|Buffer} key - AES key (16, 24, or 32 bytes)
 * @param {Uint8Array|Buffer} tweak - FF3 tweak (exactly 8 bytes)
 * @param {AlphabetSpec} spec - Alphabet specification
 * @returns {FF3} Configured cipher instance
 * @throws {Error} If parameters are invalid
 */
export function fromSpec(key, tweak, spec) {
    // Convert Buffer to Uint8Array if needed (Node.js compatibility)
    const keyArray = key instanceof Uint8Array ? key : new Uint8Array(key);
    const tweakArray = tweak instanceof Uint8Array ? tweak : new Uint8Array(tweak);

    if (!(spec instanceof AlphabetSpec)) {
        throw new Error('spec must be an AlphabetSpec instance');
    }

    if (spec.radix < 2) {
        throw new Error('Alphabet must have at least 2 characters');
    }

    const core = new FF3Cipher(spec.radix, keyArray, tweakArray);
    return new FF3(core, spec);
}

// Convenience constructors for standard alphabets using arrow functions

/**
 * Create cipher for digits (0-9)
 * @param {Uint8Array|Buffer} key - AES key
 * @param {Uint8Array|Buffer} tweak - FF3 tweak
 * @returns {FF3} Digits cipher
 */
export const digits = (key, tweak) =>
    fromSpec(key, tweak,SPEC_DIGITS());

/**
 * Create cipher for lowercase hex (0-9a-f)
 * @param {Uint8Array|Buffer} key - AES key
 * @param {Uint8Array|Buffer} tweak - FF3 tweak
 * @returns {FF3} Hex lowercase cipher
 */
export const hexLower = (key, tweak) =>
    fromSpec(key, tweak,SPEC_HEX_LOWER());

/**
 * Create cipher for uppercase hex (0-9A-F)
 * @param {Uint8Array|Buffer} key - AES key
 * @param {Uint8Array|Buffer} tweak - FF3 tweak
 * @returns {FF3} Hex uppercase cipher
 */
export const hexUpper = (key, tweak) =>
    fromSpec(key, tweak,SPEC_HEX_UPPER());

/**
 * Create cipher for lowercase base36 (0-9a-z)
 * @param {Uint8Array|Buffer} key - AES key
 * @param {Uint8Array|Buffer} tweak - FF3 tweak
 * @returns {FF3} Base36 lowercase cipher
 */
export const base36Lower = (key, tweak) =>
    fromSpec(key, tweak,SPEC_BASE36_LOW());

/**
 * Create cipher for uppercase base36 (0-9A-Z)
 * @param {Uint8Array|Buffer} key - AES key
 * @param {Uint8Array|Buffer} tweak - FF3 tweak
 * @returns {FF3} Base36 uppercase cipher
 */
export const base36Upper = (key, tweak) =>
    fromSpec(key, tweak,SPEC_BASE36_UP());

/**
 * Create cipher for base62 (0-9A-Za-z)
 * @param {Uint8Array|Buffer} key - AES key
 * @param {Uint8Array|Buffer} tweak - FF3 tweak
 * @returns {FF3} Base62 cipher
 */
export const base62 = (key, tweak) =>
    fromSpec(key, tweak,SPEC_BASE62());

/**
 * Create cipher for radix 26 (0-9a-p)
 * @param {Uint8Array|Buffer} key - AES key
 * @param {Uint8Array|Buffer} tweak - FF3 tweak
 * @returns {FF3} Radix 26 cipher
 */
export const radix26 = (key, tweak) =>
    fromSpec(key, tweak, SPEC_RADIX26());

// Compatibility aliases
export const hex = hexLower;
export const base36 = base36Lower;

/**
 * Utility function to convert hex string to Uint8Array
 * @param {string} hexString - Hex string (e.g., "EF4359D8...")
 * @returns {Uint8Array} Byte array
 * @throws {Error} If hex string is invalid
 */
export function hexToBytes(hexString) {
    if (typeof hexString !== 'string') {
        throw new Error('Hex string must be a string');
    }
    
    const cleanHex = hexString.replace(/\s+/g, '');
    if (cleanHex.length % 2 !== 0) {
        throw new Error('Hex string must have even length');
    }
    
    if (!/^[0-9a-fA-F]*$/.test(cleanHex)) {
        throw new Error('Hex string contains invalid characters');
    }

    const bytes = new Uint8Array(cleanHex.length / 2);
    for (let i = 0; i < cleanHex.length; i += 2) {
        bytes[i / 2] = parseInt(cleanHex.substr(i, 2), 16);
    }
    return bytes;
}

/**
 * Utility function to convert Uint8Array to hex string
 * @param {Uint8Array} bytes - Byte array
 * @returns {string} Hex string
 */
export function bytesToHex(bytes) {
    if (!(bytes instanceof Uint8Array)) {
        throw new Error('Input must be Uint8Array');
    }
    
    return Array.from(bytes)
        .map(byte => byte.toString(16).padStart(2, '0'))
        .join('');
}