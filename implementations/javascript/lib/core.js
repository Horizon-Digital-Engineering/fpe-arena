/**
 * FF3 Core - Pure cryptographic implementation in modern JavaScript
 * 
 * Modern JavaScript features used:
 * - ES6 modules and classes
 * - Private fields and methods
 * - BigInt for arbitrary precision arithmetic
 * - Typed arrays (Uint8Array)
 * - Async/await ready
 * - Arrow functions and destructuring
 * - Template literals
 */

import { createHash, createCipheriv } from 'crypto';

/**
 * Core FF3 cipher implementation using modern JavaScript
 * 
 * Features:
 * - Private fields for encapsulation
 * - BigInt for large number operations
 * - Proper error handling
 * - Clean separation of concerns
 */
export class FF3Cipher {
    #radix;
    #aes;
    #tweak;
    #minLen;
    #maxLen;

    /**
     * Create a new FF3 cipher
     * @param {number} radix - Radix for the character set (2-62)
     * @param {Uint8Array} key - AES key (16, 24, or 32 bytes)
     * @param {Uint8Array} tweak - FF3 tweak (exactly 8 bytes)
     */
    constructor(radix, key, tweak) {
        // Validate inputs
        if (!Number.isInteger(radix) || radix < 2 || radix > 62) {
            throw new Error(`Radix must be between 2 and 62, got ${radix}`);
        }
        
        if (!(key instanceof Uint8Array) || ![16, 24, 32].includes(key.length)) {
            throw new Error(`Key must be 16, 24, or 32 bytes, got ${key?.length || 'invalid'}`);
        }
        
        if (!(tweak instanceof Uint8Array) || tweak.length !== 8) {
            throw new Error(`Tweak must be exactly 8 bytes, got ${tweak?.length || 'invalid'}`);
        }

        this.#radix = radix;
        this.#tweak = new Uint8Array(tweak); // Copy to prevent external modification
        this.#aes = this.#createAESCipher(key);
        
        // Calculate length constraints
        this.#minLen = 2;
        this.#maxLen = radix >= 2 && radix <= 36 ? 32 : 56;
    }

    // Getters for accessing private fields
    get radix() { return this.#radix; }
    get minLen() { return this.#minLen; }
    get maxLen() { return this.#maxLen; }

    /**
     * Create AES cipher with FF3 byte-reversal convention
     * @param {Uint8Array} key - AES key
     * @returns {object} AES cipher object
     * @private
     */
    #createAESCipher(key) {
        // FF3 specification requires byte reversal of the key
        const reversedKey = new Uint8Array(key).reverse();
        
        // Return cipher factory based on key length
        const keyLength = key.length * 8; // Convert to bits
        return {
            keyLength,
            key: reversedKey,
            encrypt: (block) => this.#aesEncrypt(reversedKey, block)
        };
    }

    /**
     * AES-ECB encryption using Node.js crypto
     * @param {Uint8Array} key - AES key
     * @param {Uint8Array} block - 16-byte block to encrypt
     * @returns {Uint8Array} Encrypted block
     * @private
     */
    #aesEncrypt(key, block) {
        // Use Node.js crypto for AES-ECB
        const cipher = createCipheriv(`aes-${key.length * 8}-ecb`, key, null);
        cipher.setAutoPadding(false);
        
        const encrypted = cipher.update(block);
        cipher.final();
        
        return new Uint8Array(encrypted);
    }

    /**
     * Encrypt digit array using FF3 algorithm
     * @param {number[]} plaintext - Array of integers in radix
     * @param {Uint8Array} [additionalTweak] - Optional additional tweak
     * @returns {number[]} Encrypted digit array
     */
    encryptDigits(plaintext, additionalTweak = null) {
        this.#validateLength(plaintext.length);
        const effectiveTweak = this.#combineTweaks(additionalTweak);
        return this.#ff3Encrypt(plaintext, effectiveTweak);
    }

    /**
     * Decrypt digit array using FF3 algorithm
     * @param {number[]} ciphertext - Array of integers in radix
     * @param {Uint8Array} [additionalTweak] - Optional additional tweak
     * @returns {number[]} Decrypted digit array
     */
    decryptDigits(ciphertext, additionalTweak = null) {
        this.#validateLength(ciphertext.length);
        const effectiveTweak = this.#combineTweaks(additionalTweak);
        return this.#ff3Decrypt(ciphertext, effectiveTweak);
    }

    /**
     * Core FF3 encryption algorithm using modern JavaScript
     * @param {number[]} plaintext - Digit array to encrypt
     * @param {Uint8Array} tweak - Effective tweak
     * @returns {number[]} Encrypted digit array
     * @private
     */
    #ff3Encrypt(plaintext, tweak) {
        const n = plaintext.length;
        const u = Math.ceil(n / 2);  // Using Math.ceil for clarity
        const v = n - u;

        // Split using array destructuring and slice
        let [a, b] = [plaintext.slice(0, u), plaintext.slice(u)];

        // Perform 8 Feistel rounds using modern for-of loop
        for (const i of Array(8).keys()) {
            if (i % 2 === 0) {
                // Even round: use B to update A
                const w = this.#calculateW(tweak, i);
                const p = this.#calculateP(i, w, b);
                const m = BigInt(this.#radix) ** BigInt(u);

                // FF3 digit reversal using spread operator
                const aReversed = [...a].reverse();
                const aNum = this.#digitsToMegaInt(aReversed);

                // Modular arithmetic with BigInt
                const y = (aNum + p) % m;
                const newDigits = this.#bigIntToDigits(y, u);
                a = [...newDigits].reverse();
            } else {
                // Odd round: use A to update B  
                const w = this.#calculateW(tweak, i);
                const p = this.#calculateP(i, w, a);
                const m = BigInt(this.#radix) ** BigInt(v);

                const bReversed = [...b].reverse();
                const bNum = this.#digitsToMegaInt(bReversed);

                const y = (bNum + p) % m;
                const newDigits = this.#bigIntToDigits(y, v);
                b = [...newDigits].reverse();
            }
        }

        // Combine results using spread operator
        return [...a, ...b];
    }

    /**
     * Core FF3 decryption algorithm
     * @param {number[]} ciphertext - Digit array to decrypt
     * @param {Uint8Array} tweak - Effective tweak
     * @returns {number[]} Decrypted digit array
     * @private
     */
    #ff3Decrypt(ciphertext, tweak) {
        const n = ciphertext.length;
        const u = Math.ceil(n / 2);
        const v = n - u;

        let [a, b] = [ciphertext.slice(0, u), ciphertext.slice(u)];

        // Perform 8 Feistel rounds in reverse
        for (const i of [7, 6, 5, 4, 3, 2, 1, 0]) {
            if (i % 2 === 0) {
                // Even round reverse
                const w = this.#calculateW(tweak, i);
                const p = this.#calculateP(i, w, b);
                const m = BigInt(this.#radix) ** BigInt(u);

                const aReversed = [...a].reverse();
                const aNum = this.#digitsToMegaInt(aReversed);

                // Modular subtraction using BigInt
                const pMod = p % m;
                const y = (aNum + m - pMod) % m;
                const newDigits = this.#bigIntToDigits(y, u);
                a = [...newDigits].reverse();
            } else {
                // Odd round reverse
                const w = this.#calculateW(tweak, i);
                const p = this.#calculateP(i, w, a);
                const m = BigInt(this.#radix) ** BigInt(v);

                const bReversed = [...b].reverse();
                const bNum = this.#digitsToMegaInt(bReversed);

                const pMod = p % m;
                const y = (bNum + m - pMod) % m;
                const newDigits = this.#bigIntToDigits(y, v);
                b = [...newDigits].reverse();
            }
        }

        return [...a, ...b];
    }

    /**
     * Calculate W parameter for FF3 round function
     * @param {Uint8Array} tweak - Tweak bytes
     * @param {number} roundNum - Round number
     * @returns {Uint8Array} W parameter (4 bytes)
     * @private
     */
    #calculateW(tweak, roundNum) {
        // NIST FF3 W calculation: split 8-byte tweak into Tl and Tr
        if (roundNum % 2 === 0) {
            // Even rounds: W = Tr (rightmost 4 bytes)
            return tweak.slice(4, 8);
        } else {
            // Odd rounds: W = Tl (leftmost 4 bytes)
            return tweak.slice(0, 4);
        }
    }

    /**
     * Calculate P parameter using AES encryption
     * @param {number} roundNum - Round number
     * @param {Uint8Array} w - W parameter
     * @param {number[]} block - Digit block
     * @returns {bigint} P value
     * @private
     */
    #calculateP(roundNum, w, block) {
        // Create 16-byte input for AES
        const input = new Uint8Array(16);

        // First 4 bytes: W XOR round number in last byte
        input.set(w, 0);
        input[3] ^= roundNum;

        // Last 12 bytes: NUM_radix(REV(B))
        const reversedBlock = [...block].reverse();
        const blockNum = this.#digitsToMegaInt(reversedBlock);
        const blockBytes = this.#bigIntToBytes(blockNum, 12);
        input.set(blockBytes, 4);

        // Apply FF3 byte reversal convention
        const reversedInput = new Uint8Array([...input].reverse());

        // Encrypt with AES
        const aesOutput = this.#aes.encrypt(reversedInput);

        // Apply FF3 byte reversal convention again
        const output = new Uint8Array([...aesOutput].reverse());

        // Convert to BigInt
        return this.#bytesToBigInt(output);
    }

    /**
     * Convert digit array to BigInt using radix
     * @param {number[]} digits - Digit array
     * @returns {bigint} BigInt representation
     * @private
     */
    #digitsToMegaInt(digits) {
        return digits.reduce((acc, digit) => acc * BigInt(this.#radix) + BigInt(digit), 0n);
    }

    /**
     * Convert BigInt to digit array with specified length
     * @param {bigint} num - BigInt number
     * @param {number} length - Required length
     * @returns {number[]} Digit array
     * @private
     */
    #bigIntToDigits(num, length) {
        if (num === 0n) return new Array(length).fill(0);

        const digits = [];
        const radixBig = BigInt(this.#radix);
        let temp = num;

        while (temp > 0n) {
            digits.push(Number(temp % radixBig));
            temp = temp / radixBig;
        }

        // Pad with zeros and reverse
        while (digits.length < length) digits.push(0);
        return digits.reverse();
    }

    /**
     * Convert BigInt to byte array with padding
     * @param {bigint} num - BigInt number
     * @param {number} length - Required byte length
     * @returns {Uint8Array} Byte array
     * @private
     */
    #bigIntToBytes(num, length) {
        if (num === 0n) return new Uint8Array(length);

        const bytes = [];
        let temp = num;

        while (temp > 0n) {
            bytes.push(Number(temp % 256n));
            temp = temp / 256n;
        }

        // Pad and reverse
        const result = new Uint8Array(length);
        const offset = Math.max(0, length - bytes.length);
        for (let i = 0; i < bytes.length && i < length; i++) {
            result[offset + i] = bytes[bytes.length - 1 - i];
        }

        return result;
    }

    /**
     * Convert byte array to BigInt
     * @param {Uint8Array} bytes - Byte array
     * @returns {bigint} BigInt representation
     * @private
     */
    #bytesToBigInt(bytes) {
        return bytes.reduce((acc, byte) => acc * 256n + BigInt(byte), 0n);
    }

    /**
     * Combine base tweak with additional tweak
     * @param {Uint8Array} [additionalTweak] - Optional additional tweak
     * @returns {Uint8Array} Combined tweak
     * @private
     */
    #combineTweaks(additionalTweak) {
        if (!additionalTweak) return this.#tweak;

        const combined = new Uint8Array(8);
        for (let i = 0; i < 8; i++) {
            combined[i] = this.#tweak[i] ^ (additionalTweak[i] || 0);
        }
        return combined;
    }

    /**
     * Validate plaintext/ciphertext length
     * @param {number} length - Length to validate
     * @private
     */
    #validateLength(length) {
        if (length < this.#minLen || length > this.#maxLen) {
            throw new Error(`Length ${length} out of bounds [${this.#minLen}, ${this.#maxLen}]`);
        }
    }
}