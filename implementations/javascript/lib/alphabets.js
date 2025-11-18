/**
 * FF3 Alphabets - Canonical alphabets for cross-language compatibility
 * 
 * Modern JavaScript implementation using ES6+ features:
 * - ES6 modules (import/export)
 * - Classes with private fields
 * - Map objects for fast lookups
 * - Template literals
 * - Destructuring
 */

// Canonical alphabets used by test vectors (order is part of the spec!)
export const ALPHA_DIGITS = "0123456789";                                                 // radix 10
export const ALPHA_HEX_LOWER = "0123456789abcdef";                                          // radix 16 lowercase
export const ALPHA_HEX_UPPER = "0123456789ABCDEF";                                          // radix 16 uppercase
export const ALPHA_BASE36_LOW = "0123456789abcdefghijklmnopqrstuvwxyz";                      // radix 36 lowercase
export const ALPHA_BASE36_UP = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";                      // radix 36 uppercase
export const ALPHA_BASE62 = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"; // radix 62

/**
 * Alphabet specification for format-preserving encryption
 * NO NORMALIZATION - pure format preservation
 * 
 * Uses modern JavaScript features:
 * - Private fields (#charset, #charToInt, etc.)
 * - Getter methods
 * - Map for O(1) lookups
 */
export class AlphabetSpec {
    #charset;
    #charToInt;
    #intToChar;
    #radix;

    /**
     * Create an alphabet specification
     * @param {string} charset - The character set defining valid symbols
     * @throws {Error} If charset is invalid
     */
    constructor(charset) {
        if (typeof charset !== 'string' || charset.length < 2) {
            throw new Error('Alphabet must have at least 2 characters');
        }
        
        // Check for duplicate characters
        const uniqueChars = new Set(charset);
        if (uniqueChars.size !== charset.length) {
            throw new Error('Alphabet cannot contain duplicate characters');
        }

        this.#charset = charset;
        this.#radix = charset.length;

        // Create bidirectional mappings using Maps (faster than object lookup)
        this.#charToInt = new Map();
        this.#intToChar = new Map();
        
        for (let i = 0; i < charset.length; i++) {
            const char = charset[i];
            this.#charToInt.set(char, i);
            this.#intToChar.set(i, char);
        }
    }

    // Getters for read-only access to private fields
    get charset() { return this.#charset; }
    get radix() { return this.#radix; }
    
    /**
     * Convert character to integer
     * @param {string} char - Character to convert
     * @returns {number} Integer value or undefined if invalid
     */
    charToInt(char) {
        return this.#charToInt.get(char);
    }

    /**
     * Convert integer to character  
     * @param {number} int - Integer to convert
     * @returns {string} Character or undefined if invalid
     */
    intToChar(int) {
        return this.#intToChar.get(int);
    }

    /**
     * Check if character is valid for this alphabet
     * @param {string} char - Character to check
     * @returns {boolean} True if valid
     */
    hasChar(char) {
        return this.#charToInt.has(char);
    }

    /**
     * Check if integer is valid for this alphabet
     * @param {number} int - Integer to check  
     * @returns {boolean} True if valid
     */
    hasInt(int) {
        return this.#intToChar.has(int);
    }
}

// Built-in alphabet specifications using arrow functions and modern syntax
export const createSpecDigits = () => new AlphabetSpec(ALPHA_DIGITS);
export const createSpecHexLower = () => new AlphabetSpec(ALPHA_HEX_LOWER);
export const createSpecHexUpper = () => new AlphabetSpec(ALPHA_HEX_UPPER);
export const createSpecBase36Low = () => new AlphabetSpec(ALPHA_BASE36_LOW);
export const createSpecBase36Up = () => new AlphabetSpec(ALPHA_BASE36_UP);
export const createSpecBase62 = () => new AlphabetSpec(ALPHA_BASE62);

// Special case: radix 26 using first 26 characters of base36 (0-9a-p)
export const createSpecRadix26 = () => new AlphabetSpec(ALPHA_BASE36_LOW.slice(0, 26));

// Convenience pre-built specs (lazy-loaded for performance)
let _specDigits, _specHexLower, _specHexUpper, _specBase36Low, _specBase36Up, _specBase62, _specRadix26;

export const SPEC_DIGITS = () => _specDigits ??= createSpecDigits();
export const SPEC_HEX_LOWER = () => _specHexLower ??= createSpecHexLower();
export const SPEC_HEX_UPPER = () => _specHexUpper ??= createSpecHexUpper();
export const SPEC_BASE36_LOW = () => _specBase36Low ??= createSpecBase36Low();
export const SPEC_BASE36_UP = () => _specBase36Up ??= createSpecBase36Up();
export const SPEC_BASE62 = () => _specBase62 ??= createSpecBase62();
export const SPEC_RADIX26 = () => _specRadix26 ??= createSpecRadix26();