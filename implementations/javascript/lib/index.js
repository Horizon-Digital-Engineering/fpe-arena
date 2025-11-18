/**
 * FF3 Format Preserving Encryption - Modern JavaScript Implementation
 * 
 * Experimental implementation for research and educational purposes.
 * 
 * ⚠️ FF3 was withdrawn by NIST due to security vulnerabilities.
 * This implementation is for educational and research purposes only.
 * 
 * Features:
 * - Modern ES6+ JavaScript
 * - Works in Node.js and browsers
 * - Zero dependencies (uses built-in crypto)
 * - TypeScript-compatible JSDoc types
 * - Universal module (ESM + CJS ready)
 */

// Re-export all public APIs
export {
    // Core classes
    FF3,
    // Constructor functions
    fromSpec,
    digits,
    hex,
    hexLower,
    hexUpper,
    base36,
    base36Lower,
    base36Upper,
    base62,
    radix26,
    // Utility functions
    hexToBytes,
    bytesToHex
} from './api.js';

export {
    // Alphabet classes and constants
    AlphabetSpec,
    ALPHA_DIGITS,
    ALPHA_HEX_LOWER,
    ALPHA_HEX_UPPER,
    ALPHA_BASE36_LOW,
    ALPHA_BASE36_UP,
    ALPHA_BASE62,
    // Alphabet spec factories
    createSpecDigits,
    createSpecHexLower,
    createSpecHexUpper,
    createSpecBase36Low,
    createSpecBase36Up,
    createSpecBase62,
    createSpecRadix26,
    // Pre-built specs (lazy-loaded)
    SPEC_DIGITS,
    SPEC_HEX_LOWER,
    SPEC_HEX_UPPER,
    SPEC_BASE36_LOW,
    SPEC_BASE36_UP,
    SPEC_BASE62,
    SPEC_RADIX26
} from './alphabets.js';

export {
    // Core cipher (advanced usage)
    FF3Cipher
} from './core.js';

/**
 * Library version
 */
export const VERSION = '0.1.0';

/**
 * Quick start example for copy-paste usage
 * @example
 * ```javascript
 * import { digits, hexToBytes } from './ff3.js';
 *
 * const key = hexToBytes('EF4359D8D580AA4F7F036D6F04FC6A94');
 * const tweak = hexToBytes('D8E7920AFA330A73');
 * const cipher = digits(key, tweak);
 * 
 * const encrypted = cipher.encrypt('1234567890');
 * const decrypted = cipher.decrypt(encrypted);
 * console.log({ encrypted, decrypted });
 * ```
 */

/**
 * Browser usage example  
 * @example
 * ```html
 * <script type="module">
 *   import * as FF3 from './ff3.js';
 *   
 *   const key = FF3.hexToBytes('EF4359D8D580AA4F7F036D6F04FC6A94');
 *   const tweak = FF3.hexToBytes('D8E7920AFA330A73');
 *   const cipher = FF3.digits(key, tweak);
 *   
 *   document.getElementById('result').textContent = 
 *     cipher.encrypt('1234567890');
 * </script>
 * ```
 */

// Note: Security warnings are displayed by CLI tools, not the library

// Import everything for default export
import {
    FF3,
    fromSpec,
    digits,
    hex,
    hexLower,
    hexUpper,
    base36,
    base36Lower,
    base36Upper,
    base62,
    radix26,
    hexToBytes,
    bytesToHex
} from './api.js';

import { AlphabetSpec } from './alphabets.js';
import { FF3Cipher } from './core.js';

// Default export for CommonJS compatibility
export default {
    FF3,
    fromSpec,
    digits,
    hex,
    hexLower,
    hexUpper,
    base36,
    base36Lower,
    base36Upper,
    base62,
    radix26,
    hexToBytes,
    bytesToHex,
    AlphabetSpec,
    FF3Cipher,
    VERSION
};