/**
 * FF3 API - Public interface with string ↔ digits conversion.
 * This file provides the user-facing API, equivalent to other language implementations.
 */

import Foundation

/**
 * FF3 API - User-facing interface with alphabet-specific string conversion.
 * Combines the core FF3 cryptographic engine with alphabet-specific character mapping.
 * Creates efficient lookup tables for fast character-to-index and index-to-character conversion.
 * Provides a clean API for encrypting/decrypting strings while preserving exact format.
 */
public class FF3 {
    private let core: FF3Cipher
    private let spec: AlphabetSpec
    private let alpha: [Character]
    private let index: [Character: Int]

    init(core: FF3Cipher, spec: AlphabetSpec) {
        self.core = core
        self.spec = spec
        self.alpha = Array(spec.charset)
        self.index = Dictionary(uniqueKeysWithValues: alpha.enumerated().map { ($1, $0) })
    }

    /**
     * Convert input string to array of integer digits using the cipher's alphabet.
     * Each character is mapped to its position in the alphabet (0-based indexing).
     * Validates that all characters exist in the defined alphabet.
     * Returns digit array suitable for FF3 core cryptographic operations.
     */
    private func toDigits(_ s: String) throws -> [Int] {
        var digits: [Int] = []
        for (i, char) in s.enumerated() {
            guard let digit = index[char] else {
                throw FF3Cipher.FF3Error.encryptionFailed("invalid char '\(char)' at pos \(i) for this alphabet")
            }
            digits.append(digit)
        }
        return digits
    }

    /**
     * Convert array of integer digits back to string using the cipher's alphabet.
     * Each digit index is mapped to its corresponding character in the alphabet.
     * Validates that all digit values are within the valid range for the alphabet.
     * Returns the reconstructed string preserving exact format and character set.
     */
    private func fromDigits(_ digits: [Int]) throws -> String {
        var chars: [Character] = []
        for digit in digits {
            guard digit >= 0 && digit < alpha.count else {
                throw FF3Cipher.FF3Error.encryptionFailed("digit \(digit) out of range for radix \(alpha.count)")
            }
            chars.append(alpha[digit])
        }
        return String(chars)
    }

    /**
     * Encrypt a plaintext string using FF3 while preserving exact format and character set.
     * Converts string to digits, applies FF3 encryption, then converts back to string.
     * Optional additionalTweak allows context-specific encryption for domain separation.
     * Output has same length and uses same alphabet as input (format-preserving property).
     */
    public func encrypt(_ plaintext: String, additionalTweak: Data? = nil) throws -> String {
        let digits = try toDigits(plaintext)
        let n = digits.count

        guard n >= core.getMinLength() && n <= core.getMaxLength() else {
            throw FF3Cipher.FF3Error.invalidInputLength("length \(n) out of bounds [\(core.getMinLength()),\(core.getMaxLength())]")
        }

        let encryptedDigits = try core.encryptDigits(digits, additionalTweak: additionalTweak)
        return try fromDigits(encryptedDigits)
    }

    /**
     * Decrypt a ciphertext string using FF3 to recover the original plaintext.
     * Converts string to digits, applies FF3 decryption, then converts back to string.
     * Must use the same additionalTweak (if any) that was used during encryption.
     * Returns original plaintext with exact same format and character set as before encryption.
     */
    public func decrypt(_ ciphertext: String, additionalTweak: Data? = nil) throws -> String {
        let digits = try toDigits(ciphertext)
        let n = digits.count

        guard n >= core.getMinLength() && n <= core.getMaxLength() else {
            throw FF3Cipher.FF3Error.invalidInputLength("length \(n) out of bounds [\(core.getMinLength()),\(core.getMaxLength())]")
        }

        let decryptedDigits = try core.decryptDigits(digits, additionalTweak: additionalTweak)
        return try fromDigits(decryptedDigits)
    }
}

// MARK: - Public Factory Functions

/**
 * Create a new FF3 cipher instance from a custom alphabet specification.
 * Validates the alphabet has sufficient characters (>=2) for meaningful encryption.
 * Constructs the core FF3 cipher with the alphabet's radix and wraps it in FF3.
 * This is the most flexible constructor allowing any custom character set.
 */
public func fromSpec(key: Data, tweak: Data, spec: AlphabetSpec) throws -> FF3 {
    guard spec.charset.count >= 2 else {
        throw FF3Cipher.FF3Error.encryptionFailed("alphabet must have >=2 chars")
    }

    let radix = spec.charset.count
    let core = try FF3Cipher(radix: radix, key: key, tweak: tweak)
    return FF3(core: core, spec: spec)
}

/**
 * Create a new FF3 cipher for decimal digits (0-9).
 * Most commonly used cipher for encrypting numeric data like SSNs, credit cards, etc.
 * Uses radix 10 and preserves exact numeric format of input.
 * Perfect for financial and governmental numeric identifiers.
 */
public func digits(key: Data, tweak: Data) throws -> FF3 {
    return try fromSpec(key: key, tweak: tweak, spec: SPEC_DIGITS)
}

/**
 * Create a new FF3 cipher for lowercase hex (0-9a-f).
 * Ideal for encrypting hexadecimal strings while preserving lowercase format.
 * Commonly used for hash values, UUIDs, and other hex-encoded data.
 * Maintains exact case and character distribution of input.
 */
public func hexLower(key: Data, tweak: Data) throws -> FF3 {
    return try fromSpec(key: key, tweak: tweak, spec: SPEC_HEX_LOWER)
}

/**
 * Create a new FF3 cipher for uppercase hex (0-9A-F).
 * Ideal for encrypting hexadecimal strings while preserving uppercase format.
 * Commonly used for hash values, UUIDs, and other hex-encoded data.
 * Maintains exact case and character distribution of input.
 */
public func hexUpper(key: Data, tweak: Data) throws -> FF3 {
    return try fromSpec(key: key, tweak: tweak, spec: SPEC_HEX_UPPER)
}

/**
 * Create a new FF3 cipher for lowercase base36 (0-9a-z).
 * Supports digits and lowercase letters for alphanumeric encryption.
 * Perfect for usernames, identifiers, and other lowercase alphanumeric data.
 * Preserves exact case and character distribution of the input data.
 */
public func base36Lower(key: Data, tweak: Data) throws -> FF3 {
    return try fromSpec(key: key, tweak: tweak, spec: SPEC_BASE36_LOW)
}

/**
 * Create a new FF3 cipher for uppercase base36 (0-9A-Z).
 * Supports digits and uppercase letters for alphanumeric encryption.
 * Perfect for usernames, identifiers, and other uppercase alphanumeric data.
 * Preserves exact case and character distribution of the input data.
 */
public func base36Upper(key: Data, tweak: Data) throws -> FF3 {
    return try fromSpec(key: key, tweak: tweak, spec: SPEC_BASE36_UP)
}

/**
 * Create a new FF3 cipher for base62 encoding (0-9A-Za-z).
 * Supports the full range of alphanumeric characters with case sensitivity.
 * Ideal for encrypting tokens, usernames, or mixed alphanumeric identifiers.
 * Preserves exact case and character distribution of the input data.
 */
public func base62(key: Data, tweak: Data) throws -> FF3 {
    return try fromSpec(key: key, tweak: tweak, spec: SPEC_BASE62)
}

// MARK: - Backward Compatibility Aliases

/**
 * Create a new FF3 cipher for hex (lowercase, for compatibility).
 * Alias for newHexLowerCipher for backward compatibility.
 */
public func hex(key: Data, tweak: Data) throws -> FF3 {
    return try hexLower(key: key, tweak: tweak)
}

/**
 * Create a new FF3 cipher for base36 (lowercase, for compatibility).
 * Alias for newBase36LowerCipher for backward compatibility.
 */
public func base36(key: Data, tweak: Data) throws -> FF3 {
    return try base36Lower(key: key, tweak: tweak)
}