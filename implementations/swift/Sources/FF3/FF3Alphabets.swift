/**
 * FF3 Alphabets - Canonical alphabets for cross-language compatibility.
 * Order is part of the spec - these must match other language implementations exactly.
 */

import Foundation

// Canonical alphabets used by test vectors (order is part of the spec!)
public let ALPHA_DIGITS = "0123456789"                                                 // radix 10
public let ALPHA_HEX_LOWER = "0123456789abcdef"                                        // radix 16 lowercase
public let ALPHA_HEX_UPPER = "0123456789ABCDEF"                                        // radix 16 uppercase
public let ALPHA_BASE36_LOW = "0123456789abcdefghijklmnopqrstuvwxyz"                    // radix 36 lowercase
public let ALPHA_BASE36_UP = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"                    // radix 36 uppercase
public let ALPHA_BASE62 = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" // radix 62

/**
 * Alphabet specification for use with FF3 ciphers.
 * Stores the character set exactly as provided with no normalization or validation.
 * The charset defines the valid characters and their ordering for digit conversion.
 * Order matters: position in charset determines the numeric value for each character.
 */
public struct AlphabetSpec {
    public let charset: String

    public init(charset: String) {
        self.charset = charset
    }
}

// Built-in specs - alphabet defines what's valid, preserve exactly what user inputs
public let SPEC_DIGITS = AlphabetSpec(charset: ALPHA_DIGITS)
public let SPEC_HEX_LOWER = AlphabetSpec(charset: ALPHA_HEX_LOWER)
public let SPEC_HEX_UPPER = AlphabetSpec(charset: ALPHA_HEX_UPPER)
public let SPEC_BASE36_LOW = AlphabetSpec(charset: ALPHA_BASE36_LOW)
public let SPEC_BASE36_UP = AlphabetSpec(charset: ALPHA_BASE36_UP)
public let SPEC_BASE62 = AlphabetSpec(charset: ALPHA_BASE62)