/// FF3 Alphabets - Canonical alphabets for cross-language compatibility.

// Canonical alphabets used by test vectors (order is part of the spec!)
pub const ALPHA_DIGITS: &str = "0123456789";                                                 // radix 10
pub const ALPHA_HEX_LOWER: &str = "0123456789abcdef";                                          // radix 16 lowercase
pub const ALPHA_HEX_UPPER: &str = "0123456789ABCDEF";                                          // radix 16 uppercase
pub const ALPHA_BASE36_LOW: &str = "0123456789abcdefghijklmnopqrstuvwxyz";                      // radix 36 lowercase
pub const ALPHA_BASE36_UP: &str = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";                      // radix 36 uppercase
pub const ALPHA_BASE62: &str = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"; // radix 62

/// Alphabet specification - NO NORMALIZATION, pure format preservation
#[derive(Clone, Debug)]
pub struct AlphabetSpec {
    pub charset: String,
}

impl AlphabetSpec {
    pub fn new(charset: &str) -> Self {
        Self {
            charset: charset.to_string(),
        }
    }
}

// Built-in specs - alphabet defines what's valid, preserve exactly what user inputs
pub const SPEC_DIGITS: AlphabetSpec = AlphabetSpec {
    charset: String::new(), // Will be set properly in lazy_static or constructor
};

// For now, let's use functions to create specs to avoid const String issues
pub fn spec_digits() -> AlphabetSpec {
    AlphabetSpec::new(ALPHA_DIGITS)
}

pub fn spec_hex_lower() -> AlphabetSpec {
    AlphabetSpec::new(ALPHA_HEX_LOWER)
}

pub fn spec_hex_upper() -> AlphabetSpec {
    AlphabetSpec::new(ALPHA_HEX_UPPER)
}

pub fn spec_base36_low() -> AlphabetSpec {
    AlphabetSpec::new(ALPHA_BASE36_LOW)
}

pub fn spec_base36_up() -> AlphabetSpec {
    AlphabetSpec::new(ALPHA_BASE36_UP)
}

pub fn spec_base62() -> AlphabetSpec {
    AlphabetSpec::new(ALPHA_BASE62)
}