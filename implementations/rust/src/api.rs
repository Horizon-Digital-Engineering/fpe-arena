/// FF3 API - Public interface with string ↔ digits conversion.
/// This file provides the user-facing API, equivalent to Go ff3_api.go and Python ff3_api.py.

use crate::core::FF3Cipher;
use crate::alphabets::{AlphabetSpec, spec_digits, spec_hex_lower, spec_hex_upper, 
                      spec_base36_low, spec_base36_up, spec_base62, ALPHA_BASE36_LOW};
use std::collections::HashMap;

/// FF3 API - User-facing interface with alphabet-specific string conversion
pub struct FF3 {
    core: FF3Cipher,
    alpha: Vec<char>,
    index: HashMap<char, usize>,
}

impl FF3 {
    /// Create new FF3 from core and spec
    fn new(core: FF3Cipher, spec: AlphabetSpec) -> Self {
        let alpha: Vec<char> = spec.charset.chars().collect();
        let index: HashMap<char, usize> = alpha.iter()
            .enumerate()
            .map(|(i, &c)| (c, i))
            .collect();

        Self {
            core,
            alpha,
            index,
        }
    }

    /// Convert string to digit array using alphabet
    fn to_digits(&self, s: &str) -> Result<Vec<usize>, String> {
        let mut digits = Vec::new();
        for (i, c) in s.chars().enumerate() {
            match self.index.get(&c) {
                Some(&digit) => digits.push(digit),
                None => return Err(format!("invalid char '{}' at pos {} for this alphabet", c, i)),
            }
        }
        Ok(digits)
    }

    /// Convert digit array to string using alphabet
    fn from_digits(&self, digits: &[usize]) -> Result<String, String> {
        let mut chars = Vec::new();
        for &digit in digits {
            if digit >= self.alpha.len() {
                return Err(format!("digit {} out of range for radix {}", digit, self.alpha.len()));
            }
            chars.push(self.alpha[digit]);
        }
        Ok(chars.into_iter().collect())
    }

    /// Encrypt string preserving exact format
    pub fn encrypt(&self, plaintext: &str, additional_tweak: Option<&[u8]>) -> Result<String, String> {
        let digits = self.to_digits(plaintext)?;
        let n = digits.len();

        if n < self.core.get_min_len() || n > self.core.get_max_len() {
            return Err(format!("length {} out of bounds [{},{}]", 
                              n, self.core.get_min_len(), self.core.get_max_len()));
        }

        let encrypted_digits = self.core.encrypt_digits(&digits, additional_tweak);
        self.from_digits(&encrypted_digits)
    }

    /// Decrypt string preserving exact format
    pub fn decrypt(&self, ciphertext: &str, additional_tweak: Option<&[u8]>) -> Result<String, String> {
        let digits = self.to_digits(ciphertext)?;
        let n = digits.len();

        if n < self.core.get_min_len() || n > self.core.get_max_len() {
            return Err(format!("length {} out of bounds [{},{}]", 
                              n, self.core.get_min_len(), self.core.get_max_len()));
        }

        let decrypted_digits = self.core.decrypt_digits(&digits, additional_tweak);
        self.from_digits(&decrypted_digits)
    }
}

/// Create cipher from alphabet specification
pub fn from_spec(key: &[u8], tweak: &[u8], spec: AlphabetSpec) -> Result<FF3, String> {
    if spec.charset.len() < 2 {
        return Err("alphabet must have >=2 chars".to_string());
    }

    let radix = spec.charset.len();
    let core = FF3Cipher::new(radix, key, tweak)?;
    Ok(FF3::new(core, spec))
}

/// Create cipher for digits (0-9)
pub fn digits(key: &[u8], tweak: &[u8]) -> Result<FF3, String> {
    from_spec(key, tweak, spec_digits())
}

/// Create cipher for lowercase hex (0-9a-f)
pub fn hex_lower(key: &[u8], tweak: &[u8]) -> Result<FF3, String> {
    from_spec(key, tweak, spec_hex_lower())
}

/// Create cipher for uppercase hex (0-9A-F)
pub fn hex_upper(key: &[u8], tweak: &[u8]) -> Result<FF3, String> {
    from_spec(key, tweak, spec_hex_upper())
}

/// Create cipher for lowercase base36 (0-9a-z)
pub fn base36_lower(key: &[u8], tweak: &[u8]) -> Result<FF3, String> {
    from_spec(key, tweak, spec_base36_low())
}

/// Create cipher for uppercase base36 (0-9A-Z)
pub fn base36_upper(key: &[u8], tweak: &[u8]) -> Result<FF3, String> {
    from_spec(key, tweak, spec_base36_up())
}

/// Create cipher for base62 (0-9A-Za-z)
pub fn base62(key: &[u8], tweak: &[u8]) -> Result<FF3, String> {
    from_spec(key, tweak, spec_base62())
}

/// Create cipher for hex (lowercase, for compatibility)
pub fn hex(key: &[u8], tweak: &[u8]) -> Result<FF3, String> {
    hex_lower(key, tweak)
}

/// Create cipher for base36 (lowercase, for compatibility)
pub fn base36(key: &[u8], tweak: &[u8]) -> Result<FF3, String> {
    base36_lower(key, tweak)
}

/// Create cipher for radix 26 (first 26 chars of base36: 0-9a-p)
pub fn radix26(key: &[u8], tweak: &[u8]) -> Result<FF3, String> {
    let spec = AlphabetSpec::new(&ALPHA_BASE36_LOW[..26]);
    from_spec(key, tweak, spec)
}