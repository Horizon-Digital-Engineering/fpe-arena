/// FF3 Core - Pure cryptographic implementation of FF3 Format Preserving Encryption.

use aes::{Aes128, Aes192, Aes256, Block};
use aes::cipher::{BlockEncrypt, KeyInit};
use num_bigint::BigUint;
use num_traits::{Zero, One};
use num_integer::Integer;

/// Core FF3 cipher instance with pure cryptographic math
pub struct FF3Cipher {
    radix: usize,
    aes_cipher: Box<dyn AesEncryptor>,
    tweak: Vec<u8>,
    min_len: usize,
    max_len: usize,
}

/// Trait to abstract over different AES key sizes
trait AesEncryptor: Send + Sync {
    fn encrypt_block(&self, block: &mut Block);
}

struct Aes128Encryptor(Aes128);
struct Aes192Encryptor(Aes192);
struct Aes256Encryptor(Aes256);

impl AesEncryptor for Aes128Encryptor {
    fn encrypt_block(&self, block: &mut Block) {
        self.0.encrypt_block(block);
    }
}

impl AesEncryptor for Aes192Encryptor {
    fn encrypt_block(&self, block: &mut Block) {
        self.0.encrypt_block(block);
    }
}

impl AesEncryptor for Aes256Encryptor {
    fn encrypt_block(&self, block: &mut Block) {
        self.0.encrypt_block(block);
    }
}

impl FF3Cipher {
    /// Create a new FF3 cipher with the specified radix, key, and tweak
    pub fn new(radix: usize, key: &[u8], tweak: &[u8]) -> Result<Self, String> {
        // Validate radix - relaxed for experimental repo
        if radix < 2 || radix > 62 {
            return Err(format!("radix must be between 2 and 62, got {}", radix));
        }

        // Validate key length
        if ![16, 24, 32].contains(&key.len()) {
            return Err(format!("key length must be 16, 24, or 32 bytes, got {}", key.len()));
        }

        // Validate tweak length (FF3 requires exactly 64 bits = 8 bytes)
        if tweak.len() != 8 {
            return Err(format!("tweak must be exactly 8 bytes (64 bits) for FF3, got {}", tweak.len()));
        }

        // Create AES cipher with FF3 byte-reversal convention
        let aes_cipher = Self::create_aes_cipher(key)?;

        // Calculate maximum length for FF3 (more permissive for experimental use)
        let max_len = if radix >= 2 && radix <= 36 {
            32  // For small radixes, allow reasonable lengths
        } else {
            56  // Conservative maximum for most practical use cases
        };

        Ok(FF3Cipher {
            radix,
            aes_cipher,
            tweak: tweak.to_vec(),
            min_len: 2,      // Minimum length for FF3
            max_len,         // Maximum length based on practical constraints
        })
    }

    /// Create AES cipher with FF3 byte-reversal convention
    fn create_aes_cipher(key: &[u8]) -> Result<Box<dyn AesEncryptor>, String> {
        // FF3 specification requires byte reversal of the key
        let mut reversed_key = key.to_vec();
        reversed_key.reverse();

        match key.len() {
            16 => {
                let cipher = Aes128::new_from_slice(&reversed_key)
                    .map_err(|e| format!("Failed to create AES-128 cipher: {:?}", e))?;
                Ok(Box::new(Aes128Encryptor(cipher)))
            }
            24 => {
                let cipher = Aes192::new_from_slice(&reversed_key)
                    .map_err(|e| format!("Failed to create AES-192 cipher: {:?}", e))?;
                Ok(Box::new(Aes192Encryptor(cipher)))
            }
            32 => {
                let cipher = Aes256::new_from_slice(&reversed_key)
                    .map_err(|e| format!("Failed to create AES-256 cipher: {:?}", e))?;
                Ok(Box::new(Aes256Encryptor(cipher)))
            }
            _ => Err(format!("Invalid key length: {}", key.len())),
        }
    }

    pub fn get_min_len(&self) -> usize {
        self.min_len
    }

    pub fn get_max_len(&self) -> usize {
        self.max_len
    }

    /// Encrypt digit array using FF3 algorithm
    pub fn encrypt_digits(&self, plaintext: &[usize], additional_tweak: Option<&[u8]>) -> Vec<usize> {
        // Merge tweaks - main tweak + optional additional tweak
        let effective_tweak = self.combine_tweaks(additional_tweak);
        self.ff3_encrypt(plaintext, &effective_tweak)
    }

    /// Decrypt digit array using FF3 algorithm
    pub fn decrypt_digits(&self, ciphertext: &[usize], additional_tweak: Option<&[u8]>) -> Vec<usize> {
        // Merge tweaks - main tweak + optional additional tweak
        let effective_tweak = self.combine_tweaks(additional_tweak);
        self.ff3_decrypt(ciphertext, &effective_tweak)
    }

    /// Core FF3 encryption algorithm
    fn ff3_encrypt(&self, plaintext: &[usize], tweak: &[u8]) -> Vec<usize> {
        let n = plaintext.len();

        // Split plaintext into left and right halves (u = ceil(n/2), v = n - u)
        let u = (n + 1) / 2;  // This gives ceil(n/2)
        let v = n - u;

        let mut a = plaintext[..u].to_vec();
        let mut b = plaintext[u..].to_vec();

        // Perform 8 Feistel rounds according to FF3 specification
        for i in 0..8 {
            if i % 2 == 0 {
                // Even round: use B to update A
                let w = self.calculate_w(tweak, i, &b);
                let p = self.calculate_p(i, &w, &b);
                let m = self.radix_power(u);

                // FF3 uses reversed digit order: NUM_radix(REV(A))
                let mut reversed_a = a.clone();
                reversed_a.reverse();
                let a_num = self.num_array_to_bigint(&reversed_a);

                // c = (NUM_radix(REV(A)) + NUM(S)) mod radix^u
                let y = (&a_num + &p) % &m;

                // C = REV(STR_radix(c))
                let mut new_digits = self.bigint_to_num_array(&y, u);
                new_digits.reverse();
                a = new_digits;
            } else {
                // Odd round: use A to update B
                let w = self.calculate_w(tweak, i, &a);
                let p = self.calculate_p(i, &w, &a);
                let m = self.radix_power(v);

                // FF3 uses reversed digit order: NUM_radix(REV(B))
                let mut reversed_b = b.clone();
                reversed_b.reverse();
                let b_num = self.num_array_to_bigint(&reversed_b);

                // c = (NUM_radix(REV(B)) + NUM(S)) mod radix^v
                let y = (&b_num + &p) % &m;

                // C = REV(STR_radix(c))
                let mut new_digits = self.bigint_to_num_array(&y, v);
                new_digits.reverse();
                b = new_digits;
            }
        }

        // Combine final A and B
        let mut result = a;
        result.extend(b);
        result
    }

    /// Core FF3 decryption algorithm
    fn ff3_decrypt(&self, ciphertext: &[usize], tweak: &[u8]) -> Vec<usize> {
        let n = ciphertext.len();

        // Split ciphertext into left and right halves
        let u = (n + 1) / 2;
        let v = n - u;

        let mut a = ciphertext[..u].to_vec();
        let mut b = ciphertext[u..].to_vec();

        // Perform 8 Feistel rounds in reverse order
        for i in (0..8).rev() {
            if i % 2 == 0 {
                // Even round: use B to update A (reverse)
                let w = self.calculate_w(tweak, i, &b);
                let p = self.calculate_p(i, &w, &b);
                let m = self.radix_power(u);

                // FF3 uses reversed digit order: NUM_radix(REV(A))
                let mut reversed_a = a.clone();
                reversed_a.reverse();
                let a_num = self.num_array_to_bigint(&reversed_a);

                // c = (NUM_radix(REV(A)) - NUM(S)) mod radix^u
                // Use proper modular subtraction: (a - b) mod m = (a + m - (b mod m)) mod m
                let p_mod = p % &m;
                let y = (&a_num + &m - &p_mod) % &m;

                // C = REV(STR_radix(c))
                let mut new_digits = self.bigint_to_num_array(&y, u);
                new_digits.reverse();
                a = new_digits;
            } else {
                // Odd round: use A to update B (reverse)
                let w = self.calculate_w(tweak, i, &a);
                let p = self.calculate_p(i, &w, &a);
                let m = self.radix_power(v);

                // FF3 uses reversed digit order: NUM_radix(REV(B))
                let mut reversed_b = b.clone();
                reversed_b.reverse();
                let b_num = self.num_array_to_bigint(&reversed_b);

                // c = (NUM_radix(REV(B)) - NUM(S)) mod radix^v
                // Use proper modular subtraction: (a - b) mod m = (a + m - (b mod m)) mod m
                let p_mod = p % &m;
                let y = (&b_num + &m - &p_mod) % &m;

                // C = REV(STR_radix(c))
                let mut new_digits = self.bigint_to_num_array(&y, v);
                new_digits.reverse();
                b = new_digits;
            }
        }

        // Combine final A and B
        let mut result = a;
        result.extend(b);
        result
    }

    /// Calculate W for FF3 round function per NIST specification
    fn calculate_w(&self, tweak: &[u8], round_num: usize, _block: &[usize]) -> Vec<u8> {
        // NIST FF3 W calculation: split 8-byte tweak into Tl (first 4 bytes) and Tr (last 4 bytes)
        let mut w = vec![0u8; 4];

        if round_num % 2 == 0 {
            // Even rounds: W = Tr (rightmost 4 bytes)
            w.copy_from_slice(&tweak[4..8]);
        } else {
            // Odd rounds: W = Tl (leftmost 4 bytes)
            w.copy_from_slice(&tweak[..4]);
        }

        w
    }

    /// Calculate P value for FF3 round function using AES encryption
    fn calculate_p(&self, round_num: usize, w: &[u8], block: &[usize]) -> BigUint {
        // NIST FF3 P calculation with proper byte reversal
        // P = REVB(AES-ECB(K, REVB(W XOR [i]_4 || NUM_radix(REV(B)))))

        // Create 16-byte input
        let mut input = [0u8; 16];

        // First 4 bytes: W XOR with round number in the last byte of W
        input[..4].copy_from_slice(w);
        input[3] ^= round_num as u8;

        // Last 12 bytes: NUM_radix(REV(B)) - reverse digits of B first
        let mut reversed_block = block.to_vec();
        reversed_block.reverse();
        let block_num = self.num_array_to_bigint(&reversed_block);

        // Convert to bytes with proper padding to 12 bytes
        let block_bytes = if block_num.is_zero() {
            vec![0u8; 12]
        } else {
            let bytes = block_num.to_bytes_be();
            if bytes.len() <= 12 {
                let mut padded = vec![0u8; 12 - bytes.len()];
                padded.extend(bytes);
                padded
            } else {
                bytes[bytes.len() - 12..].to_vec()
            }
        };

        input[4..].copy_from_slice(&block_bytes);

        // Apply FF3 byte reversal convention: REVB before AES
        let mut reversed_input = input;
        reversed_input.reverse();

        // Encrypt with AES
        let mut aes_output = Block::from(reversed_input);
        self.aes_cipher.encrypt_block(&mut aes_output);

        // Apply FF3 byte reversal convention: REVB after AES
        let mut output = aes_output.to_vec();
        output.reverse();

        // Convert to BigUint
        BigUint::from_bytes_be(&output)
    }

    /// Convert digit array to BigUint using radix
    fn num_array_to_bigint(&self, digits: &[usize]) -> BigUint {
        let mut result = BigUint::zero();
        let radix = BigUint::from(self.radix);

        for &digit in digits {
            result = &result * &radix + BigUint::from(digit);
        }

        result
    }

    /// Convert BigUint to digit array with specified length
    fn bigint_to_num_array(&self, num: &BigUint, length: usize) -> Vec<usize> {
        if num.is_zero() {
            return vec![0; length];
        }

        let mut digits = Vec::new();
        let mut temp = num.clone();
        let radix = BigUint::from(self.radix);

        while !temp.is_zero() {
            let (quotient, remainder) = temp.div_rem(&radix);
            // Convert remainder to usize safely
            let remainder_u64_digits = remainder.to_u64_digits();
            let digit = if remainder_u64_digits.is_empty() {
                0
            } else {
                remainder_u64_digits[0] as usize
            };
            digits.push(digit);
            temp = quotient;
        }

        // Pad with zeros to required length
        while digits.len() < length {
            digits.push(0);
        }

        // Reverse to get most significant digit first
        digits.reverse();
        digits
    }

    /// Calculate radix^length for modular arithmetic
    fn radix_power(&self, length: usize) -> BigUint {
        let radix = BigUint::from(self.radix);
        let mut result = BigUint::one();
        for _ in 0..length {
            result *= &radix;
        }
        result
    }

    /// Combine base tweak with additional tweak for domain separation
    fn combine_tweaks(&self, additional_tweak: Option<&[u8]>) -> Vec<u8> {
        match additional_tweak {
            None => self.tweak.clone(),
            Some(additional) => {
                let mut combined = self.tweak.clone();
                // XOR with additional tweak bytes
                for (i, &byte) in additional.iter().enumerate() {
                    if i < 8 {
                        combined[i] ^= byte;
                    }
                }
                combined
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_key() -> Vec<u8> {
        hex::decode("EF4359D8D580AA4F7F036D6F04FC6A94").unwrap()
    }

    fn test_tweak() -> Vec<u8> {
        hex::decode("D8E7920AFA330A73").unwrap()
    }

    #[test]
    fn test_cipher_creation() {
        // Valid parameters
        assert!(FF3Cipher::new(10, &test_key(), &test_tweak()).is_ok());
        assert!(FF3Cipher::new(26, &test_key(), &test_tweak()).is_ok());
        assert!(FF3Cipher::new(62, &test_key(), &test_tweak()).is_ok());

        // Invalid radix
        assert!(FF3Cipher::new(1, &test_key(), &test_tweak()).is_err());
        assert!(FF3Cipher::new(63, &test_key(), &test_tweak()).is_err());

        // Invalid key length
        let short_key = vec![0u8; 8];
        assert!(FF3Cipher::new(10, &short_key, &test_tweak()).is_err());

        // Invalid tweak length
        let short_tweak = vec![0u8; 4];
        assert!(FF3Cipher::new(10, &test_key(), &short_tweak).is_err());
    }

    #[test]
    fn test_different_key_sizes() {
        let key128 = vec![0u8; 16];
        let key192 = vec![0u8; 24];
        let key256 = vec![0u8; 32];
        let tweak = test_tweak();

        assert!(FF3Cipher::new(10, &key128, &tweak).is_ok());
        assert!(FF3Cipher::new(10, &key192, &tweak).is_ok());
        assert!(FF3Cipher::new(10, &key256, &tweak).is_ok());
    }

    #[test]
    fn test_encrypt_decrypt_roundtrip() {
        let cipher = FF3Cipher::new(10, &test_key(), &test_tweak()).unwrap();
        let plaintext_digits = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 0];

        let ciphertext_digits = cipher.encrypt_digits(&plaintext_digits, None);
        let decrypted_digits = cipher.decrypt_digits(&ciphertext_digits, None);

        assert_eq!(decrypted_digits, plaintext_digits);
        assert_ne!(ciphertext_digits, plaintext_digits); // Should be different with high probability
    }

    #[test]
    fn test_different_inputs_produce_different_outputs() {
        let cipher = FF3Cipher::new(10, &test_key(), &test_tweak()).unwrap();

        let input1 = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 0];
        let input2 = vec![0, 9, 8, 7, 6, 5, 4, 3, 2, 1];

        let output1 = cipher.encrypt_digits(&input1, None);
        let output2 = cipher.encrypt_digits(&input2, None);

        assert_ne!(output1, output2);
    }

    #[test]
    fn test_same_input_produces_same_output() {
        let cipher = FF3Cipher::new(10, &test_key(), &test_tweak()).unwrap();
        let input = vec![1, 2, 3, 4, 5];

        let output1 = cipher.encrypt_digits(&input, None);
        let output2 = cipher.encrypt_digits(&input, None);

        assert_eq!(output1, output2);
    }

    #[test]
    fn test_different_tweaks_produce_different_outputs() {
        let key = test_key();
        let tweak1 = test_tweak();
        let tweak2 = hex::decode("9A768A92F60E12D8").unwrap();

        let cipher1 = FF3Cipher::new(10, &key, &tweak1).unwrap();
        let cipher2 = FF3Cipher::new(10, &key, &tweak2).unwrap();

        let input = vec![1, 2, 3, 4, 5];
        let output1 = cipher1.encrypt_digits(&input, None);
        let output2 = cipher2.encrypt_digits(&input, None);

        assert_ne!(output1, output2);
    }

    #[test]
    fn test_length_constraints() {
        let cipher = FF3Cipher::new(10, &test_key(), &test_tweak()).unwrap();

        assert_eq!(cipher.get_min_len(), 2);
        assert!(cipher.get_max_len() > 10);
    }

    #[test]
    fn test_radix_preservation() {
        let cipher = FF3Cipher::new(26, &test_key(), &test_tweak()).unwrap();
        let input = vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18];

        let output = cipher.encrypt_digits(&input, None);

        // All output digits should be in valid range for radix 26
        for &digit in &output {
            assert!(digit < 26);
        }

        // Should be same length
        assert_eq!(output.len(), input.len());
    }

    #[test]
    fn test_bigint_conversion() {
        let cipher = FF3Cipher::new(10, &test_key(), &test_tweak()).unwrap();

        let digits = vec![1, 2, 3, 4, 5];
        let bigint = cipher.num_array_to_bigint(&digits);
        let converted_back = cipher.bigint_to_num_array(&bigint, digits.len());

        assert_eq!(converted_back, digits);
    }

    #[test]
    fn test_tweak_combination() {
        let cipher = FF3Cipher::new(10, &test_key(), &test_tweak()).unwrap();

        let base_tweak = cipher.combine_tweaks(None);
        let additional = vec![0x01, 0x02, 0x03, 0x04];
        let combined_tweak = cipher.combine_tweaks(Some(&additional));

        assert_eq!(base_tweak, test_tweak());
        assert_ne!(combined_tweak, base_tweak);
        assert_eq!(combined_tweak.len(), 8);
    }
}