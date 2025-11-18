/*!
FF3 Format Preserving Encryption - Rust Implementation
Experimental implementation for research and educational purposes.

⚠️ FF3 was withdrawn by NIST due to security vulnerabilities.
This implementation is for educational and research purposes only.
*/

pub mod alphabets;
pub mod core;
pub mod api;

// Re-export public API
pub use api::{
    FF3,
    from_spec,
    digits,
    hex,
    hex_lower,
    hex_upper,
    base36,
    base36_lower,
    base36_upper,
    base62,
    radix26,
};

pub use alphabets::{
    AlphabetSpec,
    ALPHA_DIGITS,
    ALPHA_HEX_LOWER,
    ALPHA_HEX_UPPER,
    ALPHA_BASE36_LOW,
    ALPHA_BASE36_UP,
    ALPHA_BASE62,
    spec_digits,
    spec_hex_lower,
    spec_hex_upper,
    spec_base36_low,
    spec_base36_up,
    spec_base62,
};

#[cfg(test)]
mod tests {
    use super::*;
    use serde::Deserialize;
    use std::fs;

    #[derive(Debug, Deserialize, Clone)]
    struct NistTestVector {
        sample: u32,
        algorithm: String,
        key: String,
        radix: u32,
        plaintext: String,
        tweak: String,
        ciphertext: String,
    }

    #[derive(Debug, Deserialize)]
    struct NistTestVectors {
        description: String,
        source: String,
        algorithm: String,
        specification: String,
        note: String,
        status: String,
        warning: String,
        vectors: Vec<NistTestVector>,
    }

    fn load_nist_test_vectors() -> Result<(NistTestVectors, Vec<NistTestVector>), Box<dyn std::error::Error>> {
        let data = fs::read_to_string("../../shared/test-vectors/nist_ff3_official_vectors.json")?;
        let vectors: NistTestVectors = serde_json::from_str(&data)?;
        let test_vectors = vectors.vectors.clone();
        Ok((vectors, test_vectors))
    }

    #[test]
    fn test_basic_functionality() {
        let key = hex::decode("EF4359D8D580AA4F7F036D6F04FC6A94").unwrap();
        let tweak = hex::decode("D8E7920AFA330A73").unwrap();

        let cipher = digits(&key, &tweak).unwrap();

        let plaintext = "1234567890";
        let ciphertext = cipher.encrypt(plaintext, None).unwrap();
        let decrypted = cipher.decrypt(&ciphertext, None).unwrap();

        assert_eq!(decrypted, plaintext);
        assert_ne!(ciphertext, plaintext);
        println!("Basic test: {} -> {} -> {}", plaintext, ciphertext, decrypted);

        // Test against first NIST vector
        let nist_plaintext = "890121234567890000";
        let nist_expected = "750918814058654607";
        let nist_result = cipher.encrypt(nist_plaintext, None).unwrap();
        println!("NIST test: {} -> {} (expected: {})", nist_plaintext, nist_result, nist_expected);
        assert_eq!(nist_result, nist_expected);
    }

    #[test]
    fn test_format_preservation() {
        let key = hex::decode("EF4359D8D580AA4F7F036D6F04FC6A94").unwrap();
        let tweak = hex::decode("D8E7920AFA330A73").unwrap();

        // Test base62 case preservation
        let cipher = base62(&key, &tweak).unwrap();
        let test_cases = ["Ab9Z", "Hello123", "TestCase"];

        for plaintext in &test_cases {
            let ciphertext = cipher.encrypt(plaintext, None).unwrap();
            let decrypted = cipher.decrypt(&ciphertext, None).unwrap();

            assert_eq!(decrypted, *plaintext);
            println!("Format preservation: {} -> {} -> {}", plaintext, ciphertext, decrypted);
        }
    }

    #[test]
    fn test_nist_vectors() {
        let (metadata, vectors) = load_nist_test_vectors().expect("Failed to load NIST test vectors");

        println!("🧪 {} - {}", metadata.description, metadata.algorithm);
        println!("📋 Source: {} ({})", metadata.source, metadata.specification);
        if !metadata.warning.is_empty() {
            println!("⚠️  {}", metadata.warning);
        }
        if !metadata.note.is_empty() {
            println!("📝 Note: {}", metadata.note);
        }
        println!("🔍 Status: {}", metadata.status);
        println!("🚀 Testing {} NIST FF3 vectors...\n", vectors.len());

        for vector in &vectors {
            // Decode hex key and tweak
            let key = hex::decode(&vector.key).unwrap();
            let tweak = hex::decode(&vector.tweak).unwrap();

            // Create cipher based on radix
            let cipher = match vector.radix {
                10 => digits(&key, &tweak).unwrap(),
                16 => hex(&key, &tweak).unwrap(),
                26 => radix26(&key, &tweak).unwrap(),
                36 => base36(&key, &tweak).unwrap(),
                62 => base62(&key, &tweak).unwrap(),
                _ => panic!("Unsupported radix {} in test vectors", vector.radix),
            };

            // Test encryption
            let encrypted = cipher.encrypt(&vector.plaintext, None).unwrap();

            println!("Sample {} ({}):", vector.sample, vector.algorithm);
            println!("  Key: {}", vector.key);
            println!("  Tweak: {}", vector.tweak);
            println!("  Plaintext: {}", vector.plaintext);
            println!("  Expected: {}", vector.ciphertext);
            println!("  Got:      {}", encrypted);

            if encrypted == vector.ciphertext {
                println!("  ✅ PERFECT MATCH!");
            } else {
                println!("  ❌ MISMATCH");
            }

            // Assert exact match with NIST vectors
            assert_eq!(encrypted, vector.ciphertext, "Sample {} failed", vector.sample);

            // Test decryption (round-trip)
            let decrypted = cipher.decrypt(&encrypted, None).unwrap();
            assert_eq!(decrypted, vector.plaintext, "Round-trip failed for sample {}", vector.sample);
            println!("  ✅ Round-trip successful");
            println!();
        }
    }
}