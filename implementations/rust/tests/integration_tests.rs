// Integration tests for FF3 library
// Tests the public API end-to-end using use statements to import the public interface

use fpe_ff3::{digits, hex_lower, hex_upper, base36_lower, base36_upper, base62, radix26, from_spec, AlphabetSpec};
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

fn load_nist_test_vectors() -> Result<Vec<NistTestVector>, Box<dyn std::error::Error>> {
    // Try environment variable first (for CI/CD)
    let data = if let Ok(path) = std::env::var("FF3_TEST_VECTORS_PATH") {
        fs::read_to_string(&path)
            .map_err(|_| format!("Could not read NIST test vectors from env path: {}", path))?
    } else {
        // Fallback to relative paths for local development
        fs::read_to_string("../shared/test-vectors/nist_ff3_official_vectors.json")
            .or_else(|_| fs::read_to_string("../../shared/test-vectors/nist_ff3_official_vectors.json"))
            .map_err(|_| "Could not find NIST test vectors file at relative paths")?
    };

    let vectors: NistTestVectors = serde_json::from_str(&data)?;
    Ok(vectors.vectors)
}

#[test]
fn test_public_api_roundtrip() {
    let key = hex::decode("EF4359D8D580AA4F7F036D6F04FC6A94").unwrap();
    let tweak = hex::decode("D8E7920AFA330A73").unwrap();

    // Test digits
    let cipher = digits(&key, &tweak).unwrap();
    let plaintext = "1234567890";
    let ciphertext = cipher.encrypt(plaintext, None).unwrap();
    let decrypted = cipher.decrypt(&ciphertext, None).unwrap();
    assert_eq!(decrypted, plaintext);
    assert_ne!(ciphertext, plaintext);

    // Test hex_lower
    let cipher = hex_lower(&key, &tweak).unwrap();
    let plaintext = "1234567890abcdef";
    let ciphertext = cipher.encrypt(plaintext, None).unwrap();
    let decrypted = cipher.decrypt(&ciphertext, None).unwrap();
    assert_eq!(decrypted, plaintext);

    // Test base62
    let cipher = base62(&key, &tweak).unwrap();
    let plaintext = "Hello123World";
    let ciphertext = cipher.encrypt(plaintext, None).unwrap();
    let decrypted = cipher.decrypt(&ciphertext, None).unwrap();
    assert_eq!(decrypted, plaintext);
}

#[test]
fn test_custom_alphabet() {
    let key = hex::decode("EF4359D8D580AA4F7F036D6F04FC6A94").unwrap();
    let tweak = hex::decode("D8E7920AFA330A73").unwrap();

    let spec = AlphabetSpec::new("!@#$%^&*()");
    let cipher = from_spec(&key, &tweak, spec).unwrap();

    let plaintext = "!@#$%^";
    let ciphertext = cipher.encrypt(plaintext, None).unwrap();
    let decrypted = cipher.decrypt(&ciphertext, None).unwrap();
    assert_eq!(decrypted, plaintext);
}

#[test]
fn test_alphabet_factories() {
    let key = hex::decode("EF4359D8D580AA4F7F036D6F04FC6A94").unwrap();
    let tweak = hex::decode("D8E7920AFA330A73").unwrap();

    // Test all factory functions
    assert!(digits(&key, &tweak).is_ok());
    assert!(hex_lower(&key, &tweak).is_ok());
    assert!(hex_upper(&key, &tweak).is_ok());
    assert!(base36_lower(&key, &tweak).is_ok());
    assert!(base36_upper(&key, &tweak).is_ok());
    assert!(base62(&key, &tweak).is_ok());
}

#[test]
fn test_case_preservation() {
    let key = hex::decode("EF4359D8D580AA4F7F036D6F04FC6A94").unwrap();
    let tweak = hex::decode("D8E7920AFA330A73").unwrap();

    // Test hex case preservation
    let hex_lower_cipher = hex_lower(&key, &tweak).unwrap();
    let hex_upper_cipher = hex_upper(&key, &tweak).unwrap();

    let lower_input = "1234567890abcdef";
    let upper_input = "1234567890ABCDEF";

    let lower_output = hex_lower_cipher.encrypt(lower_input, None).unwrap();
    let upper_output = hex_upper_cipher.encrypt(upper_input, None).unwrap();

    // Outputs should be different due to different alphabets
    assert_ne!(lower_output, upper_output);

    // Round-trip should preserve case
    let lower_decrypted = hex_lower_cipher.decrypt(&lower_output, None).unwrap();
    let upper_decrypted = hex_upper_cipher.decrypt(&upper_output, None).unwrap();

    assert_eq!(lower_decrypted, lower_input);
    assert_eq!(upper_decrypted, upper_input);
}

#[test]
fn test_nist_vectors_integration() {
    let vectors = match load_nist_test_vectors() {
        Ok(v) => v,
        Err(_) => {
            // Skip if test vectors not available
            println!("Skipping NIST vectors test - file not found");
            return;
        }
    };

    println!("Testing {} NIST FF3 vectors via public API...", vectors.len());

    for vector in &vectors {
        let key = hex::decode(&vector.key).unwrap();
        let tweak = hex::decode(&vector.tweak).unwrap();

        // Create cipher based on radix using public API
        let cipher = match vector.radix {
            10 => digits(&key, &tweak).unwrap(),
            16 => hex_lower(&key, &tweak).unwrap(), // NIST uses lowercase hex
            26 => radix26(&key, &tweak).unwrap(),
            36 => base36_lower(&key, &tweak).unwrap(),
            62 => base62(&key, &tweak).unwrap(),
            _ => panic!("Unsupported radix {} in test vectors", vector.radix),
        };

        // Test encryption
        let encrypted = cipher.encrypt(&vector.plaintext, None).unwrap();
        assert_eq!(encrypted, vector.ciphertext, "Sample {} failed", vector.sample);

        // Test decryption (round-trip)
        let decrypted = cipher.decrypt(&encrypted, None).unwrap();
        assert_eq!(decrypted, vector.plaintext, "Round-trip failed for sample {}", vector.sample);
    }

    println!("✅ All {} NIST vectors passed via public API!", vectors.len());
}

#[test]
fn test_error_handling() {
    let key = hex::decode("EF4359D8D580AA4F7F036D6F04FC6A94").unwrap();
    let tweak = hex::decode("D8E7920AFA330A73").unwrap();

    let cipher = digits(&key, &tweak).unwrap();

    // Test invalid characters
    assert!(cipher.encrypt("abc", None).is_err()); // Invalid for digits alphabet

    // Test empty string
    assert!(cipher.encrypt("", None).is_err());

    // Test single character (too short for FF3)
    assert!(cipher.encrypt("1", None).is_err());
}

#[test]
fn test_additional_tweak() {
    let key = hex::decode("EF4359D8D580AA4F7F036D6F04FC6A94").unwrap();
    let tweak = hex::decode("D8E7920AFA330A73").unwrap();

    let cipher = digits(&key, &tweak).unwrap();
    let plaintext = "1234567890";

    // Test with no additional tweak
    let result1 = cipher.encrypt(plaintext, None).unwrap();

    // Test with additional tweak
    let additional_tweak = vec![0x01, 0x02, 0x03, 0x04];
    let result2 = cipher.encrypt(plaintext, Some(&additional_tweak)).unwrap();

    // Results should be different
    assert_ne!(result1, result2);

    // Both should round-trip correctly
    let decrypted1 = cipher.decrypt(&result1, None).unwrap();
    let decrypted2 = cipher.decrypt(&result2, Some(&additional_tweak)).unwrap();

    assert_eq!(decrypted1, plaintext);
    assert_eq!(decrypted2, plaintext);
}

#[test]
fn test_different_radixes() {
    let key = hex::decode("EF4359D8D580AA4F7F036D6F04FC6A94").unwrap();
    let tweak = hex::decode("D8E7920AFA330A73").unwrap();

    // Test different common radixes
    let test_cases = vec![
        (digits(&key, &tweak).unwrap(), "1234567890"),
        (hex_lower(&key, &tweak).unwrap(), "1234567890abcdef"),
        (base36_lower(&key, &tweak).unwrap(), "hello123world"),
        (base62(&key, &tweak).unwrap(), "Hello123World"),
    ];

    for (cipher, plaintext) in test_cases {
        let ciphertext = cipher.encrypt(plaintext, None).unwrap();
        let decrypted = cipher.decrypt(&ciphertext, None).unwrap();
        assert_eq!(decrypted, plaintext);
        assert_ne!(ciphertext, plaintext);
    }
}