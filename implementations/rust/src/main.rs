// FF3 CLI - Command-line tool for FF3 encryption/decryption
use std::env;

use fpe_ff3::{
    digits, hex_lower, hex_upper, base36_lower, base36_upper, base62,
    from_spec, AlphabetSpec
};

fn print_usage(program_name: &str) {
    println!("FF3 CLI - Format Preserving Encryption");
    println!();
    println!("Usage: {} [OPTIONS]", program_name);
    println!();
    println!("Options:");
    println!("  -e, --encrypt TEXT      Encrypt the given text");
    println!("  -d, --decrypt TEXT      Decrypt the given text");
    println!("  -k, --key HEX           AES key in hex format (32/48/64 hex chars)");
    println!("  -t, --tweak HEX         Tweak in hex format (16 hex chars)");
    println!("  -a, --alphabet TYPE     Alphabet type:");
    println!("                            digits (default)");
    println!("                            hex-lower");
    println!("                            hex-upper");
    println!("                            base36-lower");
    println!("                            base36-upper");
    println!("                            base62");
    println!("  -c, --custom CHARSET    Custom alphabet charset");
    println!("  -h, --help              Show this help message");
    println!();
    println!("Examples:");
    println!("  {} -e \"1234567890\" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73", program_name);
    println!("  {} -d \"7501889140\" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73", program_name);
    println!();
}

fn create_cipher(key: &[u8], tweak: &[u8], alphabet_type: &str, custom_charset: Option<&str>) -> Result<fpe_ff3::FF3, Box<dyn std::error::Error>> {
    if let Some(charset) = custom_charset {
        let spec = AlphabetSpec::new(charset);
        return Ok(from_spec(key, tweak, spec)?);
    }

    match alphabet_type {
        "digits" => Ok(digits(key, tweak)?),
        "hex-lower" => Ok(hex_lower(key, tweak)?),
        "hex-upper" => Ok(hex_upper(key, tweak)?),
        "base36-lower" => Ok(base36_lower(key, tweak)?),
        "base36-upper" => Ok(base36_upper(key, tweak)?),
        "base62" => Ok(base62(key, tweak)?),
        _ => Err(format!("Unknown alphabet type: {}", alphabet_type).into()),
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    let program_name = &args[0];

    let mut i = 1;
    let mut encrypt_text: Option<String> = None;
    let mut decrypt_text: Option<String> = None;
    let mut key_hex: Option<String> = None;
    let mut tweak_hex: Option<String> = None;
    let mut alphabet_type = "digits";
    let mut custom_charset: Option<String> = None;

    while i < args.len() {
        match args[i].as_str() {
            "-h" | "--help" => {
                print_usage(program_name);
                return Ok(());
            }
            "-e" | "--encrypt" => {
                if i + 1 < args.len() {
                    encrypt_text = Some(args[i + 1].clone());
                    i += 1;
                } else {
                    return Err("Missing value for --encrypt".into());
                }
            }
            "-d" | "--decrypt" => {
                if i + 1 < args.len() {
                    decrypt_text = Some(args[i + 1].clone());
                    i += 1;
                } else {
                    return Err("Missing value for --decrypt".into());
                }
            }
            "-k" | "--key" => {
                if i + 1 < args.len() {
                    key_hex = Some(args[i + 1].clone());
                    i += 1;
                } else {
                    return Err("Missing value for --key".into());
                }
            }
            "-t" | "--tweak" => {
                if i + 1 < args.len() {
                    tweak_hex = Some(args[i + 1].clone());
                    i += 1;
                } else {
                    return Err("Missing value for --tweak".into());
                }
            }
            "-a" | "--alphabet" => {
                if i + 1 < args.len() {
                    alphabet_type = &args[i + 1];
                    i += 1;
                } else {
                    return Err("Missing value for --alphabet".into());
                }
            }
            "-c" | "--custom" => {
                if i + 1 < args.len() {
                    custom_charset = Some(args[i + 1].clone());
                    i += 1;
                } else {
                    return Err("Missing value for --custom".into());
                }
            }
            _ => {
                return Err(format!("Unknown argument: {}", args[i]).into());
            }
        }
        i += 1;
    }

    if encrypt_text.is_none() && decrypt_text.is_none() {
        print_usage(program_name);
        return Ok(());
    }

    let key_hex = key_hex.ok_or("Key is required")?;
    let tweak_hex = tweak_hex.ok_or("Tweak is required")?;

    // Parse key and tweak
    let key = hex::decode(&key_hex).map_err(|_| "Invalid key format")?;
    if key.len() != 16 && key.len() != 24 && key.len() != 32 {
        return Err("Key must be 16, 24, or 32 bytes (32, 48, or 64 hex chars)".into());
    }

    let tweak = hex::decode(&tweak_hex).map_err(|_| "Invalid tweak format")?;
    if tweak.len() != 8 {
        return Err("Tweak must be 8 bytes (16 hex chars)".into());
    }

    // Create cipher
    let cipher = create_cipher(&key, &tweak, alphabet_type, custom_charset.as_deref())?;

    // Encrypt or decrypt
    if let Some(plaintext) = encrypt_text {
        let result = cipher.encrypt(&plaintext, None)?;
        println!("{}", result);
    } else if let Some(ciphertext) = decrypt_text {
        let result = cipher.decrypt(&ciphertext, None)?;
        println!("{}", result);
    }

    Ok(())
}