// FF3 NIST Test Vector Validation Tool
use fpe_ff3::{digits, hex, radix26, base36, base62};
use serde::Deserialize;
use std::fs;
use std::env;

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

struct Options {
    vectors_path: Option<String>,
    verbose: bool,
    quiet: bool,
    help: bool,
}

fn parse_args(args: &[String]) -> Result<Options, String> {
    let mut opts = Options {
        vectors_path: None,
        verbose: false,
        quiet: false,
        help: false,
    };

    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "-h" | "--help" => {
                opts.help = true;
                return Ok(opts);
            }
            "--vectors" => {
                i += 1;
                if i >= args.len() {
                    return Err("Missing value for --vectors".to_string());
                }
                opts.vectors_path = Some(args[i].clone());
            }
            "--verbose" => {
                opts.verbose = true;
            }
            "--quiet" => {
                opts.quiet = true;
            }
            unknown => {
                return Err(format!("Unknown option: {}", unknown));
            }
        }
        i += 1;
    }

    Ok(opts)
}

fn show_usage() {
    println!("FF3 NIST Test Vector Validation Tool");
    println!();
    println!("Usage: ff3-validate [OPTIONS]");
    println!();
    println!("Options:");
    println!("  --vectors PATH    Path to test vectors JSON file");
    println!("  --verbose         Show detailed test output");
    println!("  --quiet           Only show failures and summary");
    println!("  -h, --help        Show this help message");
    println!();
}

fn find_vectors_file(custom_path: Option<String>) -> Result<String, String> {
    if let Some(path) = custom_path {
        if std::path::Path::new(&path).exists() {
            return Ok(path);
        }
        return Err(format!("Vectors file not found: {}", path));
    }

    let paths = vec![
        "../shared/test-vectors/nist_ff3_official_vectors.json",
        "../../shared/test-vectors/nist_ff3_official_vectors.json",
        "../../../shared/test-vectors/nist_ff3_official_vectors.json",
    ];

    for path in paths {
        if std::path::Path::new(path).exists() {
            return Ok(path.to_string());
        }
    }

    Err("Could not find NIST test vectors file".to_string())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().skip(1).collect();

    let opts = match parse_args(&args) {
        Ok(opts) => opts,
        Err(e) => {
            eprintln!("Error: {}", e);
            show_usage();
            std::process::exit(1);
        }
    };

    if opts.help {
        show_usage();
        return Ok(());
    }

    if !opts.quiet {
        println!("FF3 NIST Test Vector Validation Tool");
        println!("========================================");
        println!();
    }

    // Find and load test vectors
    let vectors_path = match find_vectors_file(opts.vectors_path) {
        Ok(path) => path,
        Err(e) => {
            eprintln!("Error: {}", e);
            std::process::exit(2);
        }
    };

    let data = fs::read_to_string(&vectors_path)
        .map_err(|e| format!("Could not read vectors file: {}", e))?;

    let vectors: NistTestVectors = serde_json::from_str(&data)?;

    if !opts.quiet {
        println!("{}", vectors.description);
        println!("Source: {} ({})", vectors.source, vectors.specification);
        if !vectors.warning.is_empty() {
            println!("WARNING: {}", vectors.warning);
        }
        if !vectors.note.is_empty() {
            println!("Note: {}", vectors.note);
        }
        println!("Status: {}", vectors.status);
        println!();
        println!("Testing {} NIST FF3 vectors...", vectors.vectors.len());
        println!();
    }

    let mut passed = 0;
    let mut failed = 0;

    for vector in &vectors.vectors {
        // Decode hex key and tweak
        let key = hex::decode(&vector.key)?;
        let tweak = hex::decode(&vector.tweak)?;

        // Create cipher based on radix
        let cipher = match vector.radix {
            10 => digits(&key, &tweak)?,
            16 => hex(&key, &tweak)?,
            26 => radix26(&key, &tweak)?,
            36 => base36(&key, &tweak)?,
            62 => base62(&key, &tweak)?,
            _ => return Err(format!("Unsupported radix {} in test vectors", vector.radix).into()),
        };

        // Test encryption
        let encrypted = cipher.encrypt(&vector.plaintext, None)?;

        let encrypt_passed = encrypted == vector.ciphertext;
        let mut roundtrip_passed = true;

        if encrypt_passed {
            // Test decryption (round-trip)
            let decrypted = cipher.decrypt(&encrypted, None)?;
            roundtrip_passed = decrypted == vector.plaintext;
        }

        let test_passed = encrypt_passed && roundtrip_passed;

        if test_passed {
            passed += 1;
            if opts.verbose {
                println!("Sample {} ({}): PASS", vector.sample, vector.algorithm);
            }
        } else {
            failed += 1;
            if !opts.quiet {
                println!("Sample {} ({}): FAIL", vector.sample, vector.algorithm);
                if !encrypt_passed {
                    println!("  Expected: {}", vector.ciphertext);
                    println!("  Got:      {}", encrypted);
                }
                if !roundtrip_passed {
                    println!("  Round-trip failed");
                }
            }
        }
    }

    if !opts.quiet {
        println!();
        println!("Results: {} passed, {} failed", passed, failed);
        println!("Success rate: {:.1}%", (passed as f64 / vectors.vectors.len() as f64) * 100.0);
    }

    if failed > 0 {
        if !opts.quiet {
            println!();
            println!("VALIDATION FAILED");
        }
        std::process::exit(1);
    }

    if !opts.quiet {
        println!();
        println!("ALL NIST TEST VECTORS PASSED!");
        println!();
        println!("WARNING: FF3 was WITHDRAWN by NIST due to security vulnerabilities.");
        println!("This implementation is for EDUCATIONAL and RESEARCH purposes only.");
        println!("DO NOT use in production systems.");
        println!();
    }

    Ok(())
}
