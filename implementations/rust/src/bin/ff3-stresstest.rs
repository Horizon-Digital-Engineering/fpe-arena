use fpe_ff3::{base36_lower, base36_upper, base62, digits, hex_lower, hex_upper, FF3};
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};
use std::env;
use std::time::Instant;

const DEFAULT_ITERATIONS: usize = 1000;
const DEFAULT_MIN_LEN: usize = 6;
const DEFAULT_MAX_LEN: usize = 20;
const DEFAULT_ALPHABETS: [&str; 4] = ["digits", "hex-lower", "base36-lower", "base62"];

#[derive(Clone)]
struct Options {
    iterations: usize,
    alphabets: Vec<String>,
    min_len: usize,
    max_len: usize,
    quick: bool,
    seed: Option<u64>,
    help: bool,
}

impl Default for Options {
    fn default() -> Self {
        Self {
            iterations: DEFAULT_ITERATIONS,
            alphabets: DEFAULT_ALPHABETS.iter().map(|s| s.to_string()).collect(),
            min_len: DEFAULT_MIN_LEN,
            max_len: DEFAULT_MAX_LEN,
            quick: false,
            seed: None,
            help: false,
        }
    }
}

struct AlphabetInfo {
    name: &'static str,
    charset: &'static str,
    factory: fn(&[u8], &[u8]) -> Result<FF3, String>,
}

fn main() {
    if let Err(err) = run() {
        eprintln!("Error: {}", err);
        std::process::exit(1);
    }
}

fn run() -> Result<(), String> {
    let opts = parse_args(env::args().skip(1))?;

    if opts.help {
        print_usage();
        return Ok(());
    }

    let mut opts = opts;
    if opts.quick {
        opts.iterations = 100;
    }

    validate_options(&opts)?;

    let mut rng = match opts.seed {
        Some(seed) => StdRng::seed_from_u64(seed),
        None => StdRng::from_entropy(),
    };

    println!("FF3 Stress Test v1.0");
    println!("====================\n");
    println!("Warning: FF3 was withdrawn by NIST; run for education and research only.\n");
    println!("Test configuration");
    println!("  Iterations per alphabet: {}", opts.iterations);
    println!("  Random key/tweak generation: enabled");
    println!("  String length range: {}-{} characters", opts.min_len, opts.max_len);
    println!("  Alphabets: {}\n", opts.alphabets.join(", "));

    let alph_infos = opts
        .alphabets
        .iter()
        .map(|name| alphabet_info(name))
        .collect::<Result<Vec<_>, _>>()?;

    let start = Instant::now();
    let mut total_tests = 0usize;
    let mut total_failures = 0usize;

    for info in alph_infos {
        let (tests, failures) = stress_alphabet(&info, &opts, &mut rng);
        total_tests += tests;
        total_failures += failures;
    }

    let duration = start.elapsed();
    println!("Summary");
    println!("  Total tests: {}", total_tests);
    println!("  Failures: {}", total_failures);
    println!("  Duration: {} ms", duration.as_millis());
    if duration.as_millis() > 0 {
        let throughput = (total_tests as f64 * 1000.0) / duration.as_millis() as f64;
        println!("  Throughput: {:.2} tests/sec", throughput);
    }
    if total_failures == 0 {
        println!("  Result: all stress tests passed");
        Ok(())
    } else {
        println!("  Result: failures detected");
        Err(String::from("failures detected"))
    }
}

fn stress_alphabet(info: &AlphabetInfo, opts: &Options, rng: &mut StdRng) -> (usize, usize) {
    println!("Testing {}...", info.name);
    println!("  Alphabet: {} (radix {})", info.charset, info.charset.len());

    let mut passed = 0usize;
    let mut failed = 0usize;
    let interval = usize::max(1, opts.iterations / 10);

    for iter in 0..opts.iterations {
        let key = random_bytes(rng, 16);
        let tweak = random_bytes(rng, 8);
        let length = rng.gen_range(opts.min_len..=opts.max_len);
        let plaintext = generate_plaintext(rng, info.charset, length);

        match (info.factory)(&key, &tweak) {
            Ok(cipher) => match cipher.encrypt(&plaintext, None) {
                Ok(ciphertext) => match cipher.decrypt(&ciphertext, None) {
                    Ok(decrypted) => {
                        if decrypted == plaintext {
                            passed += 1;
                        } else {
                            failed += 1;
                            print_failure(&key, &tweak, &plaintext, Some(&ciphertext), Some(&decrypted), Some("round-trip mismatch"));
                        }
                    }
                    Err(err) => {
                        failed += 1;
                        print_failure(&key, &tweak, &plaintext, Some(&ciphertext), None, Some(&format!("decryption error: {err}")));
                    }
                },
                Err(err) => {
                    failed += 1;
                    print_failure(&key, &tweak, &plaintext, None, None, Some(&format!("encryption error: {err}")));
                }
            },
            Err(err) => {
                failed += 1;
                print_failure(&key, &tweak, &plaintext, None, None, Some(&format!("cipher init error: {err}")));
            }
        }

        if (iter + 1) % interval == 0 || iter + 1 == opts.iterations {
            let percent = ((iter + 1) * 100) / opts.iterations;
            println!("  Progress: {}/{} ({}%)", iter + 1, opts.iterations, percent);
        }
    }

    println!("  Passed: {}/{}", passed, opts.iterations);
    println!("  Failed: {}/{}\n", failed, opts.iterations);
    (opts.iterations, failed)
}

fn random_bytes(rng: &mut StdRng, length: usize) -> Vec<u8> {
    (0..length).map(|_| rng.gen()).collect()
}

fn generate_plaintext(rng: &mut StdRng, alphabet: &str, length: usize) -> String {
    let chars: Vec<char> = alphabet.chars().collect();
    (0..length)
        .map(|_| chars[rng.gen_range(0..chars.len())])
        .collect()
}

fn print_failure(key: &[u8], tweak: &[u8], plain: &str, cipher: Option<&str>, decrypted: Option<&str>, detail: Option<&str>) {
    println!("  Round-trip failed:");
    println!("    Key: {}", hex(key));
    println!("    Tweak: {}", hex(tweak));
    println!("    Plaintext: \"{}\"", plain);
    if let Some(ct) = cipher {
        println!("    Ciphertext: \"{}\"", ct);
    }
    if let Some(pt) = decrypted {
        println!("    Decrypted: \"{}\"", pt);
    }
    if let Some(msg) = detail {
        println!("    Detail: {}", msg);
    }
}

fn hex(bytes: &[u8]) -> String {
    bytes.iter().map(|b| format!("{:02x}", b)).collect()
}

fn parse_args<I>(args: I) -> Result<Options, String>
where
    I: IntoIterator<Item = String>,
{
    let mut opts = Options::default();
    let mut iter = args.into_iter();
    while let Some(arg) = iter.next() {
        match arg.as_str() {
            "--help" | "-h" => {
                opts.help = true;
                return Ok(opts);
            }
            "--alphabets" => {
                let value = iter.next().ok_or("--alphabets requires a value")?;
                let list: Vec<String> = value
                    .split(',')
                    .map(|s| s.trim().to_lowercase())
                    .filter(|s| !s.is_empty())
                    .collect();
                if list.is_empty() {
                    return Err(String::from("--alphabets list cannot be empty"));
                }
                opts.alphabets = list;
            }
            "--min-length" => {
                let value = iter.next().ok_or("--min-length requires a value")?;
                opts.min_len = value.parse().map_err(|_| "invalid value for --min-length")?;
            }
            "--max-length" => {
                let value = iter.next().ok_or("--max-length requires a value")?;
                opts.max_len = value.parse().map_err(|_| "invalid value for --max-length")?;
            }
            "--quick" => opts.quick = true,
            "--seed" => {
                let value = iter.next().ok_or("--seed requires a value")?;
                let seed: u64 = value.parse().map_err(|_| "invalid value for --seed")?;
                opts.seed = Some(seed);
            }
            other if other.starts_with('-') => return Err(format!("Unknown option: {}", other)),
            value => {
                let iterations: usize = value.parse().map_err(|_| "invalid iterations value")?;
                if iterations == 0 {
                    return Err(String::from("iterations must be greater than 0"));
                }
                opts.iterations = iterations;
            }
        }
    }
    Ok(opts)
}

fn validate_options(opts: &Options) -> Result<(), String> {
    if opts.iterations == 0 {
        return Err(String::from("iterations must be greater than 0"));
    }
    if opts.min_len < 2 {
        return Err(String::from("--min-length must be at least 2"));
    }
    if opts.max_len < opts.min_len {
        return Err(String::from("--max-length must be greater than or equal to --min-length"));
    }
    for name in &opts.alphabets {
        alphabet_info(name)?;
    }
    Ok(())
}

fn alphabet_info(name: &str) -> Result<AlphabetInfo, String> {
    match name {
        "digits" => Ok(AlphabetInfo { name: "digits", charset: "0123456789", factory: digits }),
        "hex-lower" => Ok(AlphabetInfo { name: "hex-lower", charset: "0123456789abcdef", factory: hex_lower }),
        "hex-upper" => Ok(AlphabetInfo { name: "hex-upper", charset: "0123456789ABCDEF", factory: hex_upper }),
        "base36-lower" => Ok(AlphabetInfo { name: "base36-lower", charset: "0123456789abcdefghijklmnopqrstuvwxyz", factory: base36_lower }),
        "base36-upper" => Ok(AlphabetInfo { name: "base36-upper", charset: "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ", factory: base36_upper }),
        "base62" => Ok(AlphabetInfo { name: "base62", charset: "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz", factory: base62 }),
        other => Err(format!("Unknown alphabet: {}", other)),
    }
}

fn print_usage() {
    println!("FF3 Stress Test Tool\n");
    println!("Usage: ff3-stresstest [OPTIONS] [ITERATIONS]\n");
    println!("Options:");
    println!("  --alphabets LIST      Comma-separated list (default: digits,hex-lower,base36-lower,base62)");
    println!("  --min-length N        Minimum plaintext length (default: 6)");
    println!("  --max-length N        Maximum plaintext length (default: 20)");
    println!("  --quick               Run 100 iterations (fast test)");
    println!("  --seed N              Random seed for reproducibility");
}
