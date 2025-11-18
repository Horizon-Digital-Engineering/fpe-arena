// FF3 Performance Benchmark Tool - Spec Compliant
use fpe_ff3::{digits, hex_lower, base36_lower, base62};
use rand::{Rng, SeedableRng};
use rand::rngs::StdRng;
use serde::{Deserialize, Serialize};
use std::time::Instant;

#[derive(Serialize, Deserialize)]
#[serde(default)]
struct BenchmarkConfig {
    alphabet: String,
    radix: u32,
    lengths: Vec<usize>,
    cases: Vec<String>,
    iterations: usize,
    warmup: usize,
    key: String,
    tweak: String,
    seed: u64,
    #[serde(skip)]
    verbose: bool,
    #[serde(skip)]
    json_out: Option<String>,
}

impl Default for BenchmarkConfig {
    fn default() -> Self {
        Self {
            alphabet: "digits".to_string(),
            radix: 10,
            lengths: vec![9, 12, 16],
            cases: vec!["enc".to_string(), "dec".to_string(), "roundtrip".to_string()],
            iterations: 100000,
            warmup: 10000,
            key: "EF4359D8D580AA4F7F036D6F04FC6A94".to_string(),
            tweak: "D8E7920AFA330A73".to_string(),
            seed: 42,
            verbose: false,
            json_out: None,
        }
    }
}

#[derive(Serialize)]
struct BenchmarkParameters {
    alphabet: String,
    radix: u32,
    length: usize,
    key_bits: usize,
    key_fingerprint: String,
    tweak: String,
}

#[derive(Serialize)]
struct BenchmarkResult {
    name: String,
    test_case: String,
    parameters: BenchmarkParameters,
    iterations: usize,
    elapsed_ns: u128,
    ns_per_op: f64,
    ops_per_sec: f64,
    checksum: String,
}

#[derive(Serialize)]
struct PlatformInfo {
    os: String,
    arch: String,
    cpu: String,
    cores: usize,
}

#[derive(Serialize)]
struct MetadataInfo {
    version: String,
    timestamp: String,
    language: String,
    runtime: String,
    platform: PlatformInfo,
}

#[derive(Serialize)]
struct ConfigurationInfo {
    seed: u64,
    warmup_iterations: usize,
}

#[derive(Serialize)]
struct SummaryInfo {
    total_tests: usize,
    total_duration_sec: f64,
    checksum: String,
}

#[derive(Serialize)]
struct BenchmarkReport {
    metadata: MetadataInfo,
    configuration: ConfigurationInfo,
    benchmarks: Vec<BenchmarkResult>,
    summary: SummaryInfo,
}

fn hash_string(s: &str) -> u32 {
    let mut hash: u32 = 0;
    for byte in s.bytes() {
        hash = hash.wrapping_mul(31).wrapping_add(byte as u32);
    }
    hash
}

fn generate_inputs(count: usize, length: usize, alphabet: &str, seed: u64) -> Vec<String> {
    let mut rng = StdRng::seed_from_u64(seed);
    let mut inputs = Vec::with_capacity(count);

    let charset = match alphabet {
        "digits" => "0123456789",
        "hex" => "0123456789abcdef",
        "base36" => "0123456789abcdefghijklmnopqrstuvwxyz",
        "base62" => "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz",
        _ => "0123456789",
    };

    for _ in 0..count {
        let input: String = (0..length)
            .map(|_| {
                let idx = rng.gen_range(0..charset.len());
                charset.chars().nth(idx).unwrap()
            })
            .collect();
        inputs.push(input);
    }

    inputs
}

fn run_benchmark(
    cipher: &fpe_ff3::FF3,
    bench_case: &str,
    length: usize,
    radix: u32,
    alphabet: &str,
    iterations: usize,
    warmup: usize,
    key: &[u8],
    tweak_hex: &str,
    seed: u64,
) -> Result<BenchmarkResult, Box<dyn std::error::Error>> {
    const RING_SIZE: usize = 64;
    let inputs = generate_inputs(RING_SIZE, length, alphabet, seed);

    // Precompute ciphertexts for decrypt benchmarks
    let precomputed_cts: Vec<String> = if bench_case == "dec" {
        inputs.iter()
            .map(|pt| cipher.encrypt(pt, None).unwrap())
            .collect()
    } else {
        Vec::new()
    };

    // Warmup
    for i in 0..warmup {
        let input = &inputs[i % RING_SIZE];
        match bench_case {
            "enc" => {
                let _ = cipher.encrypt(input, None)?;
            }
            "dec" => {
                let ct = &precomputed_cts[i % RING_SIZE];
                let _ = cipher.decrypt(ct, None)?;
            }
            "roundtrip" => {
                let ct = cipher.encrypt(input, None)?;
                let _ = cipher.decrypt(&ct, None)?;
            }
            _ => {}
        }
    }

    // Measured phase
    let mut checksum: u32 = 0;
    let start = Instant::now();

    for i in 0..iterations {
        let input = &inputs[i % RING_SIZE];
        let output = match bench_case {
            "enc" => {
                let out = cipher.encrypt(input, None)?;
                checksum ^= hash_string(&out);
                out
            }
            "dec" => {
                let ct = &precomputed_cts[i % RING_SIZE];
                let out = cipher.decrypt(ct, None)?;
                checksum ^= hash_string(&out);
                out
            }
            "roundtrip" => {
                let ct = cipher.encrypt(input, None)?;
                let out = cipher.decrypt(&ct, None)?;
                checksum ^= hash_string(&out);
                out
            }
            _ => String::new(),
        };
        drop(output); // Prevent optimization
    }

    let elapsed = start.elapsed();
    let elapsed_ns = elapsed.as_nanos();
    let ns_per_op = elapsed_ns as f64 / iterations as f64;
    let ops_per_sec = 1_000_000_000.0 / ns_per_op;

    let key_fingerprint = hex::encode(&key[..std::cmp::min(4, key.len())]).to_uppercase();

    Ok(BenchmarkResult {
        name: format!("{}_len{}_radix{}", bench_case, length, radix),
        test_case: bench_case.to_string(),
        parameters: BenchmarkParameters {
            alphabet: alphabet.to_string(),
            radix,
            length,
            key_bits: key.len() * 8,
            key_fingerprint,
            tweak: tweak_hex.to_string(),
        },
        iterations,
        elapsed_ns,
        ns_per_op,
        ops_per_sec,
        checksum: format!("{:08x}", checksum),
    })
}

fn get_platform_info() -> PlatformInfo {
    PlatformInfo {
        os: std::env::consts::OS.to_string(),
        arch: std::env::consts::ARCH.to_string(),
        cpu: "unknown".to_string(),
        cores: num_cpus::get(),
    }
}

fn parse_csv_usize(s: &str) -> Vec<usize> {
    s.split(',')
        .filter_map(|p| p.trim().parse().ok())
        .collect()
}

fn parse_csv_string(s: &str) -> Vec<String> {
    s.split(',')
        .map(|p| p.trim().to_string())
        .filter(|p| !p.is_empty())
        .collect()
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut config = BenchmarkConfig::default();

    // Simple arg parsing
    let args: Vec<String> = std::env::args().collect();
    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "--help" | "-h" => {
                println!("FF3 Performance Benchmark Tool\n");
                println!("Usage: ff3-benchmark [OPTIONS]\n");
                println!("Options:");
                println!("  --config <file>        JSON config file");
                println!("  --alphabet <name>      Alphabet: digits|hex|base36|base62 (default: digits)");
                println!("  --radix <n>            Radix 2-62 (default: 10)");
                println!("  --lengths <csv>        Comma-separated lengths (default: 9,12,16)");
                println!("  --cases <csv>          Test cases: enc,dec,roundtrip (default: all)");
                println!("  --iterations <n>       Measured iterations (default: 100000)");
                println!("  --warmup <n>           Warmup iterations (default: 10000)");
                println!("  --key <hex>            Hex-encoded key");
                println!("  --tweak <hex>          Hex-encoded tweak");
                println!("  --seed <n>             Random seed (default: 42)");
                println!("  --quick                Reduce iterations 10x");
                println!("  --json-out <file>      Write JSON results to file");
                println!("  --verbose              Show progress messages");
                println!("  -h, --help             Show this help");
                return Ok(());
            }
            "--config" => {
                if i + 1 < args.len() {
                    let file_content = std::fs::read_to_string(&args[i + 1])?;
                    config = serde_json::from_str(&file_content)?;
                    i += 1;
                }
            }
            "--alphabet" => {
                if i + 1 < args.len() {
                    config.alphabet = args[i + 1].clone();
                    i += 1;
                }
            }
            "--radix" => {
                if i + 1 < args.len() {
                    config.radix = args[i + 1].parse()?;
                    i += 1;
                }
            }
            "--lengths" => {
                if i + 1 < args.len() {
                    config.lengths = parse_csv_usize(&args[i + 1]);
                    i += 1;
                }
            }
            "--cases" => {
                if i + 1 < args.len() {
                    config.cases = parse_csv_string(&args[i + 1]);
                    i += 1;
                }
            }
            "--iterations" => {
                if i + 1 < args.len() {
                    config.iterations = args[i + 1].parse()?;
                    i += 1;
                }
            }
            "--warmup" => {
                if i + 1 < args.len() {
                    config.warmup = args[i + 1].parse()?;
                    i += 1;
                }
            }
            "--key" => {
                if i + 1 < args.len() {
                    config.key = args[i + 1].clone();
                    i += 1;
                }
            }
            "--tweak" => {
                if i + 1 < args.len() {
                    config.tweak = args[i + 1].clone();
                    i += 1;
                }
            }
            "--seed" => {
                if i + 1 < args.len() {
                    config.seed = args[i + 1].parse()?;
                    i += 1;
                }
            }
            "--quick" => {
                config.iterations /= 10;
                config.warmup /= 10;
            }
            "--json-out" => {
                if i + 1 < args.len() {
                    config.json_out = Some(args[i + 1].clone());
                    i += 1;
                }
            }
            "--verbose" => {
                config.verbose = true;
            }
            _ => {}
        }
        i += 1;
    }

    let key = hex::decode(&config.key)?;
    let tweak = hex::decode(&config.tweak)?;

    // Build cipher based on alphabet
    let cipher = match config.alphabet.as_str() {
        "digits" => digits(&key, &tweak)?,
        "hex" => hex_lower(&key, &tweak)?,
        "base36" => base36_lower(&key, &tweak)?,
        "base62" => base62(&key, &tweak)?,
        _ => digits(&key, &tweak)?,
    };

    let total_start = Instant::now();
    let mut benchmarks = Vec::new();
    let mut overall_checksum: u32 = 0;

    for length in &config.lengths {
        for bench_case in &config.cases {
            if config.verbose {
                eprintln!("Running {}_len{}_radix{}...", bench_case, length, config.radix);
            }

            let result = run_benchmark(
                &cipher,
                bench_case,
                *length,
                config.radix,
                &config.alphabet,
                config.iterations,
                config.warmup,
                &key,
                &config.tweak,
                config.seed,
            )?;

            overall_checksum ^= u32::from_str_radix(&result.checksum, 16)?;
            benchmarks.push(result);
        }
    }

    let total_duration = total_start.elapsed();

    let report = BenchmarkReport {
        metadata: MetadataInfo {
            version: "1.0".to_string(),
            timestamp: chrono::Utc::now().to_rfc3339(),
            language: "rust".to_string(),
            runtime: "rustc".to_string(),
            platform: get_platform_info(),
        },
        configuration: ConfigurationInfo {
            seed: config.seed,
            warmup_iterations: config.warmup,
        },
        benchmarks,
        summary: SummaryInfo {
            total_tests: config.lengths.len() * config.cases.len(),
            total_duration_sec: total_duration.as_secs_f64(),
            checksum: format!("{:08x}", overall_checksum),
        },
    };

    let json = serde_json::to_string_pretty(&report)?;

    if let Some(path) = config.json_out {
        std::fs::write(path, json)?;
    } else {
        println!("{}", json);
    }

    Ok(())
}
