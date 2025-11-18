/**
 * FF3 Performance Benchmark Tool with comprehensive CLI support
 * Measures FF3 encryption/decryption performance across different string lengths.
 *
 * ⚠️ FF3 was withdrawn by NIST due to security vulnerabilities.
 * This benchmark is for educational and research purposes only.
 */

import Foundation
import FF3

// MARK: - Configuration

struct BenchmarkConfig: Codable {
    var alphabet: String = "digits"
    var radix: Int = 10
    var lengths: [Int] = [9, 12, 16]
    var cases: [String] = ["enc", "dec", "roundtrip"]
    var iterations: Int = 100000
    var warmup: Int = 10000
    var key: String = "EF4359D8D580AA4F7F036D6F04FC6A94"
    var tweak: String = "D8E7920AFA330A73"
    var seed: Int = 42
}

struct BenchmarkResult: Codable {
    let name: String
    let test_case: String
    let parameters: BenchmarkParameters
    let iterations: Int
    let elapsed_ns: Int
    let ns_per_op: Double
    let ops_per_sec: Double
    let checksum: String
}

struct BenchmarkParameters: Codable {
    let alphabet: String
    let radix: Int
    let length: Int
    let key_bits: Int
    let key_fingerprint: String
    let tweak: String
}

struct PlatformInfo: Codable {
    let os: String
    let arch: String
    let cpu: String
    let cores: Int
}

struct Metadata: Codable {
    let version: String
    let timestamp: String
    let language: String
    let runtime: String
    let platform: PlatformInfo
}

struct Configuration: Codable {
    let seed: Int
    let warmup_iterations: Int
}

struct Summary: Codable {
    let total_tests: Int
    let total_duration_sec: Double
    let checksum: String
}

struct BenchmarkOutput: Codable {
    let metadata: Metadata
    let configuration: Configuration
    let benchmarks: [BenchmarkResult]
    let summary: Summary
}

// Helper to encode heterogeneous dictionaries
struct AnyCodable: Codable {
    let value: Any

    init(_ value: Any) {
        self.value = value
    }

    func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        if let intValue = value as? Int {
            try container.encode(intValue)
        } else if let stringValue = value as? String {
            try container.encode(stringValue)
        } else if let arrayValue = value as? [Int] {
            try container.encode(arrayValue)
        } else if let arrayValue = value as? [String] {
            try container.encode(arrayValue)
        }
    }

    init(from decoder: Decoder) throws {
        let container = try decoder.singleValueContainer()
        if let intValue = try? container.decode(Int.self) {
            value = intValue
        } else if let stringValue = try? container.decode(String.self) {
            value = stringValue
        } else {
            value = ""
        }
    }
}

// MARK: - Extensions

extension Data {
    init?(hex: String) {
        let len = hex.count / 2
        var data = Data(capacity: len)
        var i = hex.startIndex

        for _ in 0..<len {
            let j = hex.index(i, offsetBy: 2)
            let bytes = hex[i..<j]
            if let num = UInt8(bytes, radix: 16) {
                data.append(num)
            } else {
                return nil
            }
            i = j
        }
        self = data
    }
}

// MARK: - Helper Functions

func getAlphabetChars(_ alphabet: String) -> String {
    switch alphabet {
    case "digits": return "0123456789"
    case "hex": return "0123456789abcdef"
    case "base36": return "0123456789abcdefghijklmnopqrstuvwxyz"
    case "base62": return "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
    default: return "0123456789"
    }
}

func buildCipher(alphabet: String, key: Data, tweak: Data) throws -> FF3 {
    switch alphabet {
    case "digits": return try digits(key: key, tweak: tweak)
    case "hex": return try hexLower(key: key, tweak: tweak)
    case "base36": return try base36Lower(key: key, tweak: tweak)
    case "base62": return try base62(key: key, tweak: tweak)
    default: return try digits(key: key, tweak: tweak)
    }
}

func generateInputs(alphabetChars: String, length: Int, count: Int, seed: Int) -> [String] {
    var generator = SeededRandomNumberGenerator(seed: UInt64(seed))
    var inputs: [String] = []
    for _ in 0..<count {
        var input = ""
        for _ in 0..<length {
            let idx = Int.random(in: 0..<alphabetChars.count, using: &generator)
            let char = alphabetChars[alphabetChars.index(alphabetChars.startIndex, offsetBy: idx)]
            input.append(char)
        }
        inputs.append(input)
    }
    return inputs
}

struct SeededRandomNumberGenerator: RandomNumberGenerator {
    private var state: UInt64

    init(seed: UInt64) {
        state = seed
    }

    mutating func next() -> UInt64 {
        // Linear Congruential Generator
        state = state &* 1103515245 &+ 12345
        return state
    }
}

func runBenchmark(cipher: FF3, inputs: [String], caseType: String, iterations: Int, verbose: Bool) throws -> (elapsed_ns: Int, ns_per_op: Double, ops_per_sec: Double, checksum: UInt32) {
    var times: [Double] = []
    var checksum: UInt32 = 0

    // Precompute ciphertexts for decrypt benchmarks
    let ciphertexts = try inputs.map { try cipher.encrypt($0) }

    for i in 0..<iterations {
        let inputIdx = i % inputs.count
        let plaintext = inputs[inputIdx]
        let ciphertext = ciphertexts[inputIdx]

        let start = Date()

        let result: String
        switch caseType {
        case "enc":
            result = try cipher.encrypt(plaintext)
        case "dec":
            result = try cipher.decrypt(ciphertext)
        case "roundtrip":
            let ct = try cipher.encrypt(plaintext)
            result = try cipher.decrypt(ct)
            if result != plaintext {
                throw NSError(domain: "BenchmarkError", code: 1,
                            userInfo: [NSLocalizedDescriptionKey: "Round-trip verification failed"])
            }
        default:
            throw NSError(domain: "BenchmarkError", code: 2,
                        userInfo: [NSLocalizedDescriptionKey: "Unknown case type: \(caseType)"])
        }

        let end = Date()
        times.append(end.timeIntervalSince(start))

        // XOR checksum to prevent dead code elimination
        for char in result {
            checksum ^= UInt32(char.asciiValue ?? 0)
        }

        if verbose && (i + 1) % 10000 == 0 {
            fputs("  Progress: \(i + 1)/\(iterations)\n", stderr)
        }
    }

    let totalNs = times.reduce(0, +) * 1_000_000_000
    let avgNs = totalNs / Double(times.count)
    let opsPerSec = avgNs > 0 ? 1_000_000_000.0 / avgNs : 0

    return (elapsed_ns: Int(totalNs), ns_per_op: avgNs, ops_per_sec: opsPerSec, checksum: checksum)
}

// MARK: - Argument Parsing

func parseArgs() -> (config: BenchmarkConfig, verbose: Bool, jsonOut: String?) {
    var config = BenchmarkConfig()
    var verbose = false
    var jsonOut: String? = nil

    let args = CommandLine.arguments
    var i = 1
    while i < args.count {
        let arg = args[i]

        switch arg {
        case "-h", "--help":
            print("""
FF3 Performance Benchmark Tool v0.1.0

Usage: ff3-benchmark [options]

Options:
  --config FILE         Load configuration from JSON file
  --alphabet NAME       Alphabet to use (digits, hex, base36, base62)
  --radix N             Radix (2-62)
  --lengths CSV         Comma-separated input lengths (e.g., "9,12,16")
  --cases CSV           Comma-separated cases (enc,dec,roundtrip)
  --iterations N        Number of iterations (default: 100000)
  --warmup N            Warmup iterations (default: 10000)
  --key HEX             Encryption key (hex string)
  --tweak HEX           Tweak value (hex string)
  --seed N              Random seed for input generation
  --verbose             Show progress during benchmarks
  --json-out FILE       Write JSON results to file
  --quick               Quick benchmark (fewer iterations)
  -h, --help            Show this help message
  -v, --version         Show version

⚠️  FF3 was withdrawn by NIST due to security vulnerabilities.
   This tool is for educational and research purposes only.
""")
            exit(0)

        case "-v", "--version":
            print("FF3 Benchmark Tool v0.1.0")
            exit(0)

        case "--config":
            i += 1
            guard i < args.count else {
                fputs("ERR: --config requires a file path\n", stderr)
                exit(2)
            }
            let configPath = args[i]
            if let data = try? Data(contentsOf: URL(fileURLWithPath: configPath)),
               let decoded = try? JSONDecoder().decode(BenchmarkConfig.self, from: data) {
                config = decoded
            } else {
                fputs("ERR: Failed to load config file\n", stderr)
                exit(2)
            }

        case "--alphabet":
            i += 1
            guard i < args.count else {
                fputs("ERR: --alphabet requires a value\n", stderr)
                exit(2)
            }
            config.alphabet = args[i]

        case "--radix":
            i += 1
            guard i < args.count, let value = Int(args[i]) else {
                fputs("ERR: --radix requires an integer\n", stderr)
                exit(2)
            }
            config.radix = value

        case "--lengths":
            i += 1
            guard i < args.count else {
                fputs("ERR: --lengths requires a value\n", stderr)
                exit(2)
            }
            config.lengths = args[i].split(separator: ",").compactMap { Int($0.trimmingCharacters(in: .whitespaces)) }

        case "--cases":
            i += 1
            guard i < args.count else {
                fputs("ERR: --cases requires a value\n", stderr)
                exit(2)
            }
            config.cases = args[i].split(separator: ",").map { String($0.trimmingCharacters(in: .whitespaces)) }

        case "--iterations":
            i += 1
            guard i < args.count, let value = Int(args[i]) else {
                fputs("ERR: --iterations requires an integer\n", stderr)
                exit(2)
            }
            config.iterations = value

        case "--warmup":
            i += 1
            guard i < args.count, let value = Int(args[i]) else {
                fputs("ERR: --warmup requires an integer\n", stderr)
                exit(2)
            }
            config.warmup = value

        case "--key":
            i += 1
            guard i < args.count else {
                fputs("ERR: --key requires a value\n", stderr)
                exit(2)
            }
            config.key = args[i]

        case "--tweak":
            i += 1
            guard i < args.count else {
                fputs("ERR: --tweak requires a value\n", stderr)
                exit(2)
            }
            config.tweak = args[i]

        case "--seed":
            i += 1
            guard i < args.count, let value = Int(args[i]) else {
                fputs("ERR: --seed requires an integer\n", stderr)
                exit(2)
            }
            config.seed = value

        case "--verbose":
            verbose = true

        case "--json-out":
            i += 1
            guard i < args.count else {
                fputs("ERR: --json-out requires a file path\n", stderr)
                exit(2)
            }
            jsonOut = args[i]

        case "--quick":
            config.iterations = 10000
            config.warmup = 1000
            config.lengths = [9, 12]
            config.cases = ["enc", "roundtrip"]

        default:
            fputs("ERR: Unknown option: \(arg)\n", stderr)
            exit(1)
        }

        i += 1
    }

    return (config, verbose, jsonOut)
}

// MARK: - Main

do {
    let (config, verbose, jsonOut) = parseArgs()

    if verbose {
        fputs("""

🚀 FF3 Performance Benchmark Tool v0.1.0
============================================

⚠️  FF3 was WITHDRAWN by NIST due to security vulnerabilities.
    This benchmark is for EDUCATIONAL and RESEARCH purposes only.

🔧 Configuration:
   Alphabet: \(config.alphabet)
   Radix: \(config.radix)
   Key: \(config.key.prefix(16))...
   Tweak: \(config.tweak)
   Lengths: \(config.lengths)
   Cases: \(config.cases)
   Iterations: \(config.iterations)
   Warmup: \(config.warmup)
   Seed: \(config.seed)

Starting performance benchmarks...

""", stderr)
    }

    guard let key = Data(hex: config.key),
          let tweak = Data(hex: config.tweak) else {
        fputs("ERR: Failed to decode key or tweak\n", stderr)
        exit(2)
    }

    let cipher = try buildCipher(alphabet: config.alphabet, key: key, tweak: tweak)
    let alphabetChars = getAlphabetChars(config.alphabet)
    let radix = alphabetChars.count

    var benchmarks: [BenchmarkResult] = []
    var totalDuration: Double = 0
    var combinedChecksum: UInt32 = 0

    for length in config.lengths {
        // Generate ring buffer of 64 varied inputs
        let inputs = generateInputs(alphabetChars: alphabetChars, length: length, count: 64, seed: config.seed)

        // Warmup phase
        if config.warmup > 0 {
            if verbose {
                fputs("\n📊 Warmup for length \(length)...\n", stderr)
            }
            _ = try runBenchmark(cipher: cipher, inputs: inputs, caseType: "enc", iterations: config.warmup, verbose: false)
        }

        // Benchmark each case
        for caseType in config.cases {
            if verbose {
                fputs("\n📊 Benchmarking \(caseType)_len\(length)_radix\(radix)...\n", stderr)
            }

            let result = try runBenchmark(cipher: cipher, inputs: inputs, caseType: caseType, iterations: config.iterations, verbose: verbose)

            let benchResult = BenchmarkResult(
                name: "\(caseType)_len\(length)_radix\(radix)",
                test_case: caseType,
                parameters: BenchmarkParameters(
                    alphabet: config.alphabet,
                    radix: radix,
                    length: length,
                    key_bits: key.count * 8,
                    key_fingerprint: String(config.key.prefix(8)).uppercased(),
                    tweak: config.tweak.uppercased()
                ),
                iterations: config.iterations,
                elapsed_ns: result.elapsed_ns,
                ns_per_op: result.ns_per_op,
                ops_per_sec: result.ops_per_sec,
                checksum: String(format: "%08x", result.checksum)
            )
            benchmarks.append(benchResult)
            totalDuration += Double(result.elapsed_ns) / 1_000_000_000.0
            combinedChecksum ^= result.checksum

            if verbose {
                fputs("   \(Int(result.ns_per_op)) ns/op (\(Int(result.ops_per_sec)) ops/sec)\n", stderr)
            }
        }
    }

    // Output JSON results
    let dateFormatter = ISO8601DateFormatter()

    // Get platform info
    #if os(macOS)
    let osName = "darwin"
    #elseif os(Linux)
    let osName = "linux"
    #else
    let osName = "unknown"
    #endif

    #if arch(x86_64)
    let archName = "x86_64"
    #elseif arch(arm64)
    let archName = "arm64"
    #else
    let archName = "unknown"
    #endif

    let output = BenchmarkOutput(
        metadata: Metadata(
            version: "1.0",
            timestamp: dateFormatter.string(from: Date()),
            language: "swift",
            runtime: "Swift 5.9+",
            platform: PlatformInfo(
                os: osName,
                arch: archName,
                cpu: "unknown",
                cores: ProcessInfo.processInfo.processorCount
            )
        ),
        configuration: Configuration(
            seed: config.seed,
            warmup_iterations: config.warmup
        ),
        benchmarks: benchmarks,
        summary: Summary(
            total_tests: benchmarks.count,
            total_duration_sec: totalDuration,
            checksum: String(format: "%08x", combinedChecksum)
        )
    )

    let encoder = JSONEncoder()
    encoder.outputFormatting = .prettyPrinted
    let jsonData = try encoder.encode(output)
    let jsonString = String(data: jsonData, encoding: .utf8)!

    if let jsonOut = jsonOut {
        try jsonString.write(toFile: jsonOut, atomically: true, encoding: .utf8)
        if verbose {
            fputs("\n✅ Results written to \(jsonOut)\n", stderr)
        }
    } else {
        print(jsonString)
    }

    if verbose {
        fputs("""

⚠️  FF3 Format Preserving Encryption Library v0.1.0

    FF3 was WITHDRAWN by NIST due to security vulnerabilities.
    This implementation is for EDUCATIONAL and RESEARCH purposes only.

    DO NOT use in production systems.

""", stderr)
    }

} catch {
    fputs("❌ Error: \(error)\n", stderr)
    exit(1)
}