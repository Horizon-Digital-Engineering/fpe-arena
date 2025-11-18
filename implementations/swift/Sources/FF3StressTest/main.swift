import Foundation
import FF3

struct Options {
    var iterations: Int = 1000
    var alphabets: [String] = ["digits", "hex-lower", "base36-lower", "base62"]
    var minLength: Int = 6
    var maxLength: Int = 20
    var quick: Bool = false
    var seed: UInt64? = nil
    var help: Bool = false
}

struct StressTestError: LocalizedError, CustomStringConvertible {
    let message: String

    init(_ message: String) {
        self.message = message
    }

    var errorDescription: String? { message }
    var description: String { message }
}

let alphabetMap: [String: String] = [
    "digits": "0123456789",
    "hex-lower": "0123456789abcdef",
    "hex-upper": "0123456789ABCDEF",
    "base36-lower": "0123456789abcdefghijklmnopqrstuvwxyz",
    "base36-upper": "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ",
    "base62": "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
]

func printUsage() {
    print("FF3 Stress Test Tool\n")
    print("Usage: ff3-stresstest [OPTIONS] [ITERATIONS]\n")
    print("Options:")
    print("  --alphabets LIST      Comma-separated list (default: digits,hex-lower,base36-lower,base62)")
    print("  --min-length N        Minimum plaintext length (default: 6)")
    print("  --max-length N        Maximum plaintext length (default: 20)")
    print("  --quick               Run 100 iterations (fast test)")
    print("  --seed N              Random seed for reproducibility")
}

func parseArgs(_ args: [String]) throws -> Options {
    var options = Options()
    var index = 0
    while index < args.count {
        switch args[index] {
        case "--help", "-h":
            options.help = true
            return options
        case "--alphabets":
            guard index + 1 < args.count else { throw StressTestError("--alphabets requires a value") }
            options.alphabets = args[index + 1]
                .split(separator: ",")
                .map { $0.trimmingCharacters(in: .whitespaces).lowercased() }
                .filter { !$0.isEmpty }
            if options.alphabets.isEmpty { throw StressTestError("--alphabets list cannot be empty") }
            index += 1
        case "--min-length":
            guard index + 1 < args.count, let value = Int(args[index + 1]) else { throw StressTestError("invalid value for --min-length") }
            options.minLength = value
            index += 1
        case "--max-length":
            guard index + 1 < args.count, let value = Int(args[index + 1]) else { throw StressTestError("invalid value for --max-length") }
            options.maxLength = value
            index += 1
        case "--quick":
            options.quick = true
        case "--seed":
            guard index + 1 < args.count, let value = UInt64(args[index + 1]) else { throw StressTestError("invalid value for --seed") }
            options.seed = value
            index += 1
        default:
            if args[index].hasPrefix("--") {
                throw StressTestError("Unknown option: \(args[index])")
            }
            guard let value = Int(args[index]), value > 0 else { throw StressTestError("invalid iterations value: \(args[index])") }
            options.iterations = value
        }
        index += 1
    }
    return options
}

func validate(_ opts: Options) throws {
    if opts.iterations <= 0 { throw StressTestError("iterations must be greater than 0") }
    if opts.minLength < 2 { throw StressTestError("--min-length must be at least 2") }
    if opts.maxLength < opts.minLength { throw StressTestError("--max-length must be greater than or equal to --min-length") }
    for name in opts.alphabets {
        if alphabetMap[name] == nil { throw StressTestError("Unknown alphabet: \(name)") }
    }
}

struct SeededGenerator: RandomNumberGenerator {
    private var state: UInt64

    init(seed: UInt64) {
        self.state = seed != 0 ? seed : 0xdeadbeef
    }

    mutating func next() -> UInt64 {
        state ^= state << 13
        state ^= state >> 7
        state ^= state << 17
        return state
    }
}

func randomBytes(generator: inout some RandomNumberGenerator, count: Int) -> Data {
    var data = Data(count: count)
    for i in 0..<count {
        data[i] = UInt8(truncatingIfNeeded: generator.next())
    }
    return data
}

func generatePlaintext(generator: inout some RandomNumberGenerator, charset: String, length: Int) -> String {
    let chars = Array(charset)
    var result = String()
    result.reserveCapacity(length)
    for _ in 0..<length {
        let index = Int(generator.next() % UInt64(chars.count))
        result.append(chars[index])
    }
    return result
}

func hex(_ data: Data) -> String {
    data.map { String(format: "%02x", $0) }.joined()
}

func stressAlphabet(name: String, charset: String, opts: Options, generator: inout some RandomNumberGenerator) -> (tests: Int, failures: Int) {
    print("Testing \(name)...")
    print("  Alphabet: \(charset) (radix \(charset.count))")

    var failures = 0
    let interval = max(1, opts.iterations / 10)

    for i in 0..<opts.iterations {
        let key = randomBytes(generator: &generator, count: 16)
        let tweak = randomBytes(generator: &generator, count: 8)
        let length = Int(generator.next() % UInt64(opts.maxLength - opts.minLength + 1)) + opts.minLength
        let plaintext = generatePlaintext(generator: &generator, charset: charset, length: length)

        do {
            let cipher = try createCipher(key: key, tweak: tweak, name: name)
            let ciphertext = try cipher.encrypt(plaintext)
            let decrypted = try cipher.decrypt(ciphertext)
            if decrypted != plaintext {
                failures += 1
                printFailure(key: key, tweak: tweak, plaintext: plaintext, ciphertext: ciphertext, decrypted: decrypted, detail: "round-trip mismatch")
            }
        } catch {
            failures += 1
            printFailure(key: key, tweak: tweak, plaintext: plaintext, ciphertext: nil, decrypted: nil, detail: error.localizedDescription)
        }

        if (i + 1) % interval == 0 || i + 1 == opts.iterations {
            let percent = ((i + 1) * 100) / opts.iterations
            print("  Progress: \(i + 1)/\(opts.iterations) (\(percent)%)")
        }
    }

    print("  Passed: \(opts.iterations - failures)/\(opts.iterations)")
    print("  Failed: \(failures)/\(opts.iterations)\n")
    return (opts.iterations, failures)
}

func createCipher(key: Data, tweak: Data, name: String) throws -> FF3 {
    switch name {
    case "digits": return try digits(key: key, tweak: tweak)
    case "hex-lower": return try hexLower(key: key, tweak: tweak)
    case "hex-upper": return try hexUpper(key: key, tweak: tweak)
    case "base36-lower": return try base36Lower(key: key, tweak: tweak)
    case "base36-upper": return try base36Upper(key: key, tweak: tweak)
    case "base62": return try base62(key: key, tweak: tweak)
    default:
        throw StressTestError("Unknown alphabet: \(name)")
    }
}

func printFailure(key: Data, tweak: Data, plaintext: String, ciphertext: String?, decrypted: String?, detail: String) {
    print("  Round-trip failed:")
    print("    Key: \(hex(key))")
    print("    Tweak: \(hex(tweak))")
    print("    Plaintext: \"\(plaintext)\"")
    if let ct = ciphertext {
        print("    Ciphertext: \"\(ct)\"")
    }
    if let dec = decrypted {
        print("    Decrypted: \"\(dec)\"")
    }
    print("    Detail: \(detail)")
}

func run() throws {
    let args = Array(ProcessInfo.processInfo.arguments.dropFirst())
    var opts = try parseArgs(args)

    if opts.help {
        printUsage()
        return
    }

    if opts.quick {
        opts.iterations = 100
    }

    try validate(opts)

    var generator: any RandomNumberGenerator = opts.seed.map(SeededGenerator.init) ?? SystemRandomNumberGenerator()

    print("FF3 Stress Test v1.0")
    print("====================\n")
    print("Warning: FF3 was withdrawn by NIST; run for education and research only.\n")
    print("Test configuration")
    print("  Iterations per alphabet: \(opts.iterations)")
    print("  Random key/tweak generation: enabled")
    print("  String length range: \(opts.minLength)-\(opts.maxLength) characters")
    print("  Alphabets: \(opts.alphabets.joined(separator: ", "))\n")

    let start = Date()
    var totalTests = 0
    var totalFailures = 0

    for name in opts.alphabets {
        guard let charset = alphabetMap[name] else { throw StressTestError("Unknown alphabet: \(name)") }
        let (tests, failures) = stressAlphabet(name: name, charset: charset, opts: opts, generator: &generator)
        totalTests += tests
        totalFailures += failures
    }

    let durationMs = Int(Date().timeIntervalSince(start) * 1000)
    print("Summary")
    print("  Total tests: \(totalTests)")
    print("  Failures: \(totalFailures)")
    print("  Duration: \(durationMs) ms")
    if durationMs > 0 {
        let throughput = Double(totalTests) * 1000.0 / Double(durationMs)
        print(String(format: "  Throughput: %.2f tests/sec", throughput))
    }
    if totalFailures == 0 {
        print("  Result: all stress tests passed")
    } else {
        print("  Result: failures detected")
        throw StressTestError("failures detected")
    }
}

do {
    try run()
} catch {
    let message = (error as? StressTestError)?.message ?? error.localizedDescription
    fputs("Error: \(message)\n", stderr)
    exit(1)
}
