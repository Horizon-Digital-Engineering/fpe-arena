/**
 * FF3 NIST Test Vector Validation Tool
 */

import Foundation
import FF3

struct Options {
    var vectorsPath: String?
    var verbose = false
    var quiet = false
    var help = false
}

struct NistTestVector: Codable {
    let sample: Int
    let algorithm: String
    let key: String
    let radix: Int
    let alphabet: String
    let plaintext: String
    let tweak: String
    let ciphertext: String
}

struct NistTestVectors: Codable {
    let description: String
    let source: String
    let vectors: [NistTestVector]
}

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

func showUsage() {
    print("FF3 NIST Test Vector Validation Tool\n")
    print("Usage: ff3-validate [OPTIONS]\n")
    print("Options:")
    print("  --vectors PATH    Path to test vectors JSON file")
    print("  --verbose         Show detailed test output")
    print("  --quiet           Only show failures and summary")
    print("  -h, --help        Show this help message\n")
}

func parseArgs(_ args: [String]) throws -> Options {
    var opts = Options()
    var i = 0

    while i < args.count {
        switch args[i] {
        case "-h", "--help":
            opts.help = true
            return opts
        case "--vectors":
            guard i + 1 < args.count else {
                throw FF3Cipher.FF3Error.invalidInputLength("Missing value for --vectors")
            }
            i += 1
            opts.vectorsPath = args[i]
        case "--verbose":
            opts.verbose = true
        case "--quiet":
            opts.quiet = true
        default:
            throw FF3Cipher.FF3Error.invalidInputLength("Unknown option: \(args[i])")
        }
        i += 1
    }

    return opts
}

func findVectorsFile(customPath: String?) -> String? {
    if let path = customPath {
        return FileManager.default.fileExists(atPath: path) ? path : nil
    }

    let paths = [
        ProcessInfo.processInfo.environment["FF3_TEST_VECTORS_PATH"],
        "../../shared/test-vectors/nist_ff3_official_vectors.json",
        "../../../shared/test-vectors/nist_ff3_official_vectors.json",
        "./nist_ff3_official_vectors.json"
    ]

    for path in paths {
        if let p = path, FileManager.default.fileExists(atPath: p) {
            return p
        }
    }

    return nil
}

func createCipher(key: Data, tweak: Data, alphabet: String) throws -> FF3 {
    let spec = AlphabetSpec(charset: alphabet)
    return try fromSpec(key: key, tweak: tweak, spec: spec)
}

func main() {
    do {
        let args = Array(CommandLine.arguments.dropFirst())
        let opts = try parseArgs(args)

        if opts.help {
            showUsage()
            exit(0)
        }

        if !opts.quiet {
            print("FF3 NIST Test Vector Validation Tool")
            print("========================================\n")
        }

        guard let vectorsPath = findVectorsFile(customPath: opts.vectorsPath) else {
            if let custom = opts.vectorsPath {
                fputs("Error: Vectors file not found: \(custom)\n", stderr)
            } else {
                fputs("Error: Could not find NIST test vectors file\n", stderr)
                fputs("Try: ff3-validate --vectors /path/to/vectors.json\n", stderr)
            }
            exit(2)
        }

        if !opts.quiet {
            print("Vector file: \(vectorsPath)\n")
        }

        let data = try Data(contentsOf: URL(fileURLWithPath: vectorsPath))
        let testVectors = try JSONDecoder().decode(NistTestVectors.self, from: data)

        if !opts.quiet {
            print("Testing \(testVectors.vectors.count) NIST FF3 vectors...\n")
        }

        var passed = 0
        var failed = 0

        for vector in testVectors.vectors {
            guard let key = Data(hex: vector.key),
                  let tweak = Data(hex: vector.tweak) else {
                if !opts.quiet {
                    print("Sample \(vector.sample): ERROR - Invalid hex format")
                }
                failed += 1
                continue
            }

            do {
                let cipher = try createCipher(key: key, tweak: tweak, alphabet: vector.alphabet)
                let encrypted = try cipher.encrypt(vector.plaintext)

                let encryptPassed = encrypted == vector.ciphertext
                var roundtripPassed = true

                if encryptPassed {
                    let decrypted = try cipher.decrypt(encrypted)
                    roundtripPassed = decrypted == vector.plaintext
                }

                let testPassed = encryptPassed && roundtripPassed

                if testPassed {
                    passed += 1
                    if opts.verbose {
                        print("Sample \(vector.sample): PASS")
                    }
                } else {
                    failed += 1
                    if !opts.quiet {
                        print("Sample \(vector.sample): FAIL")
                        if !encryptPassed {
                            print("  Expected: \(vector.ciphertext)")
                            print("  Got:      \(encrypted)")
                        }
                        if !roundtripPassed {
                            print("  Round-trip failed")
                        }
                    }
                }
            } catch {
                failed += 1
                if !opts.quiet {
                    print("Sample \(vector.sample): ERROR - \(error)")
                }
            }
        }

        if !opts.quiet {
            print("\n========================================")
            print("Results: \(passed)/\(testVectors.vectors.count) passed\n")
        }

        if failed == 0 {
            if !opts.quiet {
                print("ALL NIST TEST VECTORS PASSED!\n")
                print("WARNING: FF3 was WITHDRAWN by NIST due to security vulnerabilities.")
                print("This implementation is for EDUCATIONAL and RESEARCH purposes only.")
                print("DO NOT use in production systems.\n")
            }
            exit(0)
        } else {
            if !opts.quiet {
                print("VALIDATION FAILED\n")
            }
            exit(1)
        }

    } catch {
        fputs("Error: \(error)\n", stderr)
        exit(2)
    }
}

main()
