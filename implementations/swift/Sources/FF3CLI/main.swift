/**
 * FF3 CLI - Command-line tool for FF3 encryption/decryption
 */

import Foundation
import FF3

struct Options {
    var help: Bool = false
    var encryptText: String? = nil
    var decryptText: String? = nil
    var keyHex: String? = nil
    var tweakHex: String? = nil
    var alphabetType: String = "digits"
    var customCharset: String? = nil
}

func parseArgs(_ args: [String]) throws -> Options {
    var opts = Options()
    var i = 0

    while i < args.count {
        let arg = args[i]

        switch arg {
        case "-h", "--help":
            opts.help = true
            return opts

        case "-e", "--encrypt":
            guard i + 1 < args.count else {
                throw FF3Cipher.FF3Error.invalidInputLength("Missing value for \(arg)")
            }
            i += 1
            opts.encryptText = args[i]

        case "-d", "--decrypt":
            guard i + 1 < args.count else {
                throw FF3Cipher.FF3Error.invalidInputLength("Missing value for \(arg)")
            }
            i += 1
            opts.decryptText = args[i]

        case "-k", "--key":
            guard i + 1 < args.count else {
                throw FF3Cipher.FF3Error.invalidInputLength("Missing value for \(arg)")
            }
            i += 1
            opts.keyHex = args[i]

        case "-t", "--tweak":
            guard i + 1 < args.count else {
                throw FF3Cipher.FF3Error.invalidInputLength("Missing value for \(arg)")
            }
            i += 1
            opts.tweakHex = args[i]

        case "-a", "--alphabet":
            guard i + 1 < args.count else {
                throw FF3Cipher.FF3Error.invalidInputLength("Missing value for \(arg)")
            }
            i += 1
            opts.alphabetType = args[i]

        case "-c", "--custom":
            guard i + 1 < args.count else {
                throw FF3Cipher.FF3Error.invalidInputLength("Missing value for \(arg)")
            }
            i += 1
            opts.customCharset = args[i]

        default:
            throw FF3Cipher.FF3Error.invalidInputLength("Unknown option: \(arg)")
        }

        i += 1
    }

    return opts
}

func createCipher(key: Data, tweak: Data, alphabetType: String, customCharset: String?) throws -> FF3 {
    if let custom = customCharset {
        let spec = AlphabetSpec(charset: custom)
        return try fromSpec(key: key, tweak: tweak, spec: spec)
    }

    switch alphabetType.lowercased() {
    case "digits":
        return try digits(key: key, tweak: tweak)
    case "hex", "hex-lower":
        return try hexLower(key: key, tweak: tweak)
    case "hex-upper":
        return try hexUpper(key: key, tweak: tweak)
    case "base36", "base36-lower":
        return try base36Lower(key: key, tweak: tweak)
    case "base36-upper":
        return try base36Upper(key: key, tweak: tweak)
    case "base62":
        return try base62(key: key, tweak: tweak)
    default:
        throw FF3Cipher.FF3Error.invalidInputLength("Unknown alphabet type: \(alphabetType)")
    }
}

func showUsage() {
    print("FF3 CLI - Format Preserving Encryption")
    print()
    print("Usage: ff3-cli [OPTIONS]")
    print()
    print("Options:")
    print("  -e, --encrypt TEXT      Encrypt the given text")
    print("  -d, --decrypt TEXT      Decrypt the given text")
    print("  -k, --key HEX           AES key in hex format (32/48/64 hex chars)")
    print("  -t, --tweak HEX         Tweak in hex format (16 hex chars)")
    print("  -a, --alphabet TYPE     Alphabet type:")
    print("                            digits (default)")
    print("                            hex-lower")
    print("                            hex-upper")
    print("                            base36-lower")
    print("                            base36-upper")
    print("                            base62")
    print("  -c, --custom CHARSET    Custom alphabet charset")
    print("  -h, --help              Show this help message")
    print()
    print("Examples:")
    print("  ff3-cli -e \"1234567890\" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73")
    print("  ff3-cli -d \"7501889140\" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73")
    print()
}

func main() {
    let args = Array(CommandLine.arguments.dropFirst())

    do {
        let opts = try parseArgs(args)

        if opts.help {
            showUsage()
            exit(0)
        }

        // Validate required arguments
        guard opts.encryptText != nil || opts.decryptText != nil else {
            fputs("Error: Either --encrypt or --decrypt must be specified\n", stderr)
            showUsage()
            exit(1)
        }

        guard let keyHex = opts.keyHex, let tweakHex = opts.tweakHex else {
            fputs("Error: Key (-k) and tweak (-t) are required\n", stderr)
            showUsage()
            exit(1)
        }

        // Parse key and tweak
        guard let key = Data(hex: keyHex) else {
            fputs("Error: Invalid key hex format\n", stderr)
            exit(1)
        }

        guard let tweak = Data(hex: tweakHex) else {
            fputs("Error: Invalid tweak hex format\n", stderr)
            exit(1)
        }

        // Validate key length
        guard [16, 24, 32].contains(key.count) else {
            fputs("Error: Key must be 16, 24, or 32 bytes (32, 48, or 64 hex chars), got \(key.count) bytes\n", stderr)
            exit(1)
        }

        // Validate tweak length
        guard tweak.count == 8 else {
            fputs("Error: Tweak must be 8 bytes (16 hex chars), got \(tweak.count) bytes\n", stderr)
            exit(1)
        }

        // Create cipher
        let cipher = try createCipher(key: key, tweak: tweak, alphabetType: opts.alphabetType, customCharset: opts.customCharset)

        // Encrypt or decrypt
        if let plaintext = opts.encryptText {
            let result = try cipher.encrypt(plaintext)
            print(result)
        } else if let ciphertext = opts.decryptText {
            let result = try cipher.decrypt(ciphertext)
            print(result)
        }

    } catch {
        fputs("Error: \(error)\n", stderr)
        exit(1)
    }
}

// Extension to create Data from hex string
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

main()
