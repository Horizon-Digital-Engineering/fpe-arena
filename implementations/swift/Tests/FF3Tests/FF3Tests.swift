/**
 * FF3 Tests - Comprehensive tests including NIST FF3 test vectors.
 * Validates the Swift implementation against official NIST SP 800-38G test vectors.
 */

import XCTest
import Foundation
@testable import FF3

// NIST Test Vector Structures (same as in FF3Validate)
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

final class FF3Tests: XCTestCase {

    func testBasicFunctionality() throws {
        // NIST Sample 1 parameters
        let key = Data(hex: "EF4359D8D580AA4F7F036D6F04FC6A94")!
        let tweak = Data(hex: "D8E7920AFA330A73")!

        let cipher = try digits(key: key, tweak: tweak)

        // Test basic round-trip
        let plaintext = "1234567890"
        let ciphertext = try cipher.encrypt(plaintext)
        let decrypted = try cipher.decrypt(ciphertext)

        XCTAssertEqual(decrypted, plaintext, "Round-trip decryption should match original")
        XCTAssertNotEqual(ciphertext, plaintext, "Ciphertext should be different from plaintext")
    }

    func testNISTSample1() throws {
        // NIST Sample 1 - the golden test
        let key = Data(hex: "EF4359D8D580AA4F7F036D6F04FC6A94")!
        let tweak = Data(hex: "D8E7920AFA330A73")!

        let cipher = try digits(key: key, tweak: tweak)

        let plaintext = "890121234567890000"
        let expected = "750918814058654607"
        let result = try cipher.encrypt(plaintext)

        XCTAssertEqual(result, expected, "NIST Sample 1 must match exactly")

        // Test round-trip
        let decrypted = try cipher.decrypt(result)
        XCTAssertEqual(decrypted, plaintext, "NIST Sample 1 round-trip must work")
    }

    func testFormatPreservation() throws {
        let key = Data(hex: "EF4359D8D580AA4F7F036D6F04FC6A94")!
        let tweak = Data(hex: "D8E7920AFA330A73")!

        // Test different alphabets preserve format
        let hexCipher = try hexUpper(key: key, tweak: tweak)
        let base62Cipher = try base62(key: key, tweak: tweak)

        let testCases = [
            ("ABC123", hexCipher),
            ("Hello123", base62Cipher),
            ("TestCase", base62Cipher)
        ]

        for (plaintext, cipher) in testCases {
            let ciphertext = try cipher.encrypt(plaintext)
            let decrypted = try cipher.decrypt(ciphertext)

            XCTAssertEqual(decrypted, plaintext, "Format preservation failed for '\(plaintext)'")
            XCTAssertEqual(ciphertext.count, plaintext.count, "Length preservation failed")
        }
    }

    func testInvalidInputs() {
        let key = Data(hex: "EF4359D8D580AA4F7F036D6F04FC6A94")!
        let tweak = Data(hex: "D8E7920AFA330A73")!

        // Test invalid radix
        XCTAssertThrowsError(try FF3Cipher(radix: 1, key: key, tweak: tweak))
        XCTAssertThrowsError(try FF3Cipher(radix: 100, key: key, tweak: tweak))

        // Test invalid key length
        let shortKey = Data([1, 2, 3])
        XCTAssertThrowsError(try FF3Cipher(radix: 10, key: shortKey, tweak: tweak))

        // Test invalid tweak length
        let shortTweak = Data([1, 2, 3])
        XCTAssertThrowsError(try FF3Cipher(radix: 10, key: key, tweak: shortTweak))
    }

    func testMultipleNISTVectors() throws {
        // Try to load NIST vectors from file if available
        if let nistVectors = loadNISTVectorsFromFile() {
            print("📊 Testing \(nistVectors.count) official NIST FF3 test vectors...")

            for vector in nistVectors {
                let key = Data(hex: vector.key)!
                let tweak = Data(hex: vector.tweak)!

                let cipher: FF3
                switch vector.radix {
                case 10:
                    cipher = try digits(key: key, tweak: tweak)
                case 26:
                    let spec = AlphabetSpec(charset: vector.alphabet)
                    cipher = try fromSpec(key: key, tweak: tweak, spec: spec)
                default:
                    XCTFail("Unsupported radix \(vector.radix) in test vector \(vector.sample)")
                    continue
                }

                let result = try cipher.encrypt(vector.plaintext)
                XCTAssertEqual(result, vector.ciphertext,
                              "NIST Sample \(vector.sample) failed: expected \(vector.ciphertext), got \(result)")

                // Test round-trip
                let decrypted = try cipher.decrypt(result)
                XCTAssertEqual(decrypted, vector.plaintext,
                              "Round-trip failed for NIST Sample \(vector.sample)")
            }
        } else {
            // Fallback to hardcoded test vectors for basic validation
            let testVectors = [
                (
                    key: "EF4359D8D580AA4F7F036D6F04FC6A94",
                    tweak: "D8E7920AFA330A73",
                    plaintext: "890121234567890000",
                    ciphertext: "750918814058654607",
                    radix: 10
                ),
                (
                    key: "EF4359D8D580AA4F7F036D6F04FC6A94",
                    tweak: "9A768A92F60E12D8",
                    plaintext: "890121234567890000",
                    ciphertext: "018989839189395384",
                    radix: 10
                )
            ]

            for vector in testVectors {
                let key = Data(hex: vector.key)!
                let tweak = Data(hex: vector.tweak)!

                let cipher = try digits(key: key, tweak: tweak)
                let result = try cipher.encrypt(vector.plaintext)

                XCTAssertEqual(result, vector.ciphertext,
                              "NIST vector failed: expected \(vector.ciphertext), got \(result)")

                // Test round-trip
                let decrypted = try cipher.decrypt(result)
                XCTAssertEqual(decrypted, vector.plaintext,
                              "Round-trip failed for NIST vector")
            }
        }
    }

    private func loadNISTVectorsFromFile() -> [NistTestVector]? {
        // Try to load test vectors from environment variable first, then fallback to relative paths
        var possiblePaths = [String]()

        // Check environment variable first (for CI/CD)
        if let envPath = ProcessInfo.processInfo.environment["FF3_TEST_VECTORS_PATH"] {
            possiblePaths.append(envPath)
        }

        // Fallback to relative paths for local development
        possiblePaths.append(contentsOf: [
            "../../../shared/test-vectors/nist_ff3_official_vectors.json",
            "../../../../shared/test-vectors/nist_ff3_official_vectors.json"
        ])

        for path in possiblePaths {
            if let data = try? Data(contentsOf: URL(fileURLWithPath: path)),
               let vectors = try? JSONDecoder().decode(NistTestVectors.self, from: data) {
                print("✅ Loaded \(vectors.vectors.count) NIST test vectors from: \(path)")
                return vectors.vectors
            }
        }

        print("⚠️  Could not load NIST test vectors from file, using fallback hardcoded vectors")
        return nil
    }

    func testAlphabetSpecs() {
        // Test alphabet specifications
        XCTAssertEqual(ALPHA_DIGITS.count, 10)
        XCTAssertEqual(ALPHA_HEX_LOWER.count, 16)
        XCTAssertEqual(ALPHA_HEX_UPPER.count, 16)
        XCTAssertEqual(ALPHA_BASE36_LOW.count, 36)
        XCTAssertEqual(ALPHA_BASE36_UP.count, 36)
        XCTAssertEqual(ALPHA_BASE62.count, 62)

        // Test specs match alphabets
        XCTAssertEqual(SPEC_DIGITS.charset, ALPHA_DIGITS)
        XCTAssertEqual(SPEC_HEX_LOWER.charset, ALPHA_HEX_LOWER)
        XCTAssertEqual(SPEC_BASE62.charset, ALPHA_BASE62)
    }

    func testDifferentKeySizes() throws {
        let tweak = Data(hex: "D8E7920AFA330A73")!
        let plaintext = "1234567890"

        // Test AES-128, AES-192, AES-256
        let keys = [
            Data(hex: "EF4359D8D580AA4F7F036D6F04FC6A94")!,                         // 16 bytes
            Data(hex: "EF4359D8D580AA4F7F036D6F04FC6A942B7E151628AED2A6")!,         // 24 bytes
            Data(hex: "EF4359D8D580AA4F7F036D6F04FC6A942B7E151628AED2A6ABF7158809CF4F3C")! // 32 bytes
        ]

        for key in keys {
            let cipher = try digits(key: key, tweak: tweak)
            let ciphertext = try cipher.encrypt(plaintext)
            let decrypted = try cipher.decrypt(ciphertext)

            XCTAssertEqual(decrypted, plaintext, "Round-trip failed for \(key.count*8)-bit key")
        }
    }
}

// Extension to create Data from hex string (same as in demo)
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