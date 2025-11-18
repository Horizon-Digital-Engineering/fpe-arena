/**
 * FF3 Core - Pure cryptographic implementation of FF3 Format Preserving Encryption.
 * This file contains the core algorithm math, equivalent to other language implementations.
 */

import Foundation
import BigInt

#if canImport(CommonCrypto)
import CommonCrypto
#else
import CryptoSwift
#endif

/**
 * Core FF3 cipher instance with pure cryptographic math.
 * Handles the 8-round Feistel network implementation according to NIST SP 800-38G.
 * Uses proper byte-reversal conventions and AES encryption as specified.
 */
public class FF3Cipher {
    private let radix: Int
    private let key: Data
    private let tweak: Data
    private let aes: AES
    private let maxLength: Int
    private let minLength: Int = 2

    // Performance optimization: cache BigInt(radix) to avoid repeated creation
    private let radixBig: BigInt

    // Performance optimization: cache for radix powers (computed lazily per input length)
    private var radixPowerCache: [Int: BigInt] = [:]

    public enum FF3Error: Error, LocalizedError {
        case invalidRadix(String)
        case invalidKeyLength(String)
        case invalidTweakLength(String)
        case invalidInputLength(String)
        case encryptionFailed(String)

        public var errorDescription: String? {
            switch self {
            case .invalidRadix(let msg): return "Invalid radix: \(msg)"
            case .invalidKeyLength(let msg): return "Invalid key length: \(msg)"
            case .invalidTweakLength(let msg): return "Invalid tweak length: \(msg)"
            case .invalidInputLength(let msg): return "Invalid input length: \(msg)"
            case .encryptionFailed(let msg): return "Encryption failed: \(msg)"
            }
        }
    }

    /**
     * Initialize FF3 cipher with the given radix, encryption key, and tweak.
     * Validates input parameters according to FF3 specification requirements.
     * Creates the internal AES cipher with proper byte-reversal as required by FF3.
     * Sets up maximum/minimum length constraints based on the radix value.
     */
    public init(radix: Int, key: Data, tweak: Data) throws {
        // Validate radix - relaxed for experimental repo
        guard radix >= 2 && radix <= 62 else {
            throw FF3Error.invalidRadix("radix must be between 2 and 62, got \(radix)")
        }

        // Validate key length
        guard [16, 24, 32].contains(key.count) else {
            throw FF3Error.invalidKeyLength("key length must be 16, 24, or 32 bytes, got \(key.count)")
        }

        // Validate tweak length (FF3 requires exactly 64 bits = 8 bytes)
        guard tweak.count == 8 else {
            throw FF3Error.invalidTweakLength("tweak must be exactly 8 bytes (64 bits) for FF3, got \(tweak.count)")
        }

        self.radix = radix
        self.radixBig = BigInt(radix)  // Cache for performance
        self.key = key
        self.tweak = tweak

        // Create AES cipher with FF3 byte-reversal convention
        self.aes = try AES(key: key)

        // Calculate maximum length for FF3 (more permissive for experimental use)
        if radix >= 2 && radix <= 36 {
            self.maxLength = 32  // For small radixes, allow reasonable lengths
        } else {
            self.maxLength = 56  // Conservative maximum for most practical use cases
        }
    }

    public func getMinLength() -> Int {
        return minLength
    }

    public func getMaxLength() -> Int {
        return maxLength
    }

    /**
     * Encrypt an array of digits using the FF3 Format Preserving Encryption algorithm.
     * Takes integer digits within the specified radix and returns encrypted digits in same format.
     * Optional additionalTweak parameter allows for domain separation or context-specific encryption.
     * The output preserves the exact length and radix constraints of the input.
     */
    public func encryptDigits(_ plaintext: [Int], additionalTweak: Data? = nil) throws -> [Int] {
        // Merge tweaks - main tweak + optional additional tweak
        var effectiveTweak = tweak
        if let additionalTweak = additionalTweak {
            // Simple XOR merge (8 bytes only)
            var merged = Data(tweak)
            for i in 0..<min(additionalTweak.count, 8) {
                merged[i] ^= additionalTweak[i]
            }
            effectiveTweak = merged
        }

        return try ff3Encrypt(plaintext, tweak: effectiveTweak)
    }

    /**
     * Decrypt an array of digits using the FF3 Format Preserving Encryption algorithm.
     * Takes encrypted integer digits and returns the original plaintext digits in same format.
     * Must use the same additionalTweak (if any) that was used during encryption.
     * Performs the inverse of the encryption process through reverse Feistel rounds.
     */
    public func decryptDigits(_ ciphertext: [Int], additionalTweak: Data? = nil) throws -> [Int] {
        // Merge tweaks - main tweak + optional additional tweak
        var effectiveTweak = tweak
        if let additionalTweak = additionalTweak {
            // Simple XOR merge (8 bytes only)
            var merged = Data(tweak)
            for i in 0..<min(additionalTweak.count, 8) {
                merged[i] ^= additionalTweak[i]
            }
            effectiveTweak = merged
        }

        return try ff3Decrypt(ciphertext, tweak: effectiveTweak)
    }

    /**
     * Core FF3 encryption implementation using 8-round Feistel network.
     * Splits input into left/right halves and applies alternating round functions.
     * Each round uses AES-based round function with proper digit reversal per NIST spec.
     * Implements the exact algorithm from NIST SP 800-38G specification.
     */
    private func ff3Encrypt(_ plaintext: [Int], tweak: Data) throws -> [Int] {
        let n = plaintext.count

        // Split plaintext into left and right halves (u = ceil(n/2), v = n - u)
        let u = (n + 1) / 2  // This gives ceil(n/2)
        let v = n - u

        var A = Array(plaintext[0..<u])
        var B = Array(plaintext[u..<n])

        // Perform 8 Feistel rounds according to FF3 specification
        for i in 0..<8 {
            if i % 2 == 0 {
                // Even round: use B to update A
                let W = calculateW(tweak: tweak, roundNum: i, block: B)
                let P = try calculateP(roundNum: i, w: W, block: B)
                let m = pow(radix, u)

                // FF3 uses reversed digit order: NUM_radix(REV(A))
                // Performance: use direct reversed conversion to avoid array allocation
                let aNum = reversedNumArrayToBigInt(A)

                // c = (NUM_radix(REV(A)) + NUM(S)) mod radix^u
                let y = (aNum + P) % m

                // C = REV(STR_radix(c))
                // Performance: convert directly to reversed array
                A = bigIntToReversedNumArray(y, length: u)
            } else {
                // Odd round: use A to update B
                let W = calculateW(tweak: tweak, roundNum: i, block: A)
                let P = try calculateP(roundNum: i, w: W, block: A)
                let m = pow(radix, v)

                // FF3 uses reversed digit order: NUM_radix(REV(B))
                // Performance: use direct reversed conversion to avoid array allocation
                let bNum = reversedNumArrayToBigInt(B)

                // c = (NUM_radix(REV(B)) + NUM(S)) mod radix^v
                let y = (bNum + P) % m

                // C = REV(STR_radix(c))
                // Performance: convert directly to reversed array
                B = bigIntToReversedNumArray(y, length: v)
            }
        }

        // Combine final A and B
        return A + B
    }

    /**
     * Core FF3 decryption implementation using reverse 8-round Feistel network.
     * Performs the exact inverse of encryption by running Feistel rounds in reverse order.
     * Uses the same round function but with subtraction instead of addition.
     * Restores the original plaintext from the ciphertext using the same key and tweak.
     */
    private func ff3Decrypt(_ ciphertext: [Int], tweak: Data) throws -> [Int] {
        let n = ciphertext.count

        // Split ciphertext into left and right halves
        let u = (n + 1) / 2
        let v = n - u

        var A = Array(ciphertext[0..<u])
        var B = Array(ciphertext[u..<n])

        // Perform 8 Feistel rounds in reverse order
        for i in (0..<8).reversed() {
            if i % 2 == 0 {
                // Even round: use B to update A (reverse)
                let W = calculateW(tweak: tweak, roundNum: i, block: B)
                let P = try calculateP(roundNum: i, w: W, block: B)
                let m = pow(radix, u)

                // FF3 uses reversed digit order: NUM_radix(REV(A))
                // Performance: use direct reversed conversion to avoid array allocation
                let aNum = reversedNumArrayToBigInt(A)

                // c = (NUM_radix(REV(A)) - NUM(S)) mod radix^u
                var y = (aNum - P) % m
                if y < 0 { y = y + m }  // Ensure positive result

                // C = REV(STR_radix(c))
                // Performance: convert directly to reversed array
                A = bigIntToReversedNumArray(y, length: u)
            } else {
                // Odd round: use A to update B (reverse)
                let W = calculateW(tweak: tweak, roundNum: i, block: A)
                let P = try calculateP(roundNum: i, w: W, block: A)
                let m = pow(radix, v)

                // FF3 uses reversed digit order: NUM_radix(REV(B))
                // Performance: use direct reversed conversion to avoid array allocation
                let bNum = reversedNumArrayToBigInt(B)

                // c = (NUM_radix(REV(B)) - NUM(S)) mod radix^v
                var y = (bNum - P) % m
                if y < 0 { y = y + m }  // Ensure positive result

                // C = REV(STR_radix(c))
                // Performance: convert directly to reversed array
                B = bigIntToReversedNumArray(y, length: v)
            }
        }

        // Combine final A and B
        return A + B
    }

    /**
     * Calculate the W parameter for FF3 round function according to NIST specification.
     * Splits the 8-byte tweak into left (Tl) and right (Tr) 4-byte halves.
     * Uses Tr for even rounds and Tl for odd rounds as required by FF3.
     * This ensures proper tweak utilization across all 8 Feistel rounds.
     */
    private func calculateW(tweak: Data, roundNum: Int, block: [Int]) -> Data {
        // NIST FF3 W calculation: split 8-byte tweak into Tl (first 4 bytes) and Tr (last 4 bytes)
        var w = Data(count: 4)

        if roundNum % 2 == 0 {
            // Even rounds: W = Tr (rightmost 4 bytes)
            w = Data(tweak[4..<8])
        } else {
            // Odd rounds: W = Tl (leftmost 4 bytes)
            w = Data(tweak[0..<4])
        }

        return w
    }

    /**
     * Calculate the P value for FF3 round function using AES encryption.
     * Constructs 16-byte input from W XOR round_number and reversed block digits.
     * Applies FF3 byte-reversal before and after AES encryption as per NIST spec.
     * Returns the encrypted result as an integer for use in Feistel round arithmetic.
     */
    private func calculateP(roundNum: Int, w: Data, block: [Int]) throws -> BigInt {
        // NIST FF3 P calculation with proper byte reversal
        // P = REVB(AES-ECB(K, REVB(W XOR [i]_4 || NUM_radix(REV(B)))))

        // Create 16-byte input
        var inputData = Data(count: 16)

        // First 4 bytes: W XOR with round number in the last byte of W
        inputData[0..<4] = w
        inputData[3] ^= UInt8(roundNum)

        // Last 12 bytes: NUM_radix(REV(B)) - reverse digits of B first
        let reversedBlock = Array(block.reversed())
        let blockNum = numArrayToBigInt(reversedBlock)

        // Convert BigInt to bytes with proper padding to 12 bytes
        let blockBytes: Data
        if blockNum == 0 {
            blockBytes = Data(count: 12)
        } else {
            // Convert BigInt to bytes manually (like C++ to_bytes method)
            var result: [UInt8] = []
            var temp = blockNum
            let base = BigInt(256)

            while temp > 0 {
                let (quotient, remainder) = temp.quotientAndRemainder(dividingBy: base)
                result.insert(UInt8(remainder), at: 0)  // Insert at beginning for big-endian
                temp = quotient
            }

            let blockBytesRaw = Data(result)
            if blockBytesRaw.count <= 12 {
                // Pad to 12 bytes (big-endian)
                blockBytes = Data(count: 12 - blockBytesRaw.count) + blockBytesRaw
            } else {
                // Truncate to 12 bytes if larger (take the least significant 12 bytes)
                blockBytes = Data(blockBytesRaw.suffix(12))
            }
        }

        inputData[4..<16] = blockBytes

        // Apply FF3 byte reversal convention: REVB before AES
        let reversedInput = Data(inputData.reversed())

        // Encrypt with AES
        let aesOutput = try aes.encrypt(reversedInput)

        // Apply FF3 byte reversal convention: REVB after AES
        let output = Data(aesOutput.reversed())

        // Convert 16-byte AES output to BigInt - interpret bytes as big-endian binary number (like Go's SetBytes)
        // CRITICAL FIX: BigInt(Data) constructor doesn't handle big-endian conversion properly
        // Must manually convert each byte to match C++/Go implementations
        var result = BigInt(0)
        for byte in output {
            result = result * 256 + BigInt(byte)
        }
        return result
    }

    /**
     * Convert an array of digits to a single integer using the cipher's radix.
     * Treats the digit array as a number in the specified radix base.
     * Most significant digit is first in the array (big-endian representation).
     * Used to convert digit blocks to integers for arithmetic operations in Feistel rounds.
     */
    private func numArrayToInt(_ digits: [Int]) -> Int {
        var result = 0
        for digit in digits {
            result = result * radix + digit
        }
        return result
    }

    private func numArrayToBigInt(_ digits: [Int]) -> BigInt {
        var result = BigInt(0)
        // Use cached radixBig for performance
        for digit in digits {
            result = result * radixBig + BigInt(digit)
        }
        return result
    }

    /**
     * Convert reversed digit array to BigInt without creating intermediate array.
     * Performance optimization to avoid allocating temporary reversed arrays.
     */
    private func reversedNumArrayToBigInt(_ digits: [Int]) -> BigInt {
        var result = BigInt(0)
        var multiplier = BigInt(1)
        // Iterate backwards to effectively reverse
        for digit in digits {
            result = result + BigInt(digit) * multiplier
            multiplier = multiplier * radixBig
        }
        return result
    }

    /**
     * Convert an integer to a digit array of specified length using the cipher's radix.
     * Performs base conversion and pads with leading zeros to reach target length.
     * Returns digits in big-endian order (most significant digit first).
     * Used to convert Feistel round results back to digit arrays for further processing.
     */
    private func intToNumArray(_ num: Int, length: Int) -> [Int] {
        return bigIntToNumArray(BigInt(num), length: length)
    }

    private func bigIntToNumArray(_ num: BigInt, length: Int) -> [Int] {
        var result = Array(repeating: 0, count: length)
        // Use cached radixBig for performance

        // Ensure we work with a positive number
        var temp = num >= 0 ? num : num + pow(radix, length)

        for i in (0..<length).reversed() {
            let (quotient, remainder) = temp.quotientAndRemainder(dividingBy: radixBig)
            let digit = Int(remainder)

            // Ensure digit is positive and within radix range
            result[i] = digit >= 0 ? digit : digit + radix
            temp = quotient
        }

        return result
    }

    /**
     * Convert BigInt to reversed digit array without creating intermediate array.
     * Performance optimization to avoid allocating and reversing arrays.
     */
    private func bigIntToReversedNumArray(_ num: BigInt, length: Int) -> [Int] {
        var result = Array(repeating: 0, count: length)
        // Use cached radixBig for performance

        // Ensure we work with a positive number
        var temp = num >= 0 ? num : num + pow(radix, length)

        // Fill array in reverse order directly
        for i in 0..<length {
            let (quotient, remainder) = temp.quotientAndRemainder(dividingBy: radixBig)
            let digit = Int(remainder)

            // Ensure digit is positive and within radix range
            result[i] = digit >= 0 ? digit : digit + radix
            temp = quotient
        }

        return result
    }

    /**
     * Helper function to calculate integer power using BigInt arithmetic.
     * Avoids overflow issues for large radix^length calculations in FF3.
     * Uses caching to avoid recalculating the same powers repeatedly.
     */
    private func pow(_ base: Int, _ exponent: Int) -> BigInt {
        // Check cache first
        if let cached = radixPowerCache[exponent] {
            return cached
        }

        // Calculate and cache
        let result = radixBig.power(exponent)
        radixPowerCache[exponent] = result
        return result
    }
}

/**
 * AES wrapper for FF3-specific operations.
 * Handles the byte-reversal convention required by FF3.
 * Uses native CommonCrypto on Apple platforms for maximum performance,
 * falls back to CryptoSwift on Linux/Windows.
 */
private struct AES {
    private let key: Array<UInt8>

    /**
     * Create AES cipher instance with FF3-specific byte-reversal convention.
     * FF3 specification requires the encryption key to be byte-reversed before use.
     * This ensures compatibility with the official NIST FF3 test vectors.
     * Returns a configured AES cipher for use in round functions.
     */
    init(key: Data) throws {
        // FF3 specification requires byte reversal of the key
        let reversedKey = Data(key.reversed())
        self.key = Array(reversedKey)
    }

    func encrypt(_ data: Data) throws -> Data {
        guard data.count == 16 else {
            throw FF3Cipher.FF3Error.encryptionFailed("AES input must be exactly 16 bytes")
        }

        #if canImport(CommonCrypto)
        // Use CommonCrypto on Apple platforms (hardware-accelerated AES)
        var outLength: Int = 0
        var outBytes = [UInt8](repeating: 0, count: data.count)
        let status = CCCrypt(
            CCOperation(kCCEncrypt),
            CCAlgorithm(kCCAlgorithmAES),
            CCOptions(kCCOptionECBMode),
            key, key.count,
            nil,  // No IV for ECB
            Array(data), data.count,
            &outBytes, outBytes.count,
            &outLength
        )

        guard status == kCCSuccess else {
            throw FF3Cipher.FF3Error.encryptionFailed("AES-ECB encryption failed with status \(status)")
        }

        return Data(outBytes)
        #else
        // Use CryptoSwift on non-Apple platforms
        do {
            let aes = try CryptoSwift.AES(key: key, blockMode: ECB(), padding: .noPadding)
            let encrypted = try aes.encrypt(Array(data))
            return Data(encrypted)
        } catch {
            throw FF3Cipher.FF3Error.encryptionFailed("AES-ECB encryption failed: \(error)")
        }
        #endif
    }
}