/**
 * @file core.cpp
 * @brief OpenSSL BIGNUM-based FF3 core cipher implementation for high performance
 */

#include "ff3/core.hpp"
#include <openssl/evp.h>
#include <openssl/err.h>
#include <algorithm>
#include <stdexcept>
#include <cstring>

namespace ff3 {

// BigInt implementation using OpenSSL BIGNUM
BigInt::BigInt() {
    bn_ = BN_new();
    if (!bn_) {
        throw std::runtime_error("Failed to create BIGNUM");
    }
}

BigInt::BigInt(std::uint64_t value) {
    bn_ = BN_new();
    if (!bn_) {
        throw std::runtime_error("Failed to create BIGNUM");
    }
    if (BN_set_word(bn_, value) != 1) {
        BN_free(bn_);
        throw std::runtime_error("Failed to set BIGNUM value");
    }
}

BigInt::BigInt(const std::vector<int>& digits, int radix) {
    bn_ = BN_new();
    if (!bn_) {
        throw std::runtime_error("Failed to create BIGNUM");
    }

    for (int digit : digits) {
        if (digit < 0 || digit >= radix) {
            BN_free(bn_);
            throw std::invalid_argument("Invalid digit for radix");
        }

        // bn = bn * radix + digit
        if (BN_mul_word(bn_, radix) != 1) {
            BN_free(bn_);
            throw std::runtime_error("BIGNUM multiplication failed");
        }
        if (BN_add_word(bn_, digit) != 1) {
            BN_free(bn_);
            throw std::runtime_error("BIGNUM addition failed");
        }
    }
}

BigInt::BigInt(const BigInt& other) {
    bn_ = BN_dup(other.bn_);
    if (!bn_) {
        throw std::runtime_error("Failed to duplicate BIGNUM");
    }
}

BigInt::BigInt(BigInt&& other) noexcept : bn_(other.bn_) {
    other.bn_ = nullptr;
}

BigInt& BigInt::operator=(const BigInt& other) {
    if (this != &other) {
        if (BN_copy(bn_, other.bn_) == nullptr) {
            throw std::runtime_error("Failed to copy BIGNUM");
        }
    }
    return *this;
}

BigInt& BigInt::operator=(BigInt&& other) noexcept {
    if (this != &other) {
        BN_free(bn_);
        bn_ = other.bn_;
        other.bn_ = nullptr;
    }
    return *this;
}

BigInt::~BigInt() {
    if (bn_) {
        BN_free(bn_);
    }
}

std::vector<int> BigInt::to_digits(int radix) const {
    if (is_zero()) {
        return {0};
    }

    std::vector<int> result;
    BIGNUM* temp = BN_dup(bn_);
    if (!temp) {
        throw std::runtime_error("Failed to duplicate BIGNUM");
    }

    while (!BN_is_zero(temp)) {
        BN_ULONG remainder = BN_div_word(temp, radix);
        if (remainder == static_cast<BN_ULONG>(-1)) {
            BN_free(temp);
            throw std::runtime_error("BIGNUM division failed");
        }
        result.push_back(static_cast<int>(remainder));
    }

    BN_free(temp);
    std::reverse(result.begin(), result.end());
    return result;
}

BigInt BigInt::add(const BigInt& other) const {
    BigInt result;
    if (BN_add(result.bn_, bn_, other.bn_) != 1) {
        throw std::runtime_error("BIGNUM addition failed");
    }
    return result;
}

BigInt BigInt::subtract(const BigInt& other) const {
    if (is_less_than(other)) {
        throw std::invalid_argument("Cannot subtract larger number");
    }

    BigInt result;
    if (BN_sub(result.bn_, bn_, other.bn_) != 1) {
        throw std::runtime_error("BIGNUM subtraction failed");
    }
    return result;
}

BigInt BigInt::mod_add(const BigInt& other, const BigInt& modulus, BN_CTX* ctx) const {
    BigInt result;
    // result = (this + other) % modulus
    if (BN_mod_add(result.bn_, bn_, other.bn_, modulus.bn_, ctx) != 1) {
        throw std::runtime_error("BIGNUM modular addition failed");
    }
    return result;
}

BigInt BigInt::mod_subtract(const BigInt& other, const BigInt& modulus, BN_CTX* ctx) const {
    BigInt result;
    // result = (this - other) % modulus
    if (BN_mod_sub(result.bn_, bn_, other.bn_, modulus.bn_, ctx) != 1) {
        throw std::runtime_error("BIGNUM modular subtraction failed");
    }
    return result;
}

bool BigInt::is_zero() const {
    return BN_is_zero(bn_);
}

bool BigInt::is_less_than(const BigInt& other) const {
    return BN_cmp(bn_, other.bn_) < 0;
}

bool BigInt::equals(const BigInt& other) const {
    return BN_cmp(bn_, other.bn_) == 0;
}

BigInt BigInt::multiply_by_radix(int radix) const {
    if (is_zero()) {
        return BigInt{0};
    }

    BigInt result(*this);
    if (BN_mul_word(result.bn_, radix) != 1) {
        throw std::runtime_error("BIGNUM multiplication failed");
    }
    return result;
}

std::pair<BigInt, int> BigInt::divide_by_radix(int radix) const {
    if (is_zero()) {
        return {BigInt{0}, 0};
    }

    BigInt quotient(*this);
    BN_ULONG remainder = BN_div_word(quotient.bn_, radix);
    if (remainder == static_cast<BN_ULONG>(-1)) {
        throw std::runtime_error("BIGNUM division failed");
    }

    return {std::move(quotient), static_cast<int>(remainder)};
}

std::vector<std::uint8_t> BigInt::to_bytes() const {
    if (is_zero()) {
        return {};
    }

    int num_bytes = BN_num_bytes(bn_);
    std::vector<std::uint8_t> bytes(num_bytes);

    if (BN_bn2bin(bn_, bytes.data()) != num_bytes) {
        throw std::runtime_error("BIGNUM to bytes conversion failed");
    }

    return bytes;
}

BigInt BigInt::from_bytes(const std::vector<std::uint8_t>& bytes) {
    BigInt result;
    if (!bytes.empty()) {
        if (BN_bin2bn(bytes.data(), bytes.size(), result.bn_) == nullptr) {
            throw std::runtime_error("Failed to create BIGNUM from bytes");
        }
    }
    return result;
}

BigInt BigInt::power_of_radix(int radix, std::size_t exponent) {
    BigInt result;
    BigInt base{static_cast<std::uint64_t>(radix)};
    BigInt exp{static_cast<std::uint64_t>(exponent)};

    // Use OpenSSL's optimized BN_exp instead of manual loop
    BN_CTX* ctx = BN_CTX_new();
    if (!ctx) {
        throw std::runtime_error("Failed to create BN_CTX");
    }

    // result = base^exponent
    if (BN_exp(result.bn_, base.bn_, exp.bn_, ctx) != 1) {
        BN_CTX_free(ctx);
        throw std::runtime_error("BIGNUM exponentiation failed");
    }

    BN_CTX_free(ctx);
    return result;
}

// AESCipher implementation
AESCipher::AESCipher(const std::vector<std::uint8_t>& key) : key_{key} {
    if (key.size() != 16 && key.size() != 24 && key.size() != 32) {
        throw std::invalid_argument("Invalid AES key size");
    }

    // Create and initialize reusable contexts
    encrypt_ctx_ = EVP_CIPHER_CTX_new();
    decrypt_ctx_ = EVP_CIPHER_CTX_new();

    if (!encrypt_ctx_ || !decrypt_ctx_) {
        if (encrypt_ctx_) EVP_CIPHER_CTX_free(encrypt_ctx_);
        if (decrypt_ctx_) EVP_CIPHER_CTX_free(decrypt_ctx_);
        throw std::runtime_error("Failed to create AES contexts");
    }

    // Determine cipher type based on key size
    const EVP_CIPHER* cipher = nullptr;
    switch (key_.size()) {
        case 16: cipher = EVP_aes_128_ecb(); break;
        case 24: cipher = EVP_aes_192_ecb(); break;
        case 32: cipher = EVP_aes_256_ecb(); break;
    }

    // Initialize contexts once with the key
    if (EVP_EncryptInit_ex(encrypt_ctx_, cipher, nullptr, key_.data(), nullptr) != 1 ||
        EVP_DecryptInit_ex(decrypt_ctx_, cipher, nullptr, key_.data(), nullptr) != 1) {
        EVP_CIPHER_CTX_free(encrypt_ctx_);
        EVP_CIPHER_CTX_free(decrypt_ctx_);
        throw std::runtime_error("Failed to initialize AES contexts");
    }

    // Set no padding for both contexts (FF3 uses exact block sizes)
    EVP_CIPHER_CTX_set_padding(encrypt_ctx_, 0);
    EVP_CIPHER_CTX_set_padding(decrypt_ctx_, 0);
}

AESCipher::~AESCipher() {
    if (encrypt_ctx_) {
        EVP_CIPHER_CTX_free(encrypt_ctx_);
    }
    if (decrypt_ctx_) {
        EVP_CIPHER_CTX_free(decrypt_ctx_);
    }
}

std::vector<std::uint8_t> AESCipher::encrypt_block(const std::vector<std::uint8_t>& plaintext) const {
    if (plaintext.size() != 16) {
        throw std::invalid_argument("AES block size must be 16 bytes");
    }

    // Reset the context for a new operation (no key reinit needed)
    if (EVP_EncryptInit_ex(encrypt_ctx_, nullptr, nullptr, nullptr, nullptr) != 1) {
        throw std::runtime_error("Failed to reset AES encrypt context");
    }

    std::vector<std::uint8_t> ciphertext(16);
    int len = 0;

    if (EVP_EncryptUpdate(encrypt_ctx_, ciphertext.data(), &len, plaintext.data(), 16) != 1 || len != 16) {
        throw std::runtime_error("AES encryption failed");
    }

    return ciphertext;
}

std::vector<std::uint8_t> AESCipher::decrypt_block(const std::vector<std::uint8_t>& ciphertext) const {
    if (ciphertext.size() != 16) {
        throw std::invalid_argument("AES block size must be 16 bytes");
    }

    // Reset the context for a new operation (no key reinit needed)
    if (EVP_DecryptInit_ex(decrypt_ctx_, nullptr, nullptr, nullptr, nullptr) != 1) {
        throw std::runtime_error("Failed to reset AES decrypt context");
    }

    std::vector<std::uint8_t> plaintext(16);
    int len = 0;

    if (EVP_DecryptUpdate(decrypt_ctx_, plaintext.data(), &len, ciphertext.data(), 16) != 1 || len != 16) {
        throw std::runtime_error("AES decryption failed");
    }

    return plaintext;
}

// FF3Cipher implementation
FF3Cipher::FF3Cipher(int radix, const std::vector<std::uint8_t>& key, const std::vector<std::uint8_t>& tweak)
    : radix_{radix}, tweak_{tweak} {

    if (radix < 2 || radix > 62) {
        throw std::invalid_argument("Radix must be between 2 and 62");
    }

    if (tweak.size() != 8) {
        throw std::invalid_argument("Tweak must be exactly 8 bytes");
    }

    // FF3 specification requires byte reversal of the key
    std::vector<std::uint8_t> reversed_key(key.rbegin(), key.rend());
    aes_ = std::make_unique<AESCipher>(reversed_key);

    // Initialize reusable BN_CTX
    bn_ctx_ = BN_CTX_new();
    if (!bn_ctx_) {
        throw std::runtime_error("Failed to create BN_CTX");
    }

    // Pre-cache modulus values for common lengths (optimization)
    cached_moduli_.reserve(33); // Support up to 32 digit strings
    cached_moduli_.push_back(BigInt{1}); // Index 0 (not used)
    for (std::size_t i = 1; i <= 32; ++i) {
        cached_moduli_.push_back(BigInt::power_of_radix(radix, i));
    }
}

FF3Cipher::~FF3Cipher() {
    if (bn_ctx_) {
        BN_CTX_free(bn_ctx_);
    }
}

std::vector<int> FF3Cipher::encrypt_digits(const std::vector<int>& plaintext) const {
    if (plaintext.size() < 2 || plaintext.size() > 32) {
        throw std::invalid_argument("Plaintext length must be between 2 and 32");
    }

    // Validate digits
    for (std::size_t i = 0; i < plaintext.size(); ++i) {
        if (plaintext[i] < 0 || plaintext[i] >= radix_) {
            throw std::invalid_argument("Invalid digit for radix");
        }
    }

    const std::size_t n = plaintext.size();
    const std::size_t u = (n + 1) / 2;  // ceil(n/2)
    const std::size_t v = n - u;

    std::vector<int> A(u);
    std::vector<int> B(v);
    std::copy(plaintext.begin(), plaintext.begin() + u, A.begin());
    std::copy(plaintext.begin() + u, plaintext.end(), B.begin());

    // Perform 8 Feistel rounds according to FF3 specification
    for (int i = 0; i < 8; ++i) {
        if (i % 2 == 0) {
            // Even round: use B to update A
            auto W = calculate_w(tweak_, i, B);
            auto P = calculate_p(i, W, B);
            const auto& m = calculate_modulus(u);

            // FF3 uses reversed digit order: NUM_radix(REV(A))
            std::reverse(A.begin(), A.end());  // REV(A) - NIST spec digit reversal
            auto a_num = num_array_to_bigint(A);

            // c = (NUM_radix(REV(A)) + NUM(S)) mod radix^u
            auto Y = a_num.mod_add(P, m, bn_ctx_);

            // C = REV(STR_radix(c))
            A = bigint_to_num_array(Y, u);
            std::reverse(A.begin(), A.end());  // REV(result) - NIST spec digit reversal
        } else {
            // Odd round: use A to update B
            auto W = calculate_w(tweak_, i, A);
            auto P = calculate_p(i, W, A);
            const auto& m = calculate_modulus(v);

            // FF3 uses reversed digit order: NUM_radix(REV(B))
            std::reverse(B.begin(), B.end());  // REV(B) - NIST spec digit reversal
            auto b_num = num_array_to_bigint(B);

            // c = (NUM_radix(REV(B)) + NUM(S)) mod radix^v
            auto Y = b_num.mod_add(P, m, bn_ctx_);

            // C = REV(STR_radix(c))
            B = bigint_to_num_array(Y, v);
            std::reverse(B.begin(), B.end());  // REV(result) - NIST spec digit reversal
        }
    }

    // Combine final result
    std::vector<int> result(n);
    std::copy(A.begin(), A.end(), result.begin());
    std::copy(B.begin(), B.end(), result.begin() + u);
    return result;
}

std::vector<int> FF3Cipher::decrypt_digits(const std::vector<int>& ciphertext) const {
    if (ciphertext.size() < 2 || ciphertext.size() > 32) {
        throw std::invalid_argument("Ciphertext length must be between 2 and 32");
    }

    const std::size_t n = ciphertext.size();
    const std::size_t u = (n + 1) / 2;  // ceil(n/2)
    const std::size_t v = n - u;

    std::vector<int> A(u);
    std::vector<int> B(v);
    std::copy(ciphertext.begin(), ciphertext.begin() + u, A.begin());
    std::copy(ciphertext.begin() + u, ciphertext.end(), B.begin());

    // Perform 8 Feistel rounds in reverse according to FF3 specification
    for (int i = 7; i >= 0; --i) {
        if (i % 2 == 0) {
            // Even round: use B to update A
            auto W = calculate_w(tweak_, i, B);
            auto P = calculate_p(i, W, B);
            const auto& m = calculate_modulus(u);

            // FF3 decryption: NUM_radix(REV(A)) = (c - NUM(S)) mod radix^u
            std::reverse(A.begin(), A.end());  // REV(A) - NIST spec digit reversal
            auto a_num = num_array_to_bigint(A);

            auto Y = a_num.mod_subtract(P, m, bn_ctx_);

            // C = REV(STR_radix(c))
            A = bigint_to_num_array(Y, u);
            std::reverse(A.begin(), A.end());  // REV(result) - NIST spec digit reversal
        } else {
            // Odd round: use A to update B
            auto W = calculate_w(tweak_, i, A);
            auto P = calculate_p(i, W, A);
            const auto& m = calculate_modulus(v);

            // FF3 decryption: NUM_radix(REV(B)) = (c - NUM(S)) mod radix^v
            std::reverse(B.begin(), B.end());  // REV(B) - NIST spec digit reversal
            auto b_num = num_array_to_bigint(B);

            auto Y = b_num.mod_subtract(P, m, bn_ctx_);

            // C = REV(STR_radix(c))
            B = bigint_to_num_array(Y, v);
            std::reverse(B.begin(), B.end());  // REV(result) - NIST spec digit reversal
        }
    }

    // Combine final result
    std::vector<int> result(n);
    std::copy(A.begin(), A.end(), result.begin());
    std::copy(B.begin(), B.end(), result.begin() + u);
    return result;
}

std::vector<std::uint8_t> FF3Cipher::compute_round_tweak(int round) const {
    std::vector<std::uint8_t> result(16, 0);
    std::copy(tweak_.begin(), tweak_.end(), result.begin());
    result[8] = static_cast<std::uint8_t>(round);
    return result;
}

// FF3 specification helper functions
// COMMENTED OUT - replaced with std::reverse() for performance (eliminates 32 allocations per operation)
// std::vector<int> FF3Cipher::reverse_digits(const std::vector<int>& digits) const {
//     std::vector<int> reversed(digits.size());
//     for (std::size_t i = 0; i < digits.size(); ++i) {
//         reversed[i] = digits[digits.size() - 1 - i];
//     }
//     return reversed;
// }

// COMMENTED OUT - replaced with std::reverse() for performance (eliminates 16 allocations per operation)
// std::vector<std::uint8_t> FF3Cipher::reverse_bytes(const std::vector<std::uint8_t>& bytes) const {
//     std::vector<std::uint8_t> reversed(bytes.size());
//     for (std::size_t i = 0; i < bytes.size(); ++i) {
//         reversed[i] = bytes[bytes.size() - 1 - i];
//     }
//     return reversed;
// }

const BigInt& FF3Cipher::calculate_modulus(std::size_t length) const {
    if (length == 0 || length > 32) {
        throw std::invalid_argument("Invalid modulus length");
    }
    return cached_moduli_[length];
}

BigInt FF3Cipher::num_array_to_bigint(const std::vector<int>& nums) const {
    return BigInt{nums, radix_};
}

std::vector<int> FF3Cipher::bigint_to_num_array(const BigInt& num, std::size_t length) const {
    auto digits = num.to_digits(radix_);

    // Pad with leading zeros if necessary
    while (digits.size() < length) {
        digits.insert(digits.begin(), 0);
    }

    // Truncate if too long (shouldn't happen in normal operation)
    if (digits.size() > length) {
        digits.erase(digits.begin(), digits.begin() + (digits.size() - length));
    }

    return digits;
}

std::vector<std::uint8_t> FF3Cipher::calculate_w(const std::vector<std::uint8_t>& tweak, int round, const std::vector<int>& half) const {
    // NIST FF3 W calculation: split 8-byte tweak into Tl (first 4 bytes) and Tr (last 4 bytes)
    std::vector<std::uint8_t> w(4);

    if (round % 2 == 0) {
        // Even rounds: W = Tr (rightmost 4 bytes)
        std::copy(tweak.begin() + 4, tweak.begin() + 8, w.begin());
    } else {
        // Odd rounds: W = Tl (leftmost 4 bytes)
        std::copy(tweak.begin(), tweak.begin() + 4, w.begin());
    }

    return w;
}

BigInt FF3Cipher::calculate_p(int round, const std::vector<std::uint8_t>& w, const std::vector<int>& half) const {
    // NIST FF3 P calculation with proper byte reversal
    // P = REVB(AES-ECB(K, REVB(W XOR [i]_4 || NUM_radix(REV(B)))))

    std::vector<std::uint8_t> input(16, 0);

    // First 4 bytes: W XOR with round number in the last byte of W
    std::copy(w.begin(), w.end(), input.begin());
    input[3] ^= static_cast<std::uint8_t>(round);

    // Last 12 bytes: NUM_radix(REV(B)) - reverse digits of B first
    auto reversed_half = half;  // Make mutable copy
    std::reverse(reversed_half.begin(), reversed_half.end());  // REV(half) - NIST spec digit reversal
    auto half_bignum = num_array_to_bigint(reversed_half);
    auto half_bytes = half_bignum.to_bytes(); // Convert to bytes

    // Pad to 12 bytes (big-endian)
    std::vector<std::uint8_t> half_padded(12, 0);
    if (!half_bytes.empty()) {
        std::size_t start_pos = 12 - std::min(half_bytes.size(), std::size_t{12});
        for (std::size_t i = 0; i < std::min(half_bytes.size(), std::size_t{12}); ++i) {
            half_padded[start_pos + i] = half_bytes[i];
        }
    }
    std::copy(half_padded.begin(), half_padded.end(), input.begin() + 4);

    // Apply FF3 byte reversal convention: REVB before AES
    std::reverse(input.begin(), input.end());  // REVB(input) - NIST spec byte reversal

    // Encrypt with AES
    auto aes_output = aes_->encrypt_block(input);

    // Apply FF3 byte reversal convention: REVB after AES
    std::reverse(aes_output.begin(), aes_output.end());  // REVB(output) - NIST spec byte reversal

    // Convert to BigInt - interpret bytes as big-endian binary number
    return BigInt::from_bytes(aes_output);
}

} // namespace ff3