/**
 * @file core.hpp
 * @brief OpenSSL BIGNUM-based FF3 core cipher implementation for high performance
 */

#pragma once

#include <vector>
#include <cstdint>
#include <memory>
#include <string>
#include <openssl/bn.h>
#include <openssl/evp.h>

namespace ff3 {

/**
 * @brief OpenSSL BIGNUM wrapper for high-performance arbitrary precision arithmetic
 * Replaces the old custom BigInt implementation with OpenSSL's optimized BIGNUM
 */
class BigInt {
private:
    BIGNUM* bn_;

public:
    // Constructors and destructor
    BigInt();
    explicit BigInt(std::uint64_t value);
    explicit BigInt(const std::vector<int>& digits, int radix);
    BigInt(const BigInt& other);
    BigInt(BigInt&& other) noexcept;
    BigInt& operator=(const BigInt& other);
    BigInt& operator=(BigInt&& other) noexcept;
    ~BigInt();

    // Core arithmetic operations
    std::vector<int> to_digits(int radix) const;
    BigInt add(const BigInt& other) const;
    BigInt subtract(const BigInt& other) const;
    BigInt mod_add(const BigInt& other, const BigInt& modulus, BN_CTX* ctx) const;
    BigInt mod_subtract(const BigInt& other, const BigInt& modulus, BN_CTX* ctx) const;

    // Comparison operations
    bool is_zero() const;
    bool is_less_than(const BigInt& other) const;
    bool equals(const BigInt& other) const;

    // Radix-specific operations
    BigInt multiply_by_radix(int radix) const;
    std::pair<BigInt, int> divide_by_radix(int radix) const;
    std::vector<std::uint8_t> to_bytes() const;

    // Direct BIGNUM access for advanced operations
    const BIGNUM* bn() const { return bn_; }
    BIGNUM* bn() { return bn_; }

    // Static factory methods
    static BigInt from_bytes(const std::vector<std::uint8_t>& bytes);
    static BigInt power_of_radix(int radix, std::size_t exponent);
};

/**
 * @brief Simple AES wrapper for FF3 (now using OpenSSL optimizations)
 */
class AESCipher {
private:
    std::vector<std::uint8_t> key_;

    // Reusable AES contexts to avoid expensive create/destroy cycles
    mutable EVP_CIPHER_CTX* encrypt_ctx_;
    mutable EVP_CIPHER_CTX* decrypt_ctx_;

public:
    explicit AESCipher(const std::vector<std::uint8_t>& key);
    ~AESCipher();

    std::vector<std::uint8_t> encrypt_block(const std::vector<std::uint8_t>& plaintext) const;
    std::vector<std::uint8_t> decrypt_block(const std::vector<std::uint8_t>& ciphertext) const;
};

/**
 * @brief Simple FF3 cipher implementation (now using OpenSSL BIGNUM for performance)
 */
class FF3Cipher {
private:
    int radix_;
    std::unique_ptr<AESCipher> aes_;
    std::vector<std::uint8_t> tweak_;

    // Cached modulus values for performance
    mutable std::vector<BigInt> cached_moduli_;

    // Reusable BN_CTX for all operations
    mutable BN_CTX* bn_ctx_;

public:
    FF3Cipher(int radix, const std::vector<std::uint8_t>& key, const std::vector<std::uint8_t>& tweak);
    ~FF3Cipher();

    std::vector<int> encrypt_digits(const std::vector<int>& plaintext) const;
    std::vector<int> decrypt_digits(const std::vector<int>& ciphertext) const;

    int radix() const { return radix_; }

    // All public for experimental debugging
    std::vector<int> reverse_digits(const std::vector<int>& digits) const;
    std::vector<std::uint8_t> reverse_bytes(const std::vector<std::uint8_t>& bytes) const;
    std::vector<std::uint8_t> calculate_w(const std::vector<std::uint8_t>& tweak, int round, const std::vector<int>& half) const;
    BigInt calculate_p(int round, const std::vector<std::uint8_t>& w, const std::vector<int>& half) const;
    const BigInt& calculate_modulus(std::size_t length) const;
    BigInt num_array_to_bigint(const std::vector<int>& nums) const;
    std::vector<int> bigint_to_num_array(const BigInt& num, std::size_t length) const;
    std::vector<std::uint8_t> compute_round_tweak(int round) const;
};

} // namespace ff3