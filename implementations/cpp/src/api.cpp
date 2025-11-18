/**
 * @file api.cpp
 * @brief Simple C++ FF3 API implementation
 */

#include "ff3/api.hpp"
#include <stdexcept>
#include <sstream>
#include <iomanip>

namespace ff3 {

FF3::FF3(std::unique_ptr<FF3Cipher> core, AlphabetSpec spec)
    : core_{std::move(core)}, spec_{std::move(spec)} {}

FF3::~FF3() = default;

std::string FF3::encrypt(const std::string& plaintext) const {
    const auto digits = string_to_digits(plaintext);
    const auto encrypted_digits = core_->encrypt_digits(digits);
    return digits_to_string(encrypted_digits);
}

std::string FF3::decrypt(const std::string& ciphertext) const {
    const auto digits = string_to_digits(ciphertext);
    const auto decrypted_digits = core_->decrypt_digits(digits);
    return digits_to_string(decrypted_digits);
}

std::vector<int> FF3::string_to_digits(const std::string& str) const {
    std::vector<int> digits;
    digits.reserve(str.size());
    
    for (std::size_t i = 0; i < str.size(); ++i) {
        const char c = str[i];
        const int digit = spec_.char_to_int(c);
        if (digit == -1) {
            throw std::invalid_argument("Invalid character '" + std::string{c} + 
                                      "' at position " + std::to_string(i) + 
                                      " for this alphabet");
        }
        digits.push_back(digit);
    }
    
    return digits;
}

std::string FF3::digits_to_string(const std::vector<int>& digits) const {
    std::string result;
    result.reserve(digits.size());
    
    for (std::size_t i = 0; i < digits.size(); ++i) {
        const int digit = digits[i];
        if (digit < 0 || digit >= static_cast<int>(spec_.radix())) {
            throw std::invalid_argument("Digit " + std::to_string(digit) + 
                                      " out of range for radix " + std::to_string(spec_.radix()) + 
                                      " at position " + std::to_string(i));
        }
        result.push_back(spec_.int_to_char(digit));
    }
    
    return result;
}

// Factory functions for standard alphabets
FF3 digits(const std::vector<std::uint8_t>& key, const std::vector<std::uint8_t>& tweak) {
    auto core = std::make_unique<FF3Cipher>(10, key, tweak);
    auto spec = create_digits_alphabet();
    return FF3{std::move(core), std::move(spec)};
}

FF3 hex_lower(const std::vector<std::uint8_t>& key, const std::vector<std::uint8_t>& tweak) {
    auto core = std::make_unique<FF3Cipher>(16, key, tweak);
    auto spec = create_hex_lower_alphabet();
    return FF3{std::move(core), std::move(spec)};
}

FF3 hex_upper(const std::vector<std::uint8_t>& key, const std::vector<std::uint8_t>& tweak) {
    auto core = std::make_unique<FF3Cipher>(16, key, tweak);
    auto spec = create_hex_upper_alphabet();
    return FF3{std::move(core), std::move(spec)};
}

FF3 base36_lower(const std::vector<std::uint8_t>& key, const std::vector<std::uint8_t>& tweak) {
    auto core = std::make_unique<FF3Cipher>(36, key, tweak);
    auto spec = create_base36_lower_alphabet();
    return FF3{std::move(core), std::move(spec)};
}

FF3 base36_upper(const std::vector<std::uint8_t>& key, const std::vector<std::uint8_t>& tweak) {
    auto core = std::make_unique<FF3Cipher>(36, key, tweak);
    auto spec = create_base36_upper_alphabet();
    return FF3{std::move(core), std::move(spec)};
}

FF3 base62(const std::vector<std::uint8_t>& key, const std::vector<std::uint8_t>& tweak) {
    auto core = std::make_unique<FF3Cipher>(62, key, tweak);
    auto spec = create_base62_alphabet();
    return FF3{std::move(core), std::move(spec)};
}

FF3 radix26(const std::vector<std::uint8_t>& key, const std::vector<std::uint8_t>& tweak) {
    auto core = std::make_unique<FF3Cipher>(26, key, tweak);
    auto spec = create_radix26_alphabet();
    return FF3{std::move(core), std::move(spec)};
}

// Convenience aliases
FF3 hex(const std::vector<std::uint8_t>& key, const std::vector<std::uint8_t>& tweak) {
    return hex_lower(key, tweak);
}

FF3 base36(const std::vector<std::uint8_t>& key, const std::vector<std::uint8_t>& tweak) {
    return base36_lower(key, tweak);
}

// Custom alphabet cipher
FF3 from_spec(const std::vector<std::uint8_t>& key,
              const std::vector<std::uint8_t>& tweak,
              const AlphabetSpec& spec) {
    auto core = std::make_unique<FF3Cipher>(static_cast<int>(spec.radix()), key, tweak);
    return FF3{std::move(core), spec};
}

// Utility functions
std::vector<std::uint8_t> hex_to_bytes(const std::string& hex_string) {
    if (hex_string.size() % 2 != 0) {
        throw std::invalid_argument("Hex string must have even length");
    }
    
    std::vector<std::uint8_t> result;
    result.reserve(hex_string.size() / 2);
    
    for (std::size_t i = 0; i < hex_string.size(); i += 2) {
        const std::string byte_str = hex_string.substr(i, 2);
        
        auto hex_to_nibble = [](char c) -> std::uint8_t {
            if (c >= '0' && c <= '9') return c - '0';
            if (c >= 'a' && c <= 'f') return c - 'a' + 10;
            if (c >= 'A' && c <= 'F') return c - 'A' + 10;
            throw std::invalid_argument("Invalid hex character");
        };
        
        const std::uint8_t high = hex_to_nibble(byte_str[0]);
        const std::uint8_t low = hex_to_nibble(byte_str[1]);
        result.push_back((high << 4) | low);
    }
    
    return result;
}

std::string bytes_to_hex(const std::vector<std::uint8_t>& bytes) {
    std::ostringstream oss;
    oss << std::hex << std::setfill('0');
    
    for (std::uint8_t byte : bytes) {
        oss << std::setw(2) << static_cast<int>(byte);
    }
    
    return oss.str();
}

} // namespace ff3