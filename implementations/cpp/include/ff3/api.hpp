/**
 * @file api.hpp
 * @brief Simple C++ FF3 API with string ↔ digits conversion
 */

#pragma once

#include "core.hpp"
#include "alphabets.hpp"
#include <string>
#include <string_view>
#include <vector>

namespace ff3 {

/**
 * @brief Simple cipher with alphabet-specific string conversion
 */
class FF3 {
private:
    std::unique_ptr<FF3Cipher> core_;
    AlphabetSpec spec_;
    
public:
    FF3(std::unique_ptr<FF3Cipher> core, AlphabetSpec spec);
    ~FF3();
    
    std::string encrypt(const std::string& plaintext) const;
    std::string decrypt(const std::string& ciphertext) const;
    
    const AlphabetSpec& alphabet_spec() const { return spec_; }
    std::size_t radix() const { return spec_.radix(); }
    
private:
    std::vector<int> string_to_digits(const std::string& str) const;
    std::string digits_to_string(const std::vector<int>& digits) const;
};

// Factory functions for standard alphabets
FF3 digits(const std::vector<std::uint8_t>& key, const std::vector<std::uint8_t>& tweak);
FF3 hex(const std::vector<std::uint8_t>& key, const std::vector<std::uint8_t>& tweak);
FF3 hex_lower(const std::vector<std::uint8_t>& key, const std::vector<std::uint8_t>& tweak);
FF3 hex_upper(const std::vector<std::uint8_t>& key, const std::vector<std::uint8_t>& tweak);
FF3 base36(const std::vector<std::uint8_t>& key, const std::vector<std::uint8_t>& tweak);
FF3 base36_lower(const std::vector<std::uint8_t>& key, const std::vector<std::uint8_t>& tweak);
FF3 base36_upper(const std::vector<std::uint8_t>& key, const std::vector<std::uint8_t>& tweak);
FF3 base62(const std::vector<std::uint8_t>& key, const std::vector<std::uint8_t>& tweak);
FF3 radix26(const std::vector<std::uint8_t>& key, const std::vector<std::uint8_t>& tweak);

// Custom alphabet cipher
FF3 from_spec(const std::vector<std::uint8_t>& key,
              const std::vector<std::uint8_t>& tweak,
              const AlphabetSpec& spec);

// Utility functions
std::vector<std::uint8_t> hex_to_bytes(const std::string& hex_string);
std::string bytes_to_hex(const std::vector<std::uint8_t>& bytes);

} // namespace ff3