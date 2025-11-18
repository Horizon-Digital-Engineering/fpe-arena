/**
 * @file alphabets.hpp
 * @brief Simple C++ FF3 alphabets - no template complexity
 */

#pragma once

#include <string>
#include <string_view>
#include <unordered_map>
#include <stdexcept>

namespace ff3 {

/**
 * @brief Simple alphabet specification
 */
class AlphabetSpec {
private:
    std::string charset_;
    std::unordered_map<char, int> char_to_int_;
    
public:
    explicit AlphabetSpec(std::string_view charset);
    
    char int_to_char(int index) const;
    int char_to_int(char c) const;
    bool is_valid_char(char c) const;
    std::size_t radix() const;
    const std::string& chars() const;
};

// Standard alphabet factory functions
AlphabetSpec create_digits_alphabet();
AlphabetSpec create_hex_lower_alphabet();
AlphabetSpec create_hex_upper_alphabet();
AlphabetSpec create_base36_lower_alphabet();
AlphabetSpec create_base36_upper_alphabet();
AlphabetSpec create_base62_alphabet();
AlphabetSpec create_radix26_alphabet();

// Convenience aliases
AlphabetSpec create_hex_alphabet();
AlphabetSpec create_base36_alphabet();

} // namespace ff3