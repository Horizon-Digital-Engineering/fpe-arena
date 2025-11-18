/**
 * @file alphabets.cpp
 * @brief Simple C++ FF3 alphabets implementation
 */

#include "ff3/alphabets.hpp"

namespace ff3 {

AlphabetSpec::AlphabetSpec(std::string_view charset) : charset_{charset} {
    char_to_int_.reserve(charset.size());
    for (std::size_t i = 0; i < charset.size(); ++i) {
        char_to_int_[charset[i]] = static_cast<int>(i);
    }
}

char AlphabetSpec::int_to_char(int index) const {
    if (index < 0 || index >= static_cast<int>(charset_.size())) {
        throw std::out_of_range("Index out of range for alphabet");
    }
    return charset_[index];
}

int AlphabetSpec::char_to_int(char c) const {
    auto it = char_to_int_.find(c);
    return it != char_to_int_.end() ? it->second : -1;
}

bool AlphabetSpec::is_valid_char(char c) const {
    return char_to_int_.find(c) != char_to_int_.end();
}

std::size_t AlphabetSpec::radix() const {
    return charset_.size();
}

const std::string& AlphabetSpec::chars() const {
    return charset_;
}

// Standard alphabet factory functions
AlphabetSpec create_digits_alphabet() {
    return AlphabetSpec{"0123456789"};
}

AlphabetSpec create_hex_lower_alphabet() {
    return AlphabetSpec{"0123456789abcdef"};
}

AlphabetSpec create_hex_upper_alphabet() {
    return AlphabetSpec{"0123456789ABCDEF"};
}

AlphabetSpec create_base36_lower_alphabet() {
    return AlphabetSpec{"0123456789abcdefghijklmnopqrstuvwxyz"};
}

AlphabetSpec create_base36_upper_alphabet() {
    return AlphabetSpec{"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"};
}

AlphabetSpec create_base62_alphabet() {
    return AlphabetSpec{"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"};
}

AlphabetSpec create_radix26_alphabet() {
    return AlphabetSpec{"0123456789abcdefghijklmnop"};
}

// Convenience aliases
AlphabetSpec create_hex_alphabet() {
    return create_hex_lower_alphabet();
}

AlphabetSpec create_base36_alphabet() {
    return create_base36_lower_alphabet();
}

} // namespace ff3