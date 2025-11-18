/**
 * @file ff3.hpp
 * @brief Simple C++ FF3 Format Preserving Encryption Library
 * 
 * ⚠️ SECURITY WARNING ⚠️
 * FF3 was withdrawn by NIST due to security vulnerabilities.
 * This implementation is for EDUCATIONAL and RESEARCH purposes only.
 * DO NOT use in production systems.
 * 
 * Quick Start:
 * ```cpp
 * #include <ff3.hpp>
 * 
 * const auto key = ff3::hex_to_bytes("EF4359D8D580AA4F7F036D6F04FC6A94");
 * const auto tweak = ff3::hex_to_bytes("D8E7920AFA330A73");
 *
 * auto cipher = ff3::digits(key, tweak);
 * 
 * const std::string plaintext = "1234567890";
 * const auto encrypted = cipher.encrypt(plaintext);
 * const auto decrypted = cipher.decrypt(encrypted);
 * 
 * assert(decrypted == plaintext);
 * ```
 */

#pragma once

// Core includes
#include <cstdio>
#include "ff3/alphabets.hpp"
#include "ff3/core.hpp" 
#include "ff3/api.hpp"

namespace ff3 {

/**
 * @brief Library version information
 */
constexpr const char* VERSION = "0.1.0";

/**
 * @brief Security warning
 */
inline void print_security_warning() {
    std::fprintf(stderr, 
        "\n⚠️  FF3 Format Preserving Encryption Library v%s\n"
        "\n"
        "    FF3 was WITHDRAWN by NIST due to security vulnerabilities.\n"
        "    This implementation is for EDUCATIONAL and RESEARCH purposes only.\n"
        "\n"
        "    DO NOT use in production systems.\n"
        "\n"
        "    For more information: https://github.com/horizondigitalengineering/fpe-ff3\n"
        "\n",
        VERSION);
}

} // namespace ff3

// Automatic security warning (can be disabled with FF3_NO_WARNING)
#ifndef FF3_NO_WARNING
namespace {
    static const auto security_warning_printer = []() {
        ff3::print_security_warning();
        return 0;
    }();
}
#endif