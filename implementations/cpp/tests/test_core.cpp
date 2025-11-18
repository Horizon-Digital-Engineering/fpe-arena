/**
 * @file test_core.cpp
 * @brief Catch2 unit tests for FF3 core cryptographic functionality
 */

#include <catch2/catch_test_macros.hpp>
#include <ff3/core.hpp>
#include <vector>
#include <string>

using namespace ff3;

TEST_CASE("FF3Cipher basic functionality", "[core]") {
    // Standard test key and tweak
    std::vector<uint8_t> key = {
        0xEF, 0x43, 0x59, 0xD8, 0xD5, 0x80, 0xAA, 0x4F,
        0x7F, 0x03, 0x6D, 0x6F, 0x04, 0xFC, 0x6A, 0x94
    };
    std::vector<uint8_t> tweak = {
        0xD8, 0xE7, 0x92, 0x0A, 0xFA, 0x33, 0x0A, 0x73
    };

    SECTION("Constructor with valid parameters") {
        REQUIRE_NOTHROW(FF3Cipher(10, key, tweak));
        REQUIRE_NOTHROW(FF3Cipher(26, key, tweak));
        REQUIRE_NOTHROW(FF3Cipher(62, key, tweak));
    }

    SECTION("Constructor with invalid radix") {
        REQUIRE_THROWS(FF3Cipher(1, key, tweak));
        REQUIRE_THROWS(FF3Cipher(63, key, tweak));
    }

    SECTION("Constructor with invalid key length") {
        std::vector<uint8_t> short_key = {0x01, 0x02, 0x03};
        REQUIRE_THROWS(FF3Cipher(10, short_key, tweak));
    }

    SECTION("Constructor with invalid tweak length") {
        std::vector<uint8_t> short_tweak = {0x01, 0x02};
        REQUIRE_THROWS(FF3Cipher(10, key, short_tweak));
    }

    SECTION("Encrypt and decrypt digits") {
        FF3Cipher cipher(10, key, tweak);

        std::vector<int> plaintext = {8, 9, 0, 1, 2, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 0, 0, 0};
        auto ciphertext = cipher.encrypt_digits(plaintext);
        auto decrypted = cipher.decrypt_digits(ciphertext);

        REQUIRE(ciphertext.size() == plaintext.size());
        REQUIRE(decrypted == plaintext);

        // Check that ciphertext is different from plaintext (with very high probability)
        REQUIRE(ciphertext != plaintext);
    }

    SECTION("Encrypt preserves digit count") {
        FF3Cipher cipher(10, key, tweak);

        std::vector<int> input = {1, 2, 3, 4, 5};
        auto output = cipher.encrypt_digits(input);

        REQUIRE(output.size() == input.size());
        for (int digit : output) {
            REQUIRE(digit >= 0);
            REQUIRE(digit < 10);
        }
    }

    SECTION("Different inputs produce different outputs") {
        FF3Cipher cipher(10, key, tweak);

        std::vector<int> input1 = {1, 2, 3, 4, 5, 6, 7, 8, 9, 0};
        std::vector<int> input2 = {0, 9, 8, 7, 6, 5, 4, 3, 2, 1};

        auto output1 = cipher.encrypt_digits(input1);
        auto output2 = cipher.encrypt_digits(input2);

        REQUIRE(output1 != output2);
    }

    SECTION("Same input produces same output") {
        FF3Cipher cipher(10, key, tweak);

        std::vector<int> input = {1, 2, 3, 4, 5};
        auto output1 = cipher.encrypt_digits(input);
        auto output2 = cipher.encrypt_digits(input);

        REQUIRE(output1 == output2);
    }

    SECTION("Different tweaks produce different outputs") {
        std::vector<uint8_t> tweak2 = {
            0x9A, 0x76, 0x8A, 0x92, 0xF6, 0x0E, 0x12, 0xD8
        };

        FF3Cipher cipher1(10, key, tweak);
        FF3Cipher cipher2(10, key, tweak2);

        std::vector<int> input = {1, 2, 3, 4, 5};
        auto output1 = cipher1.encrypt_digits(input);
        auto output2 = cipher2.encrypt_digits(input);

        REQUIRE(output1 != output2);
    }
}

TEST_CASE("FF3Cipher radix handling", "[core][radix]") {
    std::vector<uint8_t> key = {
        0xEF, 0x43, 0x59, 0xD8, 0xD5, 0x80, 0xAA, 0x4F,
        0x7F, 0x03, 0x6D, 0x6F, 0x04, 0xFC, 0x6A, 0x94
    };
    std::vector<uint8_t> tweak = {
        0xD8, 0xE7, 0x92, 0x0A, 0xFA, 0x33, 0x0A, 0x73
    };

    SECTION("Radix 26 operations") {
        FF3Cipher cipher(26, key, tweak);

        std::vector<int> input = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18};
        auto encrypted = cipher.encrypt_digits(input);
        auto decrypted = cipher.decrypt_digits(encrypted);

        REQUIRE(decrypted == input);

        // Check all digits are in valid range
        for (int digit : encrypted) {
            REQUIRE(digit >= 0);
            REQUIRE(digit < 26);
        }
    }

    SECTION("Small radix operations") {
        FF3Cipher cipher(2, key, tweak);

        std::vector<int> input = {0, 1, 1, 0, 1, 0, 0, 1};
        auto encrypted = cipher.encrypt_digits(input);
        auto decrypted = cipher.decrypt_digits(encrypted);

        REQUIRE(decrypted == input);

        // Check all digits are binary
        for (int digit : encrypted) {
            REQUIRE(digit >= 0);
            REQUIRE(digit < 2);
        }
    }

    SECTION("Large radix operations") {
        FF3Cipher cipher(62, key, tweak);

        std::vector<int> input = {0, 10, 35, 61, 25, 45};
        auto encrypted = cipher.encrypt_digits(input);
        auto decrypted = cipher.decrypt_digits(encrypted);

        REQUIRE(decrypted == input);

        // Check all digits are in valid range
        for (int digit : encrypted) {
            REQUIRE(digit >= 0);
            REQUIRE(digit < 62);
        }
    }
}

TEST_CASE("FF3Cipher edge cases", "[core][edge]") {
    std::vector<uint8_t> key = {
        0xEF, 0x43, 0x59, 0xD8, 0xD5, 0x80, 0xAA, 0x4F,
        0x7F, 0x03, 0x6D, 0x6F, 0x04, 0xFC, 0x6A, 0x94
    };
    std::vector<uint8_t> tweak = {
        0xD8, 0xE7, 0x92, 0x0A, 0xFA, 0x33, 0x0A, 0x73
    };

    SECTION("Empty input") {
        FF3Cipher cipher(10, key, tweak);

        std::vector<int> empty_input;
        REQUIRE_THROWS(cipher.encrypt_digits(empty_input));
    }

    SECTION("Single digit") {
        FF3Cipher cipher(10, key, tweak);

        std::vector<int> input = {5};
        REQUIRE_THROWS(cipher.encrypt_digits(input));  // Too short for FF3
    }

    SECTION("Maximum length input") {
        FF3Cipher cipher(10, key, tweak);

        std::vector<int> max_input(32, 1);  // 32 digits (max allowed by FF3)
        auto encrypted = cipher.encrypt_digits(max_input);
        auto decrypted = cipher.decrypt_digits(encrypted);

        REQUIRE(decrypted == max_input);
        REQUIRE(encrypted.size() == max_input.size());
    }

    SECTION("Input too long throws") {
        FF3Cipher cipher(10, key, tweak);

        std::vector<int> too_long_input(100, 1);  // 100 digits (exceeds FF3 limit)
        REQUIRE_THROWS(cipher.encrypt_digits(too_long_input));
    }

    SECTION("Invalid digit values") {
        FF3Cipher cipher(10, key, tweak);

        std::vector<int> invalid_input = {1, 2, 3, 10, 5};  // 10 is out of range for radix 10
        REQUIRE_THROWS(cipher.encrypt_digits(invalid_input));

        std::vector<int> negative_input = {1, 2, -1, 4, 5};  // Negative digit
        REQUIRE_THROWS(cipher.encrypt_digits(negative_input));
    }
}