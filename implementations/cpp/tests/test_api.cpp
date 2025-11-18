/**
 * @file test_api.cpp
 * @brief Catch2 unit tests for FF3 high-level API functionality
 */

#include <catch2/catch_test_macros.hpp>
#include <ff3/api.hpp>
#include <vector>
#include <string>

using namespace ff3;

TEST_CASE("FF3 API factory functions", "[api][factory]") {
    // Standard test key and tweak
    std::vector<uint8_t> key = {
        0xEF, 0x43, 0x59, 0xD8, 0xD5, 0x80, 0xAA, 0x4F,
        0x7F, 0x03, 0x6D, 0x6F, 0x04, 0xFC, 0x6A, 0x94
    };
    std::vector<uint8_t> tweak = {
        0xD8, 0xE7, 0x92, 0x0A, 0xFA, 0x33, 0x0A, 0x73
    };

    SECTION("Digits factory function") {
        REQUIRE_NOTHROW(digits(key, tweak));

        auto cipher = digits(key, tweak);
        std::string plaintext = "1234567890";
        auto encrypted = cipher.encrypt(plaintext);
        auto decrypted = cipher.decrypt(encrypted);

        REQUIRE(decrypted == plaintext);
        REQUIRE(encrypted != plaintext);
        REQUIRE(encrypted.length() == plaintext.length());
    }

    SECTION("Hex lowercase factory function") {
        REQUIRE_NOTHROW(hex_lower(key, tweak));

        auto cipher = hex_lower(key, tweak);
        std::string plaintext = "abc123def456";
        auto encrypted = cipher.encrypt(plaintext);
        auto decrypted = cipher.decrypt(encrypted);

        REQUIRE(decrypted == plaintext);
        REQUIRE(encrypted != plaintext);
        REQUIRE(encrypted.length() == plaintext.length());
    }

    SECTION("Hex uppercase factory function") {
        REQUIRE_NOTHROW(hex_upper(key, tweak));

        auto cipher = hex_upper(key, tweak);
        std::string plaintext = "ABC123DEF456";
        auto encrypted = cipher.encrypt(plaintext);
        auto decrypted = cipher.decrypt(encrypted);

        REQUIRE(decrypted == plaintext);
        REQUIRE(encrypted != plaintext);
        REQUIRE(encrypted.length() == plaintext.length());
    }

    SECTION("Base36 lowercase factory function") {
        REQUIRE_NOTHROW(base36_lower(key, tweak));

        auto cipher = base36_lower(key, tweak);
        std::string plaintext = "abc123xyz789";
        auto encrypted = cipher.encrypt(plaintext);
        auto decrypted = cipher.decrypt(encrypted);

        REQUIRE(decrypted == plaintext);
        REQUIRE(encrypted != plaintext);
        REQUIRE(encrypted.length() == plaintext.length());
    }

    SECTION("Base36 uppercase factory function") {
        REQUIRE_NOTHROW(base36_upper(key, tweak));

        auto cipher = base36_upper(key, tweak);
        std::string plaintext = "ABC123XYZ789";
        auto encrypted = cipher.encrypt(plaintext);
        auto decrypted = cipher.decrypt(encrypted);

        REQUIRE(decrypted == plaintext);
        REQUIRE(encrypted != plaintext);
        REQUIRE(encrypted.length() == plaintext.length());
    }

    SECTION("Base62 factory function") {
        REQUIRE_NOTHROW(base62(key, tweak));

        auto cipher = base62(key, tweak);
        std::string plaintext = "AbC123XyZ789";
        auto encrypted = cipher.encrypt(plaintext);
        auto decrypted = cipher.decrypt(encrypted);

        REQUIRE(decrypted == plaintext);
        REQUIRE(encrypted != plaintext);
        REQUIRE(encrypted.length() == plaintext.length());
    }
}

TEST_CASE("FF3 API string validation", "[api][validation]") {
    std::vector<uint8_t> key = {
        0xEF, 0x43, 0x59, 0xD8, 0xD5, 0x80, 0xAA, 0x4F,
        0x7F, 0x03, 0x6D, 0x6F, 0x04, 0xFC, 0x6A, 0x94
    };
    std::vector<uint8_t> tweak = {
        0xD8, 0xE7, 0x92, 0x0A, 0xFA, 0x33, 0x0A, 0x73
    };

    SECTION("Digits cipher validates input") {
        auto cipher = digits(key, tweak);

        REQUIRE_NOTHROW(cipher.encrypt("1234567890"));
        REQUIRE_THROWS(cipher.encrypt("123abc"));  // Invalid characters
        REQUIRE_THROWS(cipher.encrypt(""));        // Empty string
        REQUIRE_THROWS(cipher.encrypt("1"));       // Too short
    }

    SECTION("Hex cipher validates input") {
        auto cipher = hex_lower(key, tweak);

        REQUIRE_NOTHROW(cipher.encrypt("abc123def456"));
        REQUIRE_THROWS(cipher.encrypt("xyz123"));  // Invalid characters
        REQUIRE_THROWS(cipher.encrypt("ABC123"));  // Wrong case
    }

    SECTION("Base62 cipher validates input") {
        auto cipher = base62(key, tweak);

        REQUIRE_NOTHROW(cipher.encrypt("AbC123XyZ789"));
        REQUIRE_THROWS(cipher.encrypt("@#$%"));    // Invalid characters
    }
}

TEST_CASE("FF3 API format preservation", "[api][format]") {
    std::vector<uint8_t> key = {
        0xEF, 0x43, 0x59, 0xD8, 0xD5, 0x80, 0xAA, 0x4F,
        0x7F, 0x03, 0x6D, 0x6F, 0x04, 0xFC, 0x6A, 0x94
    };
    std::vector<uint8_t> tweak = {
        0xD8, 0xE7, 0x92, 0x0A, 0xFA, 0x33, 0x0A, 0x73
    };

    SECTION("Preserves string length") {
        auto cipher = digits(key, tweak);

        std::string input = "1234567890123456";
        auto encrypted = cipher.encrypt(input);

        REQUIRE(encrypted.length() == input.length());
    }

    SECTION("Preserves character set") {
        auto cipher = hex_lower(key, tweak);

        std::string input = "abcdef123456";
        auto encrypted = cipher.encrypt(input);

        // All characters should be valid hex lowercase
        for (char c : encrypted) {
            REQUIRE(((c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')));
        }
    }

    SECTION("Different inputs produce different outputs") {
        auto cipher = digits(key, tweak);

        std::string input1 = "1234567890";
        std::string input2 = "0987654321";

        auto encrypted1 = cipher.encrypt(input1);
        auto encrypted2 = cipher.encrypt(input2);

        REQUIRE(encrypted1 != encrypted2);
    }

    SECTION("Case sensitivity is preserved") {
        auto base62_cipher = base62(key, tweak);

        std::string upper_input = "ABCDEFG123";
        std::string lower_input = "abcdefg123";

        auto upper_encrypted = base62_cipher.encrypt(upper_input);
        auto lower_encrypted = base62_cipher.encrypt(lower_input);

        REQUIRE(upper_encrypted != lower_encrypted);

        auto upper_decrypted = base62_cipher.decrypt(upper_encrypted);
        auto lower_decrypted = base62_cipher.decrypt(lower_encrypted);

        REQUIRE(upper_decrypted == upper_input);
        REQUIRE(lower_decrypted == lower_input);
    }
}

TEST_CASE("FF3 API roundtrip consistency", "[api][roundtrip]") {
    std::vector<uint8_t> key = {
        0xEF, 0x43, 0x59, 0xD8, 0xD5, 0x80, 0xAA, 0x4F,
        0x7F, 0x03, 0x6D, 0x6F, 0x04, 0xFC, 0x6A, 0x94
    };
    std::vector<uint8_t> tweak = {
        0xD8, 0xE7, 0x92, 0x0A, 0xFA, 0x33, 0x0A, 0x73
    };

    SECTION("Multiple roundtrips with digits") {
        auto cipher = digits(key, tweak);
        std::string original = "1234567890123456";

        std::string current = original;
        for (int i = 0; i < 10; i++) {
            auto encrypted = cipher.encrypt(current);
            current = cipher.decrypt(encrypted);
        }

        REQUIRE(current == original);
    }

    SECTION("Multiple roundtrips with base62") {
        auto cipher = base62(key, tweak);
        std::string original = "AbC123XyZ789PqR456";

        std::string current = original;
        for (int i = 0; i < 5; i++) {
            auto encrypted = cipher.encrypt(current);
            current = cipher.decrypt(encrypted);
        }

        REQUIRE(current == original);
    }

    SECTION("Deterministic encryption") {
        auto cipher = digits(key, tweak);
        std::string input = "1234567890";

        auto encrypted1 = cipher.encrypt(input);
        auto encrypted2 = cipher.encrypt(input);
        auto encrypted3 = cipher.encrypt(input);

        REQUIRE(encrypted1 == encrypted2);
        REQUIRE(encrypted2 == encrypted3);
    }
}

TEST_CASE("FF3 API error handling", "[api][error]") {
    std::vector<uint8_t> key = {
        0xEF, 0x43, 0x59, 0xD8, 0xD5, 0x80, 0xAA, 0x4F,
        0x7F, 0x03, 0x6D, 0x6F, 0x04, 0xFC, 0x6A, 0x94
    };
    std::vector<uint8_t> tweak = {
        0xD8, 0xE7, 0x92, 0x0A, 0xFA, 0x33, 0x0A, 0x73
    };

    SECTION("Invalid key length") {
        std::vector<uint8_t> short_key = {0x01, 0x02, 0x03};
        REQUIRE_THROWS(digits(short_key, tweak));
    }

    SECTION("Invalid tweak length") {
        std::vector<uint8_t> short_tweak = {0x01, 0x02};
        REQUIRE_THROWS(digits(key, short_tweak));
    }

    SECTION("Empty string encryption") {
        auto cipher = digits(key, tweak);
        REQUIRE_THROWS(cipher.encrypt(""));
    }

    SECTION("Too short string encryption") {
        auto cipher = digits(key, tweak);
        REQUIRE_THROWS(cipher.encrypt("1"));
    }

    SECTION("Invalid characters for alphabet") {
        auto cipher = digits(key, tweak);
        REQUIRE_THROWS(cipher.encrypt("123abc456"));  // Contains letters

        auto hex_cipher = hex_lower(key, tweak);
        REQUIRE_THROWS(hex_cipher.encrypt("123xyz456"));  // Contains invalid hex chars
    }

    SECTION("Decrypt invalid ciphertext") {
        auto cipher = digits(key, tweak);
        REQUIRE_THROWS(cipher.decrypt("abc123"));  // Invalid for digits cipher
    }
}