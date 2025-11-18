/**
 * @file test_alphabets.cpp
 * @brief Catch2 unit tests for FF3 alphabet and character set handling
 */

#include <catch2/catch_test_macros.hpp>
#include <ff3/alphabets.hpp>
#include <string>
#include <set>

using namespace ff3;

TEST_CASE("Alphabet factory functions", "[alphabets][factories]") {
    SECTION("Digits alphabet") {
        auto spec = create_digits_alphabet();
        REQUIRE(spec.chars() == "0123456789");
        REQUIRE(spec.radix() == 10);

        // Check all digits are unique
        std::set<char> unique_chars(spec.chars().begin(), spec.chars().end());
        REQUIRE(unique_chars.size() == spec.chars().length());
    }

    SECTION("Hex lowercase alphabet") {
        auto spec = create_hex_lower_alphabet();
        REQUIRE(spec.chars() == "0123456789abcdef");
        REQUIRE(spec.radix() == 16);

        // Check all characters are unique
        std::set<char> unique_chars(spec.chars().begin(), spec.chars().end());
        REQUIRE(unique_chars.size() == spec.chars().length());

        // Check no uppercase letters
        for (char c : spec.chars()) {
            REQUIRE((c < 'A' || c > 'Z'));
        }
    }

    SECTION("Hex uppercase alphabet") {
        auto spec = create_hex_upper_alphabet();
        REQUIRE(spec.chars() == "0123456789ABCDEF");
        REQUIRE(spec.radix() == 16);

        // Check all characters are unique
        std::set<char> unique_chars(spec.chars().begin(), spec.chars().end());
        REQUIRE(unique_chars.size() == spec.chars().length());

        // Check no lowercase letters
        for (char c : spec.chars()) {
            REQUIRE((c < 'a' || c > 'z'));
        }
    }

    SECTION("Base36 lowercase alphabet") {
        auto spec = create_base36_lower_alphabet();
        REQUIRE(spec.chars() == "0123456789abcdefghijklmnopqrstuvwxyz");
        REQUIRE(spec.radix() == 36);

        // Check all characters are unique
        std::set<char> unique_chars(spec.chars().begin(), spec.chars().end());
        REQUIRE(unique_chars.size() == spec.chars().length());

        // Check starts with digits, followed by lowercase letters
        REQUIRE(spec.chars().substr(0, 10) == "0123456789");
        REQUIRE(spec.chars().substr(10) == "abcdefghijklmnopqrstuvwxyz");
    }

    SECTION("Base36 uppercase alphabet") {
        auto spec = create_base36_upper_alphabet();
        REQUIRE(spec.chars() == "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ");
        REQUIRE(spec.radix() == 36);

        // Check all characters are unique
        std::set<char> unique_chars(spec.chars().begin(), spec.chars().end());
        REQUIRE(unique_chars.size() == spec.chars().length());

        // Check starts with digits, followed by uppercase letters
        REQUIRE(spec.chars().substr(0, 10) == "0123456789");
        REQUIRE(spec.chars().substr(10) == "ABCDEFGHIJKLMNOPQRSTUVWXYZ");
    }

    SECTION("Base62 alphabet") {
        auto spec = create_base62_alphabet();
        REQUIRE(spec.chars() == "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz");
        REQUIRE(spec.radix() == 62);

        // Check all characters are unique
        std::set<char> unique_chars(spec.chars().begin(), spec.chars().end());
        REQUIRE(unique_chars.size() == spec.chars().length());

        // Check structure: digits + uppercase + lowercase
        REQUIRE(spec.chars().substr(0, 10) == "0123456789");
        REQUIRE(spec.chars().substr(10, 26) == "ABCDEFGHIJKLMNOPQRSTUVWXYZ");
        REQUIRE(spec.chars().substr(36, 26) == "abcdefghijklmnopqrstuvwxyz");
    }
}

TEST_CASE("AlphabetSpec functionality", "[alphabets][spec]") {
    SECTION("Constructor with valid charset") {
        REQUIRE_NOTHROW(AlphabetSpec("0123456789"));
        REQUIRE_NOTHROW(AlphabetSpec("abc"));
        REQUIRE_NOTHROW(AlphabetSpec("0123456789abcdef"));
    }

    SECTION("Radix calculation") {
        AlphabetSpec digits_spec("0123456789");
        REQUIRE(digits_spec.radix() == 10);

        AlphabetSpec hex_spec("0123456789abcdef");
        REQUIRE(hex_spec.radix() == 16);

        AlphabetSpec custom_spec("ABC");
        REQUIRE(custom_spec.radix() == 3);
    }

    SECTION("Character to int conversion") {
        AlphabetSpec spec("0123456789");

        REQUIRE(spec.char_to_int('0') == 0);
        REQUIRE(spec.char_to_int('5') == 5);
        REQUIRE(spec.char_to_int('9') == 9);

        // Invalid character returns -1
        REQUIRE(spec.char_to_int('a') == -1);
        REQUIRE(spec.char_to_int('Z') == -1);
    }

    SECTION("Int to character conversion") {
        AlphabetSpec spec("0123456789abcdef");

        REQUIRE(spec.int_to_char(0) == '0');
        REQUIRE(spec.int_to_char(10) == 'a');
        REQUIRE(spec.int_to_char(15) == 'f');

        // Out of range index should throw
        REQUIRE_THROWS(spec.int_to_char(16));
        REQUIRE_THROWS(spec.int_to_char(-1));
    }

    SECTION("Character validation") {
        AlphabetSpec spec("0123456789");

        REQUIRE(spec.is_valid_char('0'));
        REQUIRE(spec.is_valid_char('5'));
        REQUIRE(spec.is_valid_char('9'));
        REQUIRE_FALSE(spec.is_valid_char('a'));
        REQUIRE_FALSE(spec.is_valid_char('A'));
        REQUIRE_FALSE(spec.is_valid_char('@'));
    }
}

TEST_CASE("AlphabetSpec edge cases", "[alphabets][edge]") {
    SECTION("Empty charset") {
        AlphabetSpec spec("");
        REQUIRE(spec.radix() == 0);
        REQUIRE(spec.chars() == "");
    }

    SECTION("Single character charset") {
        AlphabetSpec spec("A");
        REQUIRE(spec.radix() == 1);
        REQUIRE(spec.char_to_int('A') == 0);
        REQUIRE(spec.int_to_char(0) == 'A');
    }

    SECTION("Duplicate characters in charset") {
        // Should handle duplicates gracefully (implementation defined)
        REQUIRE_NOTHROW(AlphabetSpec("0011223"));
    }

    SECTION("Special characters in charset") {
        AlphabetSpec spec("!@#$%");
        REQUIRE(spec.radix() == 5);
        REQUIRE(spec.char_to_int('!') == 0);
        REQUIRE(spec.char_to_int('%') == 4);
        REQUIRE(spec.int_to_char(2) == '#');
    }

    SECTION("Unicode characters") {
        // UTF-8 encoded Greek letters - each letter is multiple bytes
        AlphabetSpec spec("αβγδε");
        // The string "αβγδε" in UTF-8 is 10 bytes (2 bytes per Greek letter)
        REQUIRE(spec.radix() == 10);
    }
}

TEST_CASE("Factory function consistency", "[alphabets][consistency]") {
    SECTION("Digits factory matches direct construction") {
        auto factory_spec = create_digits_alphabet();
        AlphabetSpec direct_spec("0123456789");

        REQUIRE(factory_spec.chars() == direct_spec.chars());
        REQUIRE(factory_spec.radix() == direct_spec.radix());
    }

    SECTION("Hex lowercase factory matches direct construction") {
        auto factory_spec = create_hex_lower_alphabet();
        AlphabetSpec direct_spec("0123456789abcdef");

        REQUIRE(factory_spec.chars() == direct_spec.chars());
        REQUIRE(factory_spec.radix() == direct_spec.radix());
    }

    SECTION("Base62 factory matches direct construction") {
        auto factory_spec = create_base62_alphabet();
        AlphabetSpec direct_spec("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz");

        REQUIRE(factory_spec.chars() == direct_spec.chars());
        REQUIRE(factory_spec.radix() == direct_spec.radix());
    }
}

TEST_CASE("Character operations", "[alphabets][operations]") {
    SECTION("Round trip char to int to char") {
        auto spec = create_digits_alphabet();

        for (size_t i = 0; i < spec.radix(); ++i) {
            char original_char = spec.int_to_char(static_cast<int>(i));
            int converted_int = spec.char_to_int(original_char);
            char round_trip_char = spec.int_to_char(converted_int);

            REQUIRE(round_trip_char == original_char);
        }
    }

    SECTION("Case sensitivity in base62") {
        auto spec = create_base62_alphabet();

        // 'A' and 'a' should have different indices
        int upper_a = spec.char_to_int('A');
        int lower_a = spec.char_to_int('a');

        REQUIRE(upper_a != lower_a);
        REQUIRE(upper_a == 10);  // 'A' is at position 10 in base62
        REQUIRE(lower_a == 36);  // 'a' is at position 36 in base62
    }

    SECTION("Invalid character handling") {
        auto spec = create_digits_alphabet();

        REQUIRE(spec.char_to_int('a') == -1);
        REQUIRE(spec.char_to_int('Z') == -1);
        REQUIRE(spec.char_to_int('@') == -1);
        REQUIRE_FALSE(spec.is_valid_char('a'));
        REQUIRE_FALSE(spec.is_valid_char('Z'));
        REQUIRE_FALSE(spec.is_valid_char('@'));
    }
}