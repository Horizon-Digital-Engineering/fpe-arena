/**
 * @file test_nist.cpp
 * @brief Catch2 unit tests for NIST FF3 test vector validation
 */

#include <catch2/catch_test_macros.hpp>
#include <ff3/api.hpp>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>

using namespace ff3;

struct NISTVector {
    int sample;
    std::string algorithm;
    std::string key;
    int radix;
    std::string alphabet;
    std::string plaintext;
    std::string tweak;
    std::string ciphertext;
};

std::vector<NISTVector> load_nist_vectors() {
    std::string filepath;

    // Try environment variable first (for CI/CD)
    const char* env_path = std::getenv("FF3_TEST_VECTORS_PATH");
    if (env_path) {
        filepath = env_path;
    } else {
        // Fallback to cmake-defined directory for local development
        std::string test_data_dir = FF3_TEST_DATA_DIR;
        filepath = test_data_dir + "/nist_ff3_official_vectors.json";
    }

    std::ifstream file(filepath);
    if (!file.is_open()) {
        throw std::runtime_error("Cannot open NIST test vectors file: " + filepath);
    }

    // Simple JSON parsing for test vectors (similar to original implementation)
    std::vector<NISTVector> vectors;
    std::string line;
    NISTVector current;

    while (std::getline(file, line)) {
        if (line.find("\"sample\":") != std::string::npos) {
            size_t pos = line.find(":") + 1;
            std::string value = line.substr(pos);
            // Remove comma if present
            size_t comma = value.find(",");
            if (comma != std::string::npos) {
                value = value.substr(0, comma);
            }
            current.sample = std::stoi(value);
        } else if (line.find("\"algorithm\":") != std::string::npos) {
            size_t start = line.find("\"", line.find(":")) + 1;
            size_t end = line.find("\"", start);
            current.algorithm = line.substr(start, end - start);
        } else if (line.find("\"key\":") != std::string::npos) {
            size_t start = line.find("\"", line.find(":")) + 1;
            size_t end = line.find("\"", start);
            current.key = line.substr(start, end - start);
        } else if (line.find("\"radix\":") != std::string::npos) {
            size_t pos = line.find(":") + 1;
            std::string value = line.substr(pos);
            size_t comma = value.find(",");
            if (comma != std::string::npos) {
                value = value.substr(0, comma);
            }
            current.radix = std::stoi(value);
        } else if (line.find("\"alphabet\":") != std::string::npos) {
            size_t start = line.find("\"", line.find(":")) + 1;
            size_t end = line.find("\"", start);
            current.alphabet = line.substr(start, end - start);
        } else if (line.find("\"plaintext\":") != std::string::npos) {
            size_t start = line.find("\"", line.find(":")) + 1;
            size_t end = line.find("\"", start);
            current.plaintext = line.substr(start, end - start);
        } else if (line.find("\"tweak\":") != std::string::npos) {
            size_t start = line.find("\"", line.find(":")) + 1;
            size_t end = line.find("\"", start);
            current.tweak = line.substr(start, end - start);
        } else if (line.find("\"ciphertext\":") != std::string::npos) {
            size_t start = line.find("\"", line.find(":")) + 1;
            size_t end = line.find("\"", start);
            current.ciphertext = line.substr(start, end - start);
            vectors.push_back(current);
            current = NISTVector();  // Reset for next vector
        }
    }

    return vectors;
}

// Use the ff3::hex_to_bytes function instead of defining our own

FF3 create_cipher_for_vector(const NISTVector& vec) {
    auto key = ff3::hex_to_bytes(vec.key);
    auto tweak = ff3::hex_to_bytes(vec.tweak);

    switch (vec.radix) {
        case 10:
            return digits(key, tweak);
        case 26:
            // NIST vectors use specific alphabet for radix 26
            return from_spec(key, tweak, AlphabetSpec(vec.alphabet));
        default:
            throw std::runtime_error("Unsupported radix: " + std::to_string(vec.radix));
    }
}

TEST_CASE("NIST FF3 test vectors - individual cases", "[nist][vectors]") {
    auto vectors = load_nist_vectors();

    SECTION("Sample 1 - AES128, Radix 10") {
        auto vec = vectors[0];  // Sample 1
        REQUIRE(vec.sample == 1);
        REQUIRE(vec.radix == 10);

        auto cipher = create_cipher_for_vector(vec);
        auto encrypted = cipher.encrypt(vec.plaintext);
        auto decrypted = cipher.decrypt(encrypted);

        REQUIRE(encrypted == vec.ciphertext);
        REQUIRE(decrypted == vec.plaintext);
    }

    SECTION("Sample 5 - AES128, Radix 26") {
        auto vec = vectors[4];  // Sample 5
        REQUIRE(vec.sample == 5);
        REQUIRE(vec.radix == 26);

        auto cipher = create_cipher_for_vector(vec);
        auto encrypted = cipher.encrypt(vec.plaintext);
        auto decrypted = cipher.decrypt(encrypted);

        REQUIRE(encrypted == vec.ciphertext);
        REQUIRE(decrypted == vec.plaintext);
    }

    SECTION("Sample 11 - AES256, Radix 10") {
        auto vec = vectors[10];  // Sample 11
        REQUIRE(vec.sample == 11);
        REQUIRE(vec.radix == 10);

        auto cipher = create_cipher_for_vector(vec);
        auto encrypted = cipher.encrypt(vec.plaintext);
        auto decrypted = cipher.decrypt(encrypted);

        REQUIRE(encrypted == vec.ciphertext);
        REQUIRE(decrypted == vec.plaintext);
    }
}

TEST_CASE("NIST FF3 test vectors - all vectors", "[nist][comprehensive]") {
    auto vectors = load_nist_vectors();

    REQUIRE(vectors.size() == 15);

    int passed = 0;
    std::vector<std::string> failures;

    for (const auto& vec : vectors) {
        // Only test supported radixes
        if (vec.radix != 10 && vec.radix != 26) {
            continue;
        }

        try {
            auto cipher = create_cipher_for_vector(vec);
            auto encrypted = cipher.encrypt(vec.plaintext);
            auto decrypted = cipher.decrypt(encrypted);

            if (encrypted == vec.ciphertext && decrypted == vec.plaintext) {
                passed++;
            } else {
                failures.push_back("Sample " + std::to_string(vec.sample) +
                                 " - Expected: " + vec.ciphertext +
                                 ", Got: " + encrypted);
            }
        } catch (const std::exception& e) {
            failures.push_back("Sample " + std::to_string(vec.sample) +
                             " - Exception: " + e.what());
        }
    }

    // Report any failures
    if (!failures.empty()) {
        std::string failure_msg = "NIST vector failures:\n";
        for (const auto& failure : failures) {
            failure_msg += "  " + failure + "\n";
        }
        FAIL(failure_msg);
    }

    // We expect all supported vectors to pass
    REQUIRE(passed >= 10);  // At least 10 vectors should pass (all radix 10 + some radix 26)
}

TEST_CASE("NIST FF3 vectors by key length", "[nist][keylength]") {
    auto vectors = load_nist_vectors();

    SECTION("AES-128 vectors") {
        int aes128_passed = 0;
        for (const auto& vec : vectors) {
            if (vec.algorithm == "FF3-AES128" && (vec.radix == 10 || vec.radix == 26)) {
                auto cipher = create_cipher_for_vector(vec);
                auto encrypted = cipher.encrypt(vec.plaintext);
                auto decrypted = cipher.decrypt(encrypted);

                REQUIRE(encrypted == vec.ciphertext);
                REQUIRE(decrypted == vec.plaintext);
                aes128_passed++;
            }
        }
        REQUIRE(aes128_passed >= 3);  // Should have at least 3 AES-128 vectors
    }

    SECTION("AES-192 vectors") {
        int aes192_passed = 0;
        for (const auto& vec : vectors) {
            if (vec.algorithm == "FF3-AES192" && (vec.radix == 10 || vec.radix == 26)) {
                auto cipher = create_cipher_for_vector(vec);
                auto encrypted = cipher.encrypt(vec.plaintext);
                auto decrypted = cipher.decrypt(encrypted);

                REQUIRE(encrypted == vec.ciphertext);
                REQUIRE(decrypted == vec.plaintext);
                aes192_passed++;
            }
        }
        REQUIRE(aes192_passed >= 3);  // Should have at least 3 AES-192 vectors
    }

    SECTION("AES-256 vectors") {
        int aes256_passed = 0;
        for (const auto& vec : vectors) {
            if (vec.algorithm == "FF3-AES256" && (vec.radix == 10 || vec.radix == 26)) {
                auto cipher = create_cipher_for_vector(vec);
                auto encrypted = cipher.encrypt(vec.plaintext);
                auto decrypted = cipher.decrypt(encrypted);

                REQUIRE(encrypted == vec.ciphertext);
                REQUIRE(decrypted == vec.plaintext);
                aes256_passed++;
            }
        }
        REQUIRE(aes256_passed >= 3);  // Should have at least 3 AES-256 vectors
    }
}

TEST_CASE("NIST FF3 vector properties", "[nist][properties]") {
    auto vectors = load_nist_vectors();

    SECTION("All vectors have required fields") {
        for (const auto& vec : vectors) {
            REQUIRE(vec.sample > 0);
            REQUIRE_FALSE(vec.algorithm.empty());
            REQUIRE_FALSE(vec.key.empty());
            REQUIRE(vec.radix >= 2);
            REQUIRE_FALSE(vec.alphabet.empty());
            REQUIRE_FALSE(vec.plaintext.empty());
            REQUIRE_FALSE(vec.tweak.empty());
            REQUIRE_FALSE(vec.ciphertext.empty());
        }
    }

    SECTION("Key lengths are valid") {
        for (const auto& vec : vectors) {
            auto key = ff3::hex_to_bytes(vec.key);
            // Should be 16, 24, or 32 bytes for AES-128, AES-192, AES-256
            REQUIRE((key.size() == 16 || key.size() == 24 || key.size() == 32));
        }
    }

    SECTION("Tweak lengths are valid") {
        for (const auto& vec : vectors) {
            auto tweak = ff3::hex_to_bytes(vec.tweak);
            REQUIRE(tweak.size() == 8);  // FF3 always uses 64-bit (8-byte) tweaks
        }
    }

    SECTION("Plaintext and ciphertext have same length") {
        for (const auto& vec : vectors) {
            REQUIRE(vec.plaintext.length() == vec.ciphertext.length());
        }
    }

    SECTION("Radix values match alphabet size") {
        for (const auto& vec : vectors) {
            REQUIRE(vec.radix == static_cast<int>(vec.alphabet.length()));
        }
    }
}

TEST_CASE("NIST FF3 vector consistency", "[nist][consistency]") {
    auto vectors = load_nist_vectors();

    SECTION("Multiple encryptions of same plaintext are deterministic") {
        for (const auto& vec : vectors) {
            if (vec.radix != 10 && vec.radix != 26) continue;

            auto cipher = create_cipher_for_vector(vec);

            auto encrypted1 = cipher.encrypt(vec.plaintext);
            auto encrypted2 = cipher.encrypt(vec.plaintext);
            auto encrypted3 = cipher.encrypt(vec.plaintext);

            REQUIRE(encrypted1 == encrypted2);
            REQUIRE(encrypted2 == encrypted3);
            REQUIRE(encrypted1 == vec.ciphertext);
        }
    }

    SECTION("Different plaintexts produce different ciphertexts") {
        // Find vectors with same parameters but different plaintexts
        for (size_t i = 0; i < vectors.size(); i++) {
            for (size_t j = i + 1; j < vectors.size(); j++) {
                const auto& vec1 = vectors[i];
                const auto& vec2 = vectors[j];

                // Same key, tweak, radix but different plaintext
                if (vec1.key == vec2.key &&
                    vec1.tweak == vec2.tweak &&
                    vec1.radix == vec2.radix &&
                    vec1.plaintext != vec2.plaintext &&
                    (vec1.radix == 10 || vec1.radix == 26)) {

                    REQUIRE(vec1.ciphertext != vec2.ciphertext);
                }
            }
        }
    }
}