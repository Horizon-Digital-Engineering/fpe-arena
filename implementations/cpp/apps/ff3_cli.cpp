/**
 * @file ff3_cli.cpp
 * @brief FF3 CLI - Command-line tool for FF3 encryption/decryption
 */

#include <ff3.hpp>
#include <iostream>
#include <string>
#include <vector>
#include <iomanip>
#include <cstring>

using namespace ff3;

void print_usage(const char* program_name) {
    std::cout << "FF3 CLI - Format Preserving Encryption\n\n";
    std::cout << "Usage: " << program_name << " [OPTIONS]\n\n";
    std::cout << "Options:\n";
    std::cout << "  -e, --encrypt TEXT      Encrypt the given text\n";
    std::cout << "  -d, --decrypt TEXT      Decrypt the given text\n";
    std::cout << "  -k, --key HEX           AES key in hex format (32/48/64 hex chars)\n";
    std::cout << "  -t, --tweak HEX         Tweak in hex format (16 hex chars)\n";
    std::cout << "  -a, --alphabet TYPE     Alphabet type:\n";
    std::cout << "                            digits (default)\n";
    std::cout << "                            hex-lower\n";
    std::cout << "                            hex-upper\n";
    std::cout << "                            base36-lower\n";
    std::cout << "                            base36-upper\n";
    std::cout << "                            base62\n";
    std::cout << "  -c, --custom CHARSET    Custom alphabet charset\n";
    std::cout << "  -h, --help              Show this help message\n\n";
    std::cout << "Examples:\n";
    std::cout << "  " << program_name << " -e \"1234567890\" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73\n";
    std::cout << "  " << program_name << " -d \"7501889140\" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73\n\n";
}

FF3 create_cipher(const std::vector<uint8_t>& key, const std::vector<uint8_t>& tweak,
                  const std::string& alphabet_type, const std::string& custom_charset = "") {
    if (!custom_charset.empty()) {
        return from_spec(key, tweak, AlphabetSpec(custom_charset));
    }

    if (alphabet_type == "digits") {
        return digits(key, tweak);
    } else if (alphabet_type == "hex-lower") {
        return hex_lower(key, tweak);
    } else if (alphabet_type == "hex-upper") {
        return hex_upper(key, tweak);
    } else if (alphabet_type == "base36-lower") {
        return base36_lower(key, tweak);
    } else if (alphabet_type == "base36-upper") {
        return base36_upper(key, tweak);
    } else if (alphabet_type == "base62") {
        return base62(key, tweak);
    } else {
        throw std::invalid_argument("Unknown alphabet type: " + alphabet_type);
    }
}

int main(int argc, char* argv[]) {
    std::string encrypt_text, decrypt_text, key_hex, tweak_hex;
    std::string alphabet_type = "digits", custom_charset;

    // Parse arguments
    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0) {
            print_usage(argv[0]);
            return 0;
        } else if ((strcmp(argv[i], "-e") == 0 || strcmp(argv[i], "--encrypt") == 0) && i + 1 < argc) {
            encrypt_text = argv[++i];
        } else if ((strcmp(argv[i], "-d") == 0 || strcmp(argv[i], "--decrypt") == 0) && i + 1 < argc) {
            decrypt_text = argv[++i];
        } else if ((strcmp(argv[i], "-k") == 0 || strcmp(argv[i], "--key") == 0) && i + 1 < argc) {
            key_hex = argv[++i];
        } else if ((strcmp(argv[i], "-t") == 0 || strcmp(argv[i], "--tweak") == 0) && i + 1 < argc) {
            tweak_hex = argv[++i];
        } else if ((strcmp(argv[i], "-a") == 0 || strcmp(argv[i], "--alphabet") == 0) && i + 1 < argc) {
            alphabet_type = argv[++i];
        } else if ((strcmp(argv[i], "-c") == 0 || strcmp(argv[i], "--custom") == 0) && i + 1 < argc) {
            custom_charset = argv[++i];
        } else {
            std::cerr << "Unknown argument: " << argv[i] << std::endl;
            print_usage(argv[0]);
            return 1;
        }
    }

    if (encrypt_text.empty() && decrypt_text.empty()) {
        print_usage(argv[0]);
        return 1;
    }

    if (key_hex.empty() || tweak_hex.empty()) {
        std::cerr << "Error: Key and tweak are required" << std::endl;
        return 1;
    }

    try {
        // Parse key and tweak
        auto key = hex_to_bytes(key_hex);
        auto tweak = hex_to_bytes(tweak_hex);

        if (key.size() != 16 && key.size() != 24 && key.size() != 32) {
            std::cerr << "Error: Key must be 16, 24, or 32 bytes (32, 48, or 64 hex chars)" << std::endl;
            return 1;
        }

        if (tweak.size() != 8) {
            std::cerr << "Error: Tweak must be 8 bytes (16 hex chars)" << std::endl;
            return 1;
        }

        // Create cipher
        auto cipher = create_cipher(key, tweak, alphabet_type, custom_charset);

        // Encrypt or decrypt
        if (!encrypt_text.empty()) {
            auto result = cipher.encrypt(encrypt_text);
            std::cout << result << std::endl;
        } else if (!decrypt_text.empty()) {
            auto result = cipher.decrypt(decrypt_text);
            std::cout << result << std::endl;
        }

    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << std::endl;
        return 1;
    }

    return 0;
}