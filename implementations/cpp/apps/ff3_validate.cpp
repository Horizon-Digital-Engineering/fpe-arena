/**
 * @file ff3_validate.cpp
 * @brief FF3 NIST Test Vector Validation Tool
 */

#include <ff3.hpp>
#include <iostream>
#include <fstream>
#include <cstring>
#include <filesystem>

using namespace ff3;

struct Options {
    std::string vectors_path;
    bool verbose = false;
    bool quiet = false;
    bool help = false;
};

struct TestVector {
    int sample;
    std::string algorithm;
    std::string key;
    int radix;
    std::string plaintext;
    std::string tweak;
    std::string ciphertext;
};

void show_usage() {
    std::cout << "FF3 NIST Test Vector Validation Tool\n\n";
    std::cout << "Usage: ff3-validate [OPTIONS]\n\n";
    std::cout << "Options:\n";
    std::cout << "  --vectors PATH    Path to test vectors JSON file\n";
    std::cout << "  --verbose         Show detailed test output\n";
    std::cout << "  --quiet           Only show failures and summary\n";
    std::cout << "  -h, --help        Show this help message\n\n";
}

Options parse_args(int argc, char* argv[]) {
    Options opts;

    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0) {
            opts.help = true;
            return opts;
        } else if (strcmp(argv[i], "--vectors") == 0) {
            if (i + 1 >= argc) {
                throw std::runtime_error("Missing value for --vectors");
            }
            opts.vectors_path = argv[++i];
        } else if (strcmp(argv[i], "--verbose") == 0) {
            opts.verbose = true;
        } else if (strcmp(argv[i], "--quiet") == 0) {
            opts.quiet = true;
        } else {
            throw std::runtime_error(std::string("Unknown option: ") + argv[i]);
        }
    }

    return opts;
}

std::string find_vectors_file(const std::string& custom_path) {
    if (!custom_path.empty()) {
        if (std::filesystem::exists(custom_path)) {
            return custom_path;
        }
        throw std::runtime_error("Vectors file not found: " + custom_path);
    }

    std::vector<std::string> paths = {
        "../../shared/test-vectors/nist_ff3_official_vectors.json",
        "../../../shared/test-vectors/nist_ff3_official_vectors.json",
        "./nist_ff3_official_vectors.json"
    };

    for (const auto& path : paths) {
        if (std::filesystem::exists(path)) {
            return path;
        }
    }

    throw std::runtime_error("Could not find NIST test vectors file");
}

std::vector<TestVector> load_vectors(const std::string& filepath) {
    std::ifstream file(filepath);
    if (!file.is_open()) {
        throw std::runtime_error("Cannot open vectors file: " + filepath);
    }

    std::vector<TestVector> vectors;
    std::string line;
    TestVector current;

    while (std::getline(file, line)) {
        if (line.find("\"sample\":") != std::string::npos) {
            size_t pos = line.find(":") + 1;
            current.sample = std::stoi(line.substr(pos));
        } else if (line.find("\"key\":") != std::string::npos) {
            size_t start = line.find("\"", line.find(":")) + 1;
            size_t end = line.find("\"", start);
            current.key = line.substr(start, end - start);
        } else if (line.find("\"radix\":") != std::string::npos) {
            size_t pos = line.find(":") + 1;
            current.radix = std::stoi(line.substr(pos));
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
        }
    }

    return vectors;
}

int main(int argc, char* argv[]) {
    try {
        Options opts = parse_args(argc, argv);

        if (opts.help) {
            show_usage();
            return 0;
        }

        if (!opts.quiet) {
            std::cout << "FF3 NIST Test Vector Validation Tool\n";
            std::cout << "========================================\n\n";
        }

        std::string vectors_path = find_vectors_file(opts.vectors_path);

        if (!opts.quiet) {
            std::cout << "Vector file: " << vectors_path << "\n\n";
        }

        auto vectors = load_vectors(vectors_path);

        if (!opts.quiet) {
            std::cout << "Testing " << vectors.size() << " NIST FF3 vectors...\n\n";
        }

        int passed = 0;
        int total = 0;

        for (const auto& vec : vectors) {
            // Only test radix 10 and 26 (skip unsupported radixes)
            if (vec.radix != 10 && vec.radix != 26) {
                continue;
            }

            total++;

            try {
                const auto key = hex_to_bytes(vec.key);
                const auto tweak = hex_to_bytes(vec.tweak);

                FF3 cipher = (vec.radix == 10) ?
                    digits(key, tweak) :
                    radix26(key, tweak);

                const auto encrypted = cipher.encrypt(vec.plaintext);
                const auto decrypted = cipher.decrypt(encrypted);

                bool encrypt_passed = (encrypted == vec.ciphertext);
                bool roundtrip_passed = (decrypted == vec.plaintext);
                bool test_passed = encrypt_passed && roundtrip_passed;

                if (test_passed) {
                    passed++;
                    if (opts.verbose) {
                        std::cout << "Sample " << vec.sample << ": PASS\n";
                    }
                } else {
                    if (!opts.quiet) {
                        std::cout << "Sample " << vec.sample << ": FAIL\n";
                        if (!encrypt_passed) {
                            std::cout << "  Expected: " << vec.ciphertext << "\n";
                            std::cout << "  Got:      " << encrypted << "\n";
                        }
                        if (!roundtrip_passed) {
                            std::cout << "  Round-trip failed\n";
                        }
                    }
                }
            } catch (const std::exception& e) {
                if (!opts.quiet) {
                    std::cout << "Sample " << vec.sample << ": ERROR - " << e.what() << "\n";
                }
            }
        }

        if (!opts.quiet) {
            std::cout << "\n========================================\n";
            std::cout << "Results: " << passed << "/" << total << " passed\n";

            if (passed == total) {
                std::cout << "\nALL NIST TEST VECTORS PASSED!\n\n";
                std::cout << "WARNING: FF3 was WITHDRAWN by NIST due to security vulnerabilities.\n";
                std::cout << "This implementation is for EDUCATIONAL and RESEARCH purposes only.\n";
                std::cout << "DO NOT use in production systems.\n\n";
            } else {
                std::cout << "\nVALIDATION FAILED\n";
            }
        }

        return (passed == total) ? 0 : 1;

    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << "\n";
        return 2;
    }
}
