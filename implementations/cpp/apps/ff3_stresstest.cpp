/**
 * @file ff3_stresstest.cpp
 * @brief FF3 Stress Test Tool - Randomized testing for stability
 */

#include <ff3.hpp>

#include <chrono>
#include <cstdint>
#include <functional>
#include <iomanip>
#include <iostream>
#include <map>
#include <random>
#include <sstream>
#include <string>
#include <vector>

using namespace ff3;

namespace {

struct AlphabetInfo {
    std::string name;
    std::string charset;
    std::function<FF3(const std::vector<uint8_t>&, const std::vector<uint8_t>&)> factory;
};

std::string join(const std::vector<std::string>& items, const std::string& sep) {
    std::ostringstream oss;
    for (size_t i = 0; i < items.size(); ++i) {
        if (i > 0) {
            oss << sep;
        }
        oss << items[i];
    }
    return oss.str();
}

std::string to_hex(const std::vector<uint8_t>& bytes) {
    std::ostringstream oss;
    oss << std::hex << std::setfill('0');
    for (auto b : bytes) {
        oss << std::setw(2) << static_cast<int>(b);
    }
    return oss.str();
}

void print_usage(const char* program) {
    std::cout << "FF3 Stress Test Tool\n\n";
    std::cout << "Usage: " << program << " [OPTIONS] [ITERATIONS]\n\n";
    std::cout << "Arguments:\n";
    std::cout << "  ITERATIONS            Number of iterations per alphabet (default: 1000)\n\n";
    std::cout << "Options:\n";
    std::cout << "  --alphabets LIST      Comma-separated list (default: digits,hex-lower,base36-lower,base62)\n";
    std::cout << "  --min-length N        Minimum plaintext length (default: 6)\n";
    std::cout << "  --max-length N        Maximum plaintext length (default: 20)\n";
    std::cout << "  --quick               Run 100 iterations (fast test)\n";
    std::cout << "  --seed N              Random seed for reproducibility\n";
    std::cout << "  -h, --help            Show this help message\n\n";
    std::cout << "Examples:\n";
    std::cout << "  " << program << "                # Run default stress test\n";
    std::cout << "  " << program << " 5000           # 5000 iterations per alphabet\n";
    std::cout << "  " << program << " --quick        # Fast run (100 iterations)\n";
    std::cout << "  " << program << " --alphabets digits,hex-lower --min-length 8 --max-length 16\n\n";
}

std::map<std::string, AlphabetInfo> build_alphabet_map() {
    return {
        {"digits", {"digits", "0123456789",
            [](const std::vector<uint8_t>& key, const std::vector<uint8_t>& tweak) { return digits(key, tweak); }}},
        {"hex-lower", {"hex-lower", "0123456789abcdef",
            [](const std::vector<uint8_t>& key, const std::vector<uint8_t>& tweak) { return hex_lower(key, tweak); }}},
        {"hex-upper", {"hex-upper", "0123456789ABCDEF",
            [](const std::vector<uint8_t>& key, const std::vector<uint8_t>& tweak) { return hex_upper(key, tweak); }}},
        {"base36-lower", {"base36-lower", "0123456789abcdefghijklmnopqrstuvwxyz",
            [](const std::vector<uint8_t>& key, const std::vector<uint8_t>& tweak) { return base36_lower(key, tweak); }}},
        {"base36-upper", {"base36-upper", "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ",
            [](const std::vector<uint8_t>& key, const std::vector<uint8_t>& tweak) { return base36_upper(key, tweak); }}},
        {"base62", {"base62", "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz",
            [](const std::vector<uint8_t>& key, const std::vector<uint8_t>& tweak) { return base62(key, tweak); }}}
    };
}

struct Options {
    size_t iterations = 1000;
    size_t min_length = 6;
    size_t max_length = 20;
    bool quick = false;
    bool help = false;
    bool seed_provided = false;
    uint64_t seed = 0;
    std::vector<std::string> alphabets = {"digits", "hex-lower", "base36-lower", "base62"};
};

bool parse_arguments(int argc, char* argv[], Options& opts) {
    bool positional_iterations_set = false;

    for (int i = 1; i < argc; ++i) {
        std::string arg = argv[i];

        if (arg == "-h" || arg == "--help") {
            opts.help = true;
            return true;
        } else if (arg == "--alphabets") {
            if (i + 1 >= argc) {
                std::cerr << "Error: --alphabets requires a value." << std::endl;
                return false;
            }
            std::string list = argv[++i];
            opts.alphabets.clear();
            std::stringstream ss(list);
            std::string item;
            while (std::getline(ss, item, ',')) {
                if (!item.empty()) {
                    opts.alphabets.push_back(item);
                }
            }
            if (opts.alphabets.empty()) {
                std::cerr << "Error: --alphabets list cannot be empty." << std::endl;
                return false;
            }
        } else if (arg == "--min-length") {
            if (i + 1 >= argc) {
                std::cerr << "Error: --min-length requires a value." << std::endl;
                return false;
            }
            opts.min_length = static_cast<size_t>(std::stoul(argv[++i]));
        } else if (arg == "--max-length") {
            if (i + 1 >= argc) {
                std::cerr << "Error: --max-length requires a value." << std::endl;
                return false;
            }
            opts.max_length = static_cast<size_t>(std::stoul(argv[++i]));
        } else if (arg == "--quick") {
            opts.quick = true;
        } else if (arg == "--seed") {
            if (i + 1 >= argc) {
                std::cerr << "Error: --seed requires a value." << std::endl;
                return false;
            }
            opts.seed = std::stoull(argv[++i]);
            opts.seed_provided = true;
        } else if (!arg.empty() && arg.front() == '-') {
            std::cerr << "Unknown option: " << arg << std::endl;
            return false;
        } else {
            if (positional_iterations_set) {
                std::cerr << "Error: multiple positional arguments provided." << std::endl;
                return false;
            }
            opts.iterations = static_cast<size_t>(std::stoul(arg));
            positional_iterations_set = true;
        }
    }

    if (opts.min_length < 2) {
        std::cerr << "Error: --min-length must be at least 2." << std::endl;
        return false;
    }

    if (opts.max_length < opts.min_length) {
        std::cerr << "Error: --max-length must be greater than or equal to --min-length." << std::endl;
        return false;
    }

    if (opts.quick) {
        opts.iterations = 100;
    }

    if (opts.iterations == 0) {
        std::cerr << "Error: iterations must be greater than 0." << std::endl;
        return false;
    }

    return true;
}

size_t progress_interval(size_t iterations) {
    return std::max<size_t>(1, iterations / 10);
}

std::string generate_plaintext(const std::string& alphabet, size_t length, std::mt19937_64& rng) {
    std::uniform_int_distribution<size_t> dist(0, alphabet.size() - 1);
    std::string result;
    result.reserve(length);
    for (size_t i = 0; i < length; ++i) {
        result.push_back(alphabet[dist(rng)]);
    }
    return result;
}

std::vector<uint8_t> random_bytes(size_t length, std::mt19937_64& rng) {
    std::uniform_int_distribution<uint32_t> dist(0, 255);
    std::vector<uint8_t> bytes(length);
    for (auto& b : bytes) {
        b = static_cast<uint8_t>(dist(rng));
    }
    return bytes;
}

} // namespace

int main(int argc, char* argv[]) {
    Options options;
    if (!parse_arguments(argc, argv, options)) {
        return 1;
    }

    if (options.help) {
        print_usage(argv[0]);
        return 0;
    }

    auto alphabet_map = build_alphabet_map();

    // Validate requested alphabets
    std::vector<AlphabetInfo> selected_alphabets;
    selected_alphabets.reserve(options.alphabets.size());
    for (const auto& name : options.alphabets) {
        auto it = alphabet_map.find(name);
        if (it == alphabet_map.end()) {
            std::cerr << "Error: Unknown alphabet '" << name << "'." << std::endl;
            return 1;
        }
        selected_alphabets.push_back(it->second);
    }

    std::mt19937_64 rng;
    if (options.seed_provided) {
        rng.seed(options.seed);
    } else {
        std::random_device rd;
        rng.seed(static_cast<uint64_t>(std::chrono::steady_clock::now().time_since_epoch().count()) ^ rd());
    }

    std::uniform_int_distribution<size_t> length_dist(options.min_length, options.max_length);

    std::cout << "FF3 Stress Test v1.0\n";
    std::cout << "====================\n\n";
    std::cout << "Warning: FF3 was withdrawn by NIST; run for education and research only.\n\n";
    std::cout << "Test configuration\n";
    std::cout << "  Iterations per alphabet: " << options.iterations << "\n";
    std::cout << "  Random key/tweak generation: enabled\n";
    std::cout << "  String length range: " << options.min_length << "-" << options.max_length << " characters\n";
    std::cout << "  Alphabets: " << join(options.alphabets, ", ") << "\n\n";

    size_t total_tests = 0;
    size_t total_failures = 0;

    auto start_all = std::chrono::steady_clock::now();

    for (const auto& alphabet : selected_alphabets) {
        std::cout << "Testing " << alphabet.name << "...\n";
        std::cout << "  Alphabet: " << alphabet.charset << " (radix " << alphabet.charset.size() << ")\n";

        size_t passed = 0;
        size_t failed = 0;
        size_t interval = progress_interval(options.iterations);

        for (size_t i = 0; i < options.iterations; ++i) {
            auto key = random_bytes(16, rng);
            auto tweak = random_bytes(8, rng);
            size_t length = length_dist(rng);
            auto plaintext = generate_plaintext(alphabet.charset, length, rng);

            bool success = true;
            std::string ciphertext;
            std::string decrypted;

            try {
                auto cipher = alphabet.factory(key, tweak);
                ciphertext = cipher.encrypt(plaintext);
                decrypted = cipher.decrypt(ciphertext);
                success = (decrypted == plaintext);
            } catch (const std::exception&) {
                success = false;
            }

            if (success) {
                ++passed;
            } else {
                ++failed;
                std::cout << "  Round-trip failed:\n";
                std::cout << "    Key: " << to_hex(key) << "\n";
                std::cout << "    Tweak: " << to_hex(tweak) << "\n";
                std::cout << "    Plaintext: \"" << plaintext << "\"\n";
                std::cout << "    Ciphertext: \"" << ciphertext << "\"\n";
                std::cout << "    Decrypted: \"" << decrypted << "\" (mismatch)\n";
            }

            if ((i + 1) % interval == 0 || i + 1 == options.iterations) {
                size_t percent = static_cast<size_t>(((i + 1) * 100) / options.iterations);
                std::cout << "  Progress: " << (i + 1) << "/" << options.iterations
                          << " (" << percent << "%)" << "\n";
            }
        }

        std::cout << "  Passed: " << passed << "/" << (passed + failed) << "\n";
        std::cout << "  Failed: " << failed << "/" << (passed + failed) << "\n\n";

        total_tests += passed + failed;
        total_failures += failed;
    }

    auto end_all = std::chrono::steady_clock::now();
    auto elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(end_all - start_all).count();

    std::cout << "Summary\n";
    std::cout << "  Total tests: " << total_tests << "\n";
    std::cout << "  Failures: " << total_failures << "\n";
    if (elapsed > 0) {
        std::cout << "  Duration: " << elapsed << " ms" << "\n";
        std::cout << "  Throughput: " << (static_cast<double>(total_tests) * 1000.0 / elapsed) << " tests/sec\n";
    }

    if (total_failures == 0) {
        std::cout << "  Result: all stress tests passed\n";
        return 0;
    }

    std::cout << "  Result: failures detected\n";
    return 1;
}
