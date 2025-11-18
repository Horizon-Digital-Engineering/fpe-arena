/**
 * @file ff3_benchmark_new.cpp
 * @brief Spec-compliant FF3 benchmark with JSON output
 */

#include <ff3.hpp>
#include <nlohmann/json.hpp>
#include <iostream>
#include <fstream>
#include <sstream>
#include <iomanip>
#include <chrono>
#include <vector>
#include <random>
#include <cstring>
#include <thread>

#ifdef _WIN32
#include <windows.h>
#else
#include <unistd.h>
#include <sys/utsname.h>
#endif

using json = nlohmann::json;
using namespace ff3;

struct BenchmarkConfig {
    std::string alphabet = "digits";
    int radix = 10;
    std::vector<int> lengths = {9, 12, 16};
    std::vector<std::string> cases = {"enc", "dec", "roundtrip"};
    int iterations = 100000;
    int warmup = 10000;
    std::string key_hex = "EF4359D8D580AA4F7F036D6F04FC6A94";
    std::string tweak_hex = "D8E7920AFA330A73";
    int seed = 42;
    bool verbose = false;
    std::string json_out;
};

struct PlatformInfo {
    std::string os;
    std::string arch;
    std::string cpu = "unknown";
    int cores;
};

struct BenchmarkResult {
    std::string name;
    std::string test_case;
    json parameters;
    int iterations;
    int64_t elapsed_ns;
    double ns_per_op;
    double ops_per_sec;
    uint32_t checksum;
};

// Simple hash function for checksum
uint32_t hash_string(const std::string& s) {
    uint32_t hash = 0;
    for (char c : s) {
        hash = hash * 31 + static_cast<uint32_t>(c);
    }
    return hash;
}

// Generate random input strings
std::vector<std::string> generate_inputs(int count, int length, const std::string& alphabet_name, int seed) {
    std::mt19937 rng(seed);
    std::string charset;

    if (alphabet_name == "digits") {
        charset = "0123456789";
    } else if (alphabet_name == "hex") {
        charset = "0123456789abcdef";
    } else if (alphabet_name == "base36") {
        charset = "0123456789abcdefghijklmnopqrstuvwxyz";
    } else if (alphabet_name == "base62") {
        charset = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
    } else {
        charset = "0123456789";
    }

    std::uniform_int_distribution<int> dist(0, charset.size() - 1);
    std::vector<std::string> inputs;
    inputs.reserve(count);

    for (int i = 0; i < count; ++i) {
        std::string input;
        input.reserve(length);
        for (int j = 0; j < length; ++j) {
            input.push_back(charset[dist(rng)]);
        }
        inputs.push_back(std::move(input));
    }

    return inputs;
}

// Get platform information
PlatformInfo get_platform_info() {
    PlatformInfo info;

#ifdef _WIN32
    info.os = "Windows";
    #ifdef _WIN64
        info.arch = "x64";
    #else
        info.arch = "x86";
    #endif
    SYSTEM_INFO sysInfo;
    GetSystemInfo(&sysInfo);
    info.cores = sysInfo.dwNumberOfProcessors;
#else
    struct utsname buf;
    if (uname(&buf) == 0) {
        info.os = buf.sysname;
        info.arch = buf.machine;
    } else {
        info.os = "unknown";
        info.arch = "unknown";
    }
    info.cores = std::thread::hardware_concurrency();
#endif

    return info;
}

// Run single benchmark
BenchmarkResult run_benchmark(
    FF3& cipher,
    const std::string& bench_case,
    int length,
    int radix,
    const std::string& alphabet_name,
    int iterations,
    int warmup,
    const std::vector<uint8_t>& key,
    const std::string& tweak_hex,
    int seed
) {
    // Generate ring buffer of inputs
    const int ring_size = 64;
    auto inputs = generate_inputs(ring_size, length, alphabet_name, seed);

    // Precompute ciphertexts for decrypt benchmarks
    std::vector<std::string> precomputed_cts;
    if (bench_case == "dec") {
        precomputed_cts.reserve(ring_size);
        for (const auto& pt : inputs) {
            precomputed_cts.push_back(cipher.encrypt(pt));
        }
    }

    // Warmup phase
    for (int i = 0; i < warmup; ++i) {
        const auto& input = inputs[i % ring_size];
        if (bench_case == "enc") {
            volatile auto ct = cipher.encrypt(input);
        } else if (bench_case == "dec") {
            const auto& ct = precomputed_cts[i % ring_size];
            volatile auto pt = cipher.decrypt(ct);
        } else { // roundtrip
            auto ct = cipher.encrypt(input);
            volatile auto pt = cipher.decrypt(ct);
        }
    }

    // Measured phase
    uint32_t checksum = 0;
    auto start = std::chrono::high_resolution_clock::now();

    for (int i = 0; i < iterations; ++i) {
        const auto& input = inputs[i % ring_size];
        std::string output;

        if (bench_case == "enc") {
            output = cipher.encrypt(input);
            checksum ^= hash_string(output);
        } else if (bench_case == "dec") {
            const auto& ct = precomputed_cts[i % ring_size];
            output = cipher.decrypt(ct);
            checksum ^= hash_string(output);
        } else { // roundtrip
            auto ct = cipher.encrypt(input);
            output = cipher.decrypt(ct);
            checksum ^= hash_string(output);
        }
    }

    auto end = std::chrono::high_resolution_clock::now();
    auto elapsed = std::chrono::duration_cast<std::chrono::nanoseconds>(end - start);
    int64_t elapsed_ns = elapsed.count();
    double ns_per_op = static_cast<double>(elapsed_ns) / iterations;
    double ops_per_sec = 1e9 / ns_per_op;

    // Create key fingerprint
    std::string key_fingerprint;
    {
        std::ostringstream oss;
        oss << std::hex << std::uppercase;
        for (size_t i = 0; i < std::min(size_t(4), key.size()); ++i) {
            oss << std::setw(2) << std::setfill('0') << static_cast<int>(key[i]);
        }
        key_fingerprint = oss.str();
    }

    BenchmarkResult result;
    result.name = bench_case + "_len" + std::to_string(length) + "_radix" + std::to_string(radix);
    result.test_case = bench_case;
    result.parameters = {
        {"alphabet", alphabet_name},
        {"radix", radix},
        {"length", length},
        {"key_bits", static_cast<int>(key.size() * 8)},
        {"key_fingerprint", key_fingerprint},
        {"tweak", tweak_hex}
    };
    result.iterations = iterations;
    result.elapsed_ns = elapsed_ns;
    result.ns_per_op = ns_per_op;
    result.ops_per_sec = ops_per_sec;
    result.checksum = checksum;

    return result;
}

// Helper to parse CSV integers
std::vector<int> parse_csv_ints(const std::string& csv) {
    std::vector<int> result;
    std::stringstream ss(csv);
    std::string item;
    while (std::getline(ss, item, ',')) {
        if (!item.empty()) {
            result.push_back(std::stoi(item));
        }
    }
    return result;
}

// Helper to parse CSV strings
std::vector<std::string> parse_csv_strings(const std::string& csv) {
    std::vector<std::string> result;
    std::stringstream ss(csv);
    std::string item;
    while (std::getline(ss, item, ',')) {
        if (!item.empty()) {
            result.push_back(item);
        }
    }
    return result;
}

// Load config from JSON file
void load_config_file(BenchmarkConfig& config, const std::string& path) {
    std::ifstream file(path);
    if (!file) {
        std::cerr << "ERR: Cannot open config file: " << path << "\n";
        std::exit(2);
    }

    json j;
    file >> j;

    if (j.contains("alphabet")) config.alphabet = j["alphabet"];
    if (j.contains("radix")) config.radix = j["radix"];
    if (j.contains("lengths")) config.lengths = j["lengths"].get<std::vector<int>>();
    if (j.contains("cases")) config.cases = j["cases"].get<std::vector<std::string>>();
    if (j.contains("iterations")) config.iterations = j["iterations"];
    if (j.contains("warmup")) config.warmup = j["warmup"];
    if (j.contains("key")) config.key_hex = j["key"];
    if (j.contains("tweak")) config.tweak_hex = j["tweak"];
    if (j.contains("seed")) config.seed = j["seed"];
}

int main(int argc, char* argv[]) {
    BenchmarkConfig config;

    // Simple argument parsing
    for (int i = 1; i < argc; ++i) {
        std::string arg = argv[i];
        if ((arg == "-h" || arg == "--help")) {
            std::cout << "FF3 Performance Benchmark Tool\n\n";
            std::cout << "Usage: ff3_benchmark [OPTIONS]\n\n";
            std::cout << "Options:\n";
            std::cout << "  --config <file>        JSON config file\n";
            std::cout << "  --alphabet <name>      Alphabet: digits|hex|base36|base62 (default: digits)\n";
            std::cout << "  --radix <n>            Radix 2-62 (default: 10)\n";
            std::cout << "  --lengths <csv>        Comma-separated lengths (default: 9,12,16)\n";
            std::cout << "  --cases <csv>          Test cases: enc,dec,roundtrip (default: all)\n";
            std::cout << "  --iterations <n>       Measured iterations (default: 100000)\n";
            std::cout << "  --warmup <n>           Warmup iterations (default: 10000)\n";
            std::cout << "  --key <hex>            Hex-encoded key\n";
            std::cout << "  --tweak <hex>          Hex-encoded tweak\n";
            std::cout << "  --seed <n>             Random seed (default: 42)\n";
            std::cout << "  --quick                Reduce iterations 10x\n";
            std::cout << "  --json-out <file>      Write JSON results to file\n";
            std::cout << "  --verbose              Show progress messages\n";
            std::cout << "  -h, --help             Show this help\n";
            return 0;
        }

        if (arg == "--config" && i + 1 < argc) {
            load_config_file(config, argv[++i]);
        } else if (arg == "--alphabet" && i + 1 < argc) {
            config.alphabet = argv[++i];
        } else if (arg == "--radix" && i + 1 < argc) {
            config.radix = std::atoi(argv[++i]);
        } else if (arg == "--lengths" && i + 1 < argc) {
            config.lengths = parse_csv_ints(argv[++i]);
        } else if (arg == "--cases" && i + 1 < argc) {
            config.cases = parse_csv_strings(argv[++i]);
        } else if (arg == "--iterations" && i + 1 < argc) {
            config.iterations = std::atoi(argv[++i]);
        } else if (arg == "--warmup" && i + 1 < argc) {
            config.warmup = std::atoi(argv[++i]);
        } else if (arg == "--key" && i + 1 < argc) {
            config.key_hex = argv[++i];
        } else if (arg == "--tweak" && i + 1 < argc) {
            config.tweak_hex = argv[++i];
        } else if (arg == "--quick") {
            config.iterations /= 10;
            config.warmup /= 10;
        } else if (arg == "--json-out" && i + 1 < argc) {
            config.json_out = argv[++i];
        } else if (arg == "--verbose") {
            config.verbose = true;
        } else if (arg == "--seed" && i + 1 < argc) {
            config.seed = std::atoi(argv[++i]);
        }
    }

    try {
        auto key = hex_to_bytes(config.key_hex);
        auto tweak = hex_to_bytes(config.tweak_hex);

        // Build cipher
        FF3 cipher = [&]() {
            if (config.alphabet == "digits") return digits(key, tweak);
            if (config.alphabet == "hex") return hex(key, tweak);
            if (config.alphabet == "base36") return base36(key, tweak);
            if (config.alphabet == "base62") return base62(key, tweak);
            return digits(key, tweak);
        }();

        auto total_start = std::chrono::high_resolution_clock::now();
        std::vector<BenchmarkResult> benchmarks;
        uint32_t overall_checksum = 0;

        for (int length : config.lengths) {
            for (const auto& bench_case : config.cases) {
                if (config.verbose) {
                    std::cerr << "Running " << bench_case << "_len" << length << "_radix" << config.radix << "...\n";
                }

                auto result = run_benchmark(
                    cipher, bench_case, length, config.radix, config.alphabet,
                    config.iterations, config.warmup, key, config.tweak_hex, config.seed
                );

                overall_checksum ^= result.checksum;
                benchmarks.push_back(result);
            }
        }

        auto total_end = std::chrono::high_resolution_clock::now();
        auto total_duration = std::chrono::duration_cast<std::chrono::nanoseconds>(total_end - total_start);
        double total_duration_sec = total_duration.count() / 1e9;

        // Build JSON report
        auto platform = get_platform_info();

        json report = {
            {"metadata", {
                {"version", "1.0"},
                {"timestamp", []() {
                    auto now = std::chrono::system_clock::now();
                    auto itt = std::chrono::system_clock::to_time_t(now);
                    std::ostringstream ss;
                    ss << std::put_time(gmtime(&itt), "%FT%TZ");
                    return ss.str();
                }()},
                {"language", "cpp"},
                {"runtime", std::to_string(__cplusplus)},
                {"platform", {
                    {"os", platform.os},
                    {"arch", platform.arch},
                    {"cpu", platform.cpu},
                    {"cores", platform.cores}
                }}
            }},
            {"configuration", {
                {"seed", config.seed},
                {"warmup_iterations", config.warmup}
            }},
            {"benchmarks", json::array()},
            {"summary", {
                {"total_tests", static_cast<int>(benchmarks.size())},
                {"total_duration_sec", total_duration_sec},
                {"checksum", []( uint32_t cs) {
                    std::ostringstream oss;
                    oss << std::hex << std::setw(8) << std::setfill('0') << cs;
                    return oss.str();
                }(overall_checksum)}
            }}
        };

        for (const auto& bench : benchmarks) {
            std::ostringstream checksum_hex;
            checksum_hex << std::hex << std::setw(8) << std::setfill('0') << bench.checksum;

            report["benchmarks"].push_back({
                {"name", bench.name},
                {"test_case", bench.test_case},
                {"parameters", bench.parameters},
                {"iterations", bench.iterations},
                {"elapsed_ns", bench.elapsed_ns},
                {"ns_per_op", bench.ns_per_op},
                {"ops_per_sec", bench.ops_per_sec},
                {"checksum", checksum_hex.str()}
            });
        }

        std::string output = report.dump(2);

        if (!config.json_out.empty()) {
            std::ofstream ofs(config.json_out);
            ofs << output << "\n";
        } else {
            std::cout << output << "\n";
        }

        return 0;

    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << "\n";
        return 2;
    }
}
