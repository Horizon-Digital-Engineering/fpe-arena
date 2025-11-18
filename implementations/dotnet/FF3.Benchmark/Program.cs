using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text.Json;
using System.Text.Json.Serialization;
using FF3.Core;

namespace FF3.Benchmark
{
    public class BenchmarkConfig
    {
        public string Alphabet { get; set; } = "digits";
        public int Radix { get; set; } = 10;
        public List<int> Lengths { get; set; } = new() { 9, 12, 16 };
        public List<string> Cases { get; set; } = new() { "enc", "dec", "roundtrip" };
        public int Iterations { get; set; } = 100000;
        public int Warmup { get; set; } = 10000;
        public string Key { get; set; } = "EF4359D8D580AA4F7F036D6F04FC6A94";
        public string Tweak { get; set; } = "D8E7920AFA330A73";
        public int Seed { get; set; } = 42;
        public bool Verbose { get; set; } = false;
        public string? JsonOut { get; set; } = null;
    }

    public class BenchmarkParameters
    {
        public string Alphabet { get; set; } = "";
        public int Radix { get; set; }
        public int Length { get; set; }
        public int KeyBits { get; set; }
        public string KeyFingerprint { get; set; } = "";
        public string Tweak { get; set; } = "";
    }

    public class BenchmarkResult
    {
        public string Name { get; set; } = "";

        [JsonPropertyName("test_case")]
        public string TestCase { get; set; } = "";

        public BenchmarkParameters Parameters { get; set; } = new();
        public int Iterations { get; set; }

        [JsonPropertyName("elapsed_ns")]
        public long ElapsedNs { get; set; }

        [JsonPropertyName("ns_per_op")]
        public double NsPerOp { get; set; }

        [JsonPropertyName("ops_per_sec")]
        public double OpsPerSec { get; set; }

        public string Checksum { get; set; } = "";
    }

    public class PlatformInfo
    {
        public string OS { get; set; } = "";
        public string Arch { get; set; } = "";
        public string CPU { get; set; } = "unknown";
        public int Cores { get; set; }
    }

    public class MetadataInfo
    {
        public string Version { get; set; } = "1.0";
        public string Timestamp { get; set; } = "";
        public string Language { get; set; } = "dotnet";
        public string Runtime { get; set; } = "";
        public PlatformInfo Platform { get; set; } = new();
    }

    public class ConfigurationInfo
    {
        public int Seed { get; set; }
        public int WarmupIterations { get; set; }
    }

    public class SummaryInfo
    {
        public int TotalTests { get; set; }
        public double TotalDurationSec { get; set; }
        public string Checksum { get; set; } = "";
    }

    public class BenchmarkReport
    {
        public MetadataInfo Metadata { get; set; } = new();
        public ConfigurationInfo Configuration { get; set; } = new();
        public List<BenchmarkResult> Benchmarks { get; set; } = new();
        public SummaryInfo Summary { get; set; } = new();
    }

    class Program
    {
        static void Main(string[] args)
        {
            if (HasArgument(args, "-h") || HasArgument(args, "--help"))
            {
                ShowHelp();
                return;
            }

            var config = new BenchmarkConfig();

            // Load config file if provided
            string? configFile = GetOptionalArgument(args, "--config");
            if (configFile != null)
            {
                try
                {
                    string json = File.ReadAllText(configFile);
                    var fileConfig = JsonSerializer.Deserialize<BenchmarkConfig>(json, new JsonSerializerOptions
                    {
                        PropertyNameCaseInsensitive = true
                    });
                    if (fileConfig != null)
                    {
                        config = fileConfig;
                    }
                }
                catch (Exception ex)
                {
                    Console.Error.WriteLine($"Error loading config file: {ex.Message}");
                    Environment.Exit(1);
                }
            }

            // Override with command-line arguments
            config.Alphabet = GetArgument(args, "--alphabet", config.Alphabet);
            config.Radix = GetIntArgument(args, "--radix", config.Radix);
            config.Iterations = GetIntArgument(args, "--iterations", config.Iterations);
            config.Warmup = GetIntArgument(args, "--warmup", config.Warmup);
            config.Key = GetArgument(args, "--key", config.Key);
            config.Tweak = GetArgument(args, "--tweak", config.Tweak);
            config.Seed = GetIntArgument(args, "--seed", config.Seed);
            config.Verbose = HasArgument(args, "--verbose");
            config.JsonOut = GetOptionalArgument(args, "--json-out");

            string? lengthsArg = GetOptionalArgument(args, "--lengths");
            if (lengthsArg != null)
            {
                config.Lengths = ParseLengths(lengthsArg).ToList();
            }

            string? casesArg = GetOptionalArgument(args, "--cases");
            if (casesArg != null)
            {
                config.Cases = ParseCases(casesArg).ToList();
            }

            if (HasArgument(args, "--quick"))
            {
                config.Iterations = Math.Min(config.Iterations, 10000);
                config.Warmup = Math.Min(config.Warmup, 1000);
            }

            try
            {
                byte[] key = Core.FF3.HexToBytes(config.Key);
                byte[] tweak = Core.FF3.HexToBytes(config.Tweak);

                var (cipher, alphaLabel) = BuildCipher(config.Radix, config.Alphabet, key, tweak);

                var report = new BenchmarkReport
                {
                    Metadata = new MetadataInfo
                    {
                        Timestamp = DateTime.UtcNow.ToString("yyyy-MM-ddTHH:mm:ss.ffffffZ"),
                        Runtime = Environment.Version.ToString(),
                        Platform = new PlatformInfo
                        {
                            OS = RuntimeInformation.OSDescription,
                            Arch = RuntimeInformation.OSArchitecture.ToString().ToLower(),
                            Cores = Environment.ProcessorCount
                        }
                    },
                    Configuration = new ConfigurationInfo
                    {
                        Seed = config.Seed,
                        WarmupIterations = config.Warmup
                    }
                };

                var totalSw = Stopwatch.StartNew();
                int overallChecksum = 0;

                using (cipher)
                {
                    foreach (int length in config.Lengths)
                    {
                        foreach (string benchCase in config.Cases)
                        {
                            if (config.Verbose)
                            {
                                Console.Error.WriteLine($"Running {benchCase}_len{length}_radix{config.Radix}...");
                            }

                            var result = RunBenchmark(cipher, benchCase, config.Radix, alphaLabel, length,
                                config.Iterations, config.Warmup, key, config.Tweak, config.Seed, out int checksum);
                            overallChecksum ^= checksum;
                            report.Benchmarks.Add(result);
                        }
                    }
                }

                totalSw.Stop();

                report.Summary = new SummaryInfo
                {
                    TotalTests = report.Benchmarks.Count,
                    TotalDurationSec = totalSw.Elapsed.TotalSeconds,
                    Checksum = overallChecksum.ToString("x8")
                };

                var options = new JsonSerializerOptions
                {
                    WriteIndented = true,
                    PropertyNamingPolicy = JsonNamingPolicy.CamelCase
                };

                string json = JsonSerializer.Serialize(report, options);

                if (config.JsonOut != null)
                {
                    File.WriteAllText(config.JsonOut, json);
                }
                else
                {
                    Console.WriteLine(json);
                }
            }
            catch (Exception ex)
            {
                Console.Error.WriteLine($"Error: {ex.Message}");
                Environment.Exit(2);
            }
        }

        static void ShowHelp()
        {
            Console.WriteLine(@"FF3 Performance Benchmark Tool

Usage: ff3-benchmark [OPTIONS]

Options:
  --config <file>        Read configuration from JSON file
  --alphabet <name>      Alphabet: digits|hex|base36|base62 (default: digits)
  --radix <n>            Radix 2..62 for custom alphabets (default: 10)
  --lengths <csv>        Comma-separated lengths (default: 9,12,16)
  --cases <csv>          Test cases: enc,dec,roundtrip (default: all)
  --iterations <n>       Measured iterations (default: 100000)
  --warmup <n>           Warmup iterations (default: 10000)
  --key <hex>            Hex-encoded key (default: standard)
  --tweak <hex>          Hex-encoded tweak (default: standard)
  --quick                Reduce iterations 10x for fast testing
  --json-out <file>      Write JSON results to file (silent mode)
  --verbose              Show progress messages
  --seed <n>             Random seed for reproducibility (default: 42)
  -h, --help             Show this help message

Examples:
  ff3-benchmark --quick --verbose
  ff3-benchmark --lengths 9,12,16 --json-out results.json
  ff3-benchmark --config benchmark.json --json-out results.json");
        }

        static BenchmarkResult RunBenchmark(Core.FF3 cipher, string benchCase, int radix, string alphabet,
                                          int length, int iterations, int warmup, byte[] key, string tweakHex,
                                          int seed, out int checksum)
        {
            var rng = new Random(seed);

            // Generate ring buffer of varied inputs (64 samples, matching Go/Java)
            const int ringSize = 64;
            var inputs = new List<string>(ringSize);
            for (int i = 0; i < ringSize; i++)
            {
                inputs.Add(GenerateSample(radix, length, alphabet, rng));
            }

            // Pre-compute ciphertexts for decrypt benchmarks (like Go and Java)
            List<string>? precomputedCTs = null;
            if (benchCase == "dec")
            {
                precomputedCTs = new List<string>(ringSize);
                foreach (var pt in inputs)
                {
                    precomputedCTs.Add(cipher.Encrypt(pt));
                }
            }

            // Warmup
            for (int i = 0; i < warmup; i++)
            {
                string input = inputs[i % inputs.Count];
                switch (benchCase)
                {
                    case "enc":
                        cipher.Encrypt(input);
                        break;
                    case "dec":
                        string ct = precomputedCTs![i % precomputedCTs.Count];
                        cipher.Decrypt(ct);
                        break;
                    case "roundtrip":
                        string ct2 = cipher.Encrypt(input);
                        cipher.Decrypt(ct2);
                        break;
                }
            }

            // Measured phase
            var sw = Stopwatch.StartNew();
            checksum = 0;

            for (int i = 0; i < iterations; i++)
            {
                string input = inputs[i % inputs.Count];
                string output;

                switch (benchCase)
                {
                    case "enc":
                        output = cipher.Encrypt(input);
                        checksum ^= output.GetHashCode();
                        break;
                    case "dec":
                        string ct = precomputedCTs![i % precomputedCTs.Count];
                        output = cipher.Decrypt(ct);
                        checksum ^= output.GetHashCode();
                        break;
                    case "roundtrip":
                        string ct2 = cipher.Encrypt(input);
                        output = cipher.Decrypt(ct2);
                        checksum ^= output.GetHashCode();
                        break;
                    default:
                        throw new ArgumentException($"Unknown test case: {benchCase}");
                }
            }

            sw.Stop();
            long elapsedNs = sw.ElapsedTicks * 1000000000L / Stopwatch.Frequency;

            return new BenchmarkResult
            {
                Name = $"{benchCase}_len{length}_radix{radix}",
                TestCase = benchCase,
                Parameters = new BenchmarkParameters
                {
                    Alphabet = alphabet,
                    Radix = radix,
                    Length = length,
                    KeyBits = key.Length * 8,
                    KeyFingerprint = ShortFingerprint(key),
                    Tweak = tweakHex
                },
                Iterations = iterations,
                ElapsedNs = elapsedNs,
                NsPerOp = (double)elapsedNs / iterations,
                OpsPerSec = iterations / sw.Elapsed.TotalSeconds,
                Checksum = checksum.ToString("x8")
            };
        }

        static (Core.FF3 cipher, string label) BuildCipher(int radix, string name, byte[] key, byte[] tweak)
        {
            return (name, radix) switch
            {
                ("digits", _) or ("auto", 10) => (Core.FF3.Digits(key, tweak), "digits"),
                ("hex", _) or ("auto", 16) => (Core.FF3.HexLower(key, tweak), "hex"),
                ("base36", _) or ("auto", 36) => (Core.FF3.Base36Lower(key, tweak), "base36"),
                ("base62", _) or ("auto", 62) => (Core.FF3.Base62(key, tweak), "base62"),
                _ when radix <= 10 => (new Core.FF3(key, tweak, FF3Alphabets.Digits[..radix]), $"custom{radix}"),
                _ when radix <= 36 => (new Core.FF3(key, tweak, FF3Alphabets.Base36Lower[..radix]), $"custom{radix}"),
                _ when radix <= 62 => (new Core.FF3(key, tweak, FF3Alphabets.Base62[..radix]), $"custom{radix}"),
                _ => throw new ArgumentException($"Unsupported radix {radix}")
            };
        }

        static string GenerateSample(int radix, int length, string alphabetName, Random rng)
        {
            string chars = alphabetName switch
            {
                "digits" => FF3Alphabets.Digits,
                "hex" => FF3Alphabets.HexLower,
                "base36" => FF3Alphabets.Base36Lower,
                "base62" => FF3Alphabets.Base62,
                _ when radix <= 10 => FF3Alphabets.Digits[..radix],
                _ when radix <= 36 => FF3Alphabets.Base36Lower[..radix],
                _ => FF3Alphabets.Base62[..Math.Min(radix, 62)]
            };

            var result = new char[length];
            for (int i = 0; i < length; i++)
            {
                result[i] = chars[rng.Next(chars.Length)];
            }
            return new string(result);
        }

        static int[] ParseLengths(string csv) =>
            csv.Split(',').Select(p => p.Trim()).Where(p => int.TryParse(p, out _))
               .Select(int.Parse).Where(v => v > 0).DefaultIfEmpty(18).ToArray();

        static string[] ParseCases(string csv) =>
            csv.Split(',').Select(p => p.Trim().ToLower()).Where(p =>
                p == "enc" || p == "dec" || p == "roundtrip").DefaultIfEmpty("enc").ToArray();

        static string ShortFingerprint(byte[] bytes)
        {
            if (bytes.Length == 0) return "";
            string hex = Convert.ToHexString(bytes);
            return hex.Length <= 8 ? hex : hex[..8];
        }

        static string GetArgument(string[] args, string flag, string defaultValue)
        {
            for (int i = 0; i < args.Length - 1; i++)
            {
                if (args[i] == flag)
                    return args[i + 1];
            }
            return defaultValue;
        }

        static string? GetOptionalArgument(string[] args, string flag)
        {
            for (int i = 0; i < args.Length - 1; i++)
            {
                if (args[i] == flag)
                    return args[i + 1];
            }
            return null;
        }

        static int GetIntArgument(string[] args, string flag, int defaultValue)
        {
            string value = GetArgument(args, flag, defaultValue.ToString());
            return int.TryParse(value, out int result) ? result : defaultValue;
        }

        static bool HasArgument(string[] args, string flag) => Array.IndexOf(args, flag) >= 0;
    }
}