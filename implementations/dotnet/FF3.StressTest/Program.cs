using System;
using System.Collections.Generic;
using System.Linq;
using System.Security.Cryptography;
using FF3.Core;
using FF3Cipher = FF3.Core.FF3;

namespace FF3.StressTest
{
    internal sealed class Options
    {
        public int Iterations { get; set; } = 1000;
        public List<string> Alphabets { get; } = new() { "digits", "hex-lower", "base36-lower", "base62" };
        public int MinLength { get; set; } = 6;
        public int MaxLength { get; set; } = 20;
        public bool Quick { get; set; }
        public bool SeedProvided { get; set; }
        public int Seed { get; set; }
        public bool Help { get; set; }
    }

    internal static class Program
    {
        private static readonly Dictionary<string, (string charset, Func<byte[], byte[], FF3Cipher>)> AlphabetMap = new()
        {
            { "digits", (FF3Alphabets.Digits, (k, t) => FF3Cipher.Digits(k, t)) },
            { "hex-lower", (FF3Alphabets.HexLower, (k, t) => FF3Cipher.HexLower(k, t)) },
            { "hex-upper", (FF3Alphabets.HexUpper, (k, t) => FF3Cipher.HexUpper(k, t)) },
            { "base36-lower", (FF3Alphabets.Base36Lower, (k, t) => FF3Cipher.Base36Lower(k, t)) },
            { "base36-upper", (FF3Alphabets.Base36Upper, (k, t) => FF3Cipher.Base36Upper(k, t)) },
            { "base62", (FF3Alphabets.Base62, (k, t) => FF3Cipher.Base62(k, t)) },
        };

        private static int Main(string[] args)
        {
            if (!TryParseOptions(args, out var options))
            {
                return 1;
            }

            if (options.Help)
            {
                PrintUsage();
                return 0;
            }

            if (options.Quick)
            {
                options.Iterations = 100;
            }

            if (options.Iterations <= 0)
            {
                Console.Error.WriteLine("Error: iterations must be greater than 0.");
                return 1;
            }
            if (options.MinLength < 2)
            {
                Console.Error.WriteLine("Error: --min-length must be at least 2.");
                return 1;
            }
            if (options.MaxLength < options.MinLength)
            {
                Console.Error.WriteLine("Error: --max-length must be greater than or equal to --min-length.");
                return 1;
            }

            var selected = new List<(string name, string charset, Func<byte[], byte[], FF3Cipher> factory)>();
            foreach (var name in options.Alphabets)
            {
                if (!AlphabetMap.TryGetValue(name, out var info))
                {
                    Console.Error.WriteLine($"Error: unknown alphabet '{name}'.");
                    return 1;
                }
                selected.Add((name, info.charset, info.Item2));
            }

            var rng = options.SeedProvided ? new Random(options.Seed) : new Random();

            Console.WriteLine("FF3 Stress Test v1.0");
            Console.WriteLine("====================\n");
            Console.WriteLine("Warning: FF3 was withdrawn by NIST; run for education and research only.\n");
            Console.WriteLine("Test configuration");
            Console.WriteLine($"  Iterations per alphabet: {options.Iterations}");
            Console.WriteLine("  Random key/tweak generation: enabled");
            Console.WriteLine($"  String length range: {options.MinLength}-{options.MaxLength} characters");
            Console.WriteLine($"  Alphabets: {string.Join(", ", options.Alphabets)}\n");

            var totalTests = 0;
            var totalFailures = 0;
            var startAll = DateTime.UtcNow;

            foreach (var alphabet in selected)
            {
                Console.WriteLine($"Testing {alphabet.name}...");
                Console.WriteLine($"  Alphabet: {alphabet.charset} (radix {alphabet.charset.Length})");

                var passed = 0;
                var failed = 0;
                var interval = Math.Max(1, options.Iterations / 10);

                for (var i = 0; i < options.Iterations; i++)
                {
                    var key = RandomBytes(16);
                    var tweak = RandomBytes(8);
                    var length = rng.Next(options.MinLength, options.MaxLength + 1);
                    var plaintext = GeneratePlaintext(alphabet.charset, length, rng);

                    FF3Cipher cipher;
                    try
                    {
                        cipher = alphabet.factory(key, tweak);
                    }
                    catch (Exception ex)
                    {
                        failed++;
                        PrintFailure(key, tweak, plaintext, string.Empty, string.Empty, $"cipher initialization failed: {ex.Message}");
                        continue;
                    }

                    try
                    {
                        var ciphertext = cipher.Encrypt(plaintext);
                        var decrypted = cipher.Decrypt(ciphertext);
                        if (decrypted != plaintext)
                        {
                            failed++;
                            PrintFailure(key, tweak, plaintext, ciphertext, decrypted, "round-trip mismatch");
                        }
                        else
                        {
                            passed++;
                        }
                    }
                    catch (Exception ex)
                    {
                        failed++;
                        PrintFailure(key, tweak, plaintext, string.Empty, string.Empty, $"encryption error: {ex.Message}");
                    }
                    finally
                    {
                        cipher.Dispose();
                    }

                    if ((i + 1) % interval == 0 || i + 1 == options.Iterations)
                    {
                        var percent = (i + 1) * 100 / options.Iterations;
                        Console.WriteLine($"  Progress: {i + 1}/{options.Iterations} ({percent}%)");
                    }
                }

                Console.WriteLine($"  Passed: {passed}/{passed + failed}");
                Console.WriteLine($"  Failed: {failed}/{passed + failed}\n");

                totalTests += passed + failed;
                totalFailures += failed;
            }

            var elapsed = (int)(DateTime.UtcNow - startAll).TotalMilliseconds;
            Console.WriteLine("Summary");
            Console.WriteLine($"  Total tests: {totalTests}");
            Console.WriteLine($"  Failures: {totalFailures}");
            Console.WriteLine($"  Duration: {elapsed} ms");
            if (elapsed > 0)
            {
                var throughput = totalTests * 1000.0 / elapsed;
                Console.WriteLine($"  Throughput: {throughput:F2} tests/sec");
            }

            if (totalFailures == 0)
            {
                Console.WriteLine("  Result: all stress tests passed");
                return 0;
            }

            Console.WriteLine("  Result: failures detected");
            return 1;
        }

        private static bool TryParseOptions(string[] args, out Options options)
        {
            options = new Options();
            var i = 0;
            while (i < args.Length)
            {
                var arg = args[i];
                switch (arg)
                {
                    case "-h":
                    case "--help":
                        options.Help = true;
                        return true;
                    case "--alphabets":
                        if (!TryReadValue(args, ref i, out var list)) return false;
                        options.Alphabets.Clear();
                        foreach (var item in list.Split(','))
                        {
                            var trimmed = item.Trim();
                            if (!string.IsNullOrEmpty(trimmed))
                            {
                                options.Alphabets.Add(trimmed);
                            }
                        }
                        if (options.Alphabets.Count == 0)
                        {
                            Console.Error.WriteLine("Error: --alphabets list cannot be empty.");
                            return false;
                        }
                        break;
                    case "--min-length":
                        if (!TryReadInt(args, ref i, out var minLen)) return false;
                        options.MinLength = minLen;
                        break;
                    case "--max-length":
                        if (!TryReadInt(args, ref i, out var maxLen)) return false;
                        options.MaxLength = maxLen;
                        break;
                    case "--quick":
                        options.Quick = true;
                        break;
                    case "--seed":
                        if (!TryReadInt(args, ref i, out var seed)) return false;
                        options.SeedProvided = true;
                        options.Seed = seed;
                        break;
                    default:
                        if (arg.StartsWith("-"))
                        {
                            Console.Error.WriteLine($"Unknown option: {arg}");
                            return false;
                        }
                        if (!int.TryParse(arg, out var iterations) || iterations <= 0)
                        {
                            Console.Error.WriteLine($"Invalid iterations value: {arg}");
                            return false;
                        }
                        options.Iterations = iterations;
                        break;
                }
                i++;
            }
            return true;
        }

        private static bool TryReadValue(string[] args, ref int index, out string value)
        {
            if (index + 1 >= args.Length)
            {
                Console.Error.WriteLine($"Error: {args[index]} requires a value.");
                value = string.Empty;
                return false;
            }
            value = args[index + 1];
            index++;
            return true;
        }

        private static bool TryReadInt(string[] args, ref int index, out int value)
        {
            if (!TryReadValue(args, ref index, out var token))
            {
                value = 0;
                return false;
            }
            if (!int.TryParse(token, out value))
            {
                Console.Error.WriteLine($"Invalid integer value: {token}");
                return false;
            }
            return true;
        }

        private static void PrintUsage()
        {
            Console.WriteLine("FF3 Stress Test Tool\n");
            Console.WriteLine("Usage: ff3-stresstest [OPTIONS] [ITERATIONS]\n");
            Console.WriteLine("Options:");
            Console.WriteLine("  --alphabets LIST      Comma-separated list (default: digits,hex-lower,base36-lower,base62)");
            Console.WriteLine("  --min-length N        Minimum plaintext length (default: 6)");
            Console.WriteLine("  --max-length N        Maximum plaintext length (default: 20)");
            Console.WriteLine("  --quick               Run 100 iterations (fast test)");
            Console.WriteLine("  --seed N              Random seed for reproducibility");
        }

        private static byte[] RandomBytes(int length)
        {
            var buffer = new byte[length];
            RandomNumberGenerator.Fill(buffer);
            return buffer;
        }

        private static string GeneratePlaintext(string charset, int length, Random rng)
        {
            var result = new char[length];
            for (var i = 0; i < length; i++)
            {
                result[i] = charset[rng.Next(charset.Length)];
            }
            return new string(result);
        }

        private static void PrintFailure(byte[] key, byte[] tweak, string plaintext, string ciphertext, string decrypted, string detail)
        {
            Console.WriteLine("  Round-trip failed:");
            Console.WriteLine($"    Key: {Convert.ToHexString(key)}");
            Console.WriteLine($"    Tweak: {Convert.ToHexString(tweak)}");
            Console.WriteLine($"    Plaintext: \"{plaintext}\"");
            if (!string.IsNullOrEmpty(ciphertext))
            {
                Console.WriteLine($"    Ciphertext: \"{ciphertext}\"");
            }
            if (!string.IsNullOrEmpty(decrypted))
            {
                Console.WriteLine($"    Decrypted: \"{decrypted}\"");
            }
            if (!string.IsNullOrEmpty(detail))
            {
                Console.WriteLine($"    Detail: {detail}");
            }
        }
    }
}
