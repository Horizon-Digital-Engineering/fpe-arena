using System;
using System.Collections.Generic;
using System.IO;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using FF3.Core;

namespace FF3.Validate
{
    public class NISTVector
    {
        public int Sample { get; set; }
        public string Algorithm { get; set; } = "";
        public string Key { get; set; } = "";
        public int Radix { get; set; }
        public string Plaintext { get; set; } = "";
        public string Tweak { get; set; } = "";
        public string Ciphertext { get; set; } = "";
    }

    public class NISTDoc
    {
        public string Description { get; set; } = "";
        public string Source { get; set; } = "";
        public List<NISTVector> Vectors { get; set; } = new();
    }

    class Program
    {
        static int Main(string[] args)
        {
            string vectorsPath = GetArgument(args, "--vectors", "../../../shared/test-vectors/nist_ff3_official_vectors.json");
            string casePrefix = GetArgument(args, "--case", "");
            bool failFast = HasArgument(args, "--fail-fast");

            try
            {
                var doc = LoadVectors(vectorsPath);
                int failures = 0;

                foreach (var v in doc.Vectors)
                {
                    string caseId = $"{v.Algorithm}#{v.Sample}";
                    if (!string.IsNullOrEmpty(casePrefix) &&
                        !caseId.StartsWith(casePrefix) &&
                        !v.Algorithm.StartsWith(casePrefix))
                    {
                        continue;
                    }

                    try
                    {
                        var key = Core.FF3.HexToBytes(v.Key);
                        var tweak = Core.FF3.HexToBytes(v.Tweak);

                        using var cipher = BuildCipher(v.Radix, key, tweak);
                        if (cipher == null)
                        {
                            Emit(caseId, "build-cipher", false, $"Unsupported radix {v.Radix}");
                            failures++;
                            if (failFast) break;
                            continue;
                        }

                        var ct = cipher.Encrypt(v.Plaintext);
                        bool match = ct == v.Ciphertext;
                        Emit(caseId, "encrypt", match, match ? null : "ciphertext mismatch");

                        var pt = cipher.Decrypt(ct);
                        bool roundTrip = pt == v.Plaintext;
                        Emit(caseId, "roundtrip", roundTrip, roundTrip ? null : "roundtrip mismatch");

                        if (!match || !roundTrip)
                        {
                            failures++;
                            if (failFast) break;
                        }
                    }
                    catch (Exception ex)
                    {
                        Emit(caseId, "error", false, ex.Message);
                        failures++;
                        if (failFast) break;
                    }
                }

                return failures > 0 ? 1 : 0;
            }
            catch (Exception ex)
            {
                Console.Error.WriteLine($"ERR load: {ex.Message}");
                return 2;
            }
        }

        static NISTDoc LoadVectors(string path)
        {
            // Try multiple possible paths
            var possiblePaths = new[]
            {
                path,
                "../../../../../../../shared/test-vectors/nist_ff3_official_vectors.json", // From bin/Debug/net8.0
                "../../../shared/test-vectors/nist_ff3_official_vectors.json",            // From project directory
                "../../shared/test-vectors/nist_ff3_official_vectors.json"
            };

            string? jsonPath = null;
            foreach (var p in possiblePaths)
            {
                if (File.Exists(p))
                {
                    jsonPath = p;
                    break;
                }
            }

            if (jsonPath == null)
            {
                throw new FileNotFoundException($"NIST test vectors not found. Tried: {string.Join(", ", possiblePaths)}");
            }

            var json = File.ReadAllText(jsonPath);
            var root = JObject.Parse(json);
            var vectors = new List<NISTVector>();

            if (root["vectors"] is JArray vectorArray)
            {
                foreach (var item in vectorArray)
                {
                    vectors.Add(new NISTVector
                    {
                        Sample = item["sample"]?.Value<int>() ?? 0,
                        Algorithm = item["algorithm"]?.Value<string>() ?? "",
                        Key = item["key"]?.Value<string>() ?? "",
                        Radix = item["radix"]?.Value<int>() ?? 0,
                        Plaintext = item["plaintext"]?.Value<string>() ?? "",
                        Tweak = item["tweak"]?.Value<string>() ?? "",
                        Ciphertext = item["ciphertext"]?.Value<string>() ?? ""
                    });
                }
            }

            return new NISTDoc
            {
                Description = root["description"]?.Value<string>() ?? "",
                Source = root["source"]?.Value<string>() ?? "",
                Vectors = vectors
            };
        }

        static Core.FF3? BuildCipher(int radix, byte[] key, byte[] tweak)
        {
            return radix switch
            {
                10 => Core.FF3.Digits(key, tweak),
                16 => Core.FF3.HexLower(key, tweak),
                26 => Core.FF3.Radix26(key, tweak),
                36 => Core.FF3.Base36Lower(key, tweak),
                62 => Core.FF3.Base62(key, tweak),
                _ when radix <= 10 => new Core.FF3(key, tweak, FF3Alphabets.Digits[..radix]),
                _ when radix <= 36 => new Core.FF3(key, tweak, FF3Alphabets.Base36Lower[..radix]),
                _ when radix <= 62 => new Core.FF3(key, tweak, FF3Alphabets.Base62[..radix]),
                _ => null
            };
        }

        static void Emit(string id, string stage, bool ok, string? msg)
        {
            var rec = new Dictionary<string, object?>
            {
                ["case_id"] = id,
                ["stage"] = stage,
                ["ok"] = ok
            };
            if (msg != null) rec["msg"] = msg;

            var json = JsonConvert.SerializeObject(rec);
            Console.WriteLine(json);
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

        static bool HasArgument(string[] args, string flag)
        {
            return Array.IndexOf(args, flag) >= 0;
        }
    }
}