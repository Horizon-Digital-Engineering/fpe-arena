using System;
using System.Collections.Generic;
using System.IO;
using Xunit;
using Newtonsoft.Json.Linq;
using FF3.Core;

namespace FF3.Tests
{
    /// <summary>
    /// Tests against official NIST SP 800-38G FF3 test vectors
    /// </summary>
    public class NistVectorTests
    {
        public class TestVector
        {
            public int Sample { get; set; }
            public string Algorithm { get; set; } = "";
            public string Key { get; set; } = "";
            public int Radix { get; set; }
            public string Plaintext { get; set; } = "";
            public string Tweak { get; set; } = "";
            public string Ciphertext { get; set; } = "";
        }

        private static List<TestVector> LoadNistVectors()
        {
            var vectors = new List<TestVector>();

            // Use environment variable to find test vectors (required for CI/CD)
            var vectorsPath = Environment.GetEnvironmentVariable("FF3_TEST_VECTORS_PATH");

            if (string.IsNullOrEmpty(vectorsPath))
            {
                throw new InvalidOperationException("FF3_TEST_VECTORS_PATH environment variable must be set to the path of nist_ff3_official_vectors.json");
            }

            if (!File.Exists(vectorsPath))
            {
                var currentDir = Directory.GetCurrentDirectory();
                throw new FileNotFoundException($"NIST test vectors not found at: {vectorsPath}. Current directory: {currentDir}");
            }

            var json = File.ReadAllText(vectorsPath);
            var root = JObject.Parse(json);
            var vectorArray = root["vectors"];

            if (vectorArray != null)
            {
                foreach (var item in vectorArray)
                {
                    var vector = new TestVector
                    {
                        Sample = item["sample"]?.Value<int>() ?? 0,
                        Algorithm = item["algorithm"]?.Value<string>() ?? "",
                        Key = item["key"]?.Value<string>() ?? "",
                        Radix = item["radix"]?.Value<int>() ?? 0,
                        Plaintext = item["plaintext"]?.Value<string>() ?? "",
                        Tweak = item["tweak"]?.Value<string>() ?? "",
                        Ciphertext = item["ciphertext"]?.Value<string>() ?? ""
                    };
                    vectors.Add(vector);
                }
            }

            return vectors;
        }

        [Fact]
        public void TestAllNistVectors()
        {
            var vectors = LoadNistVectors();
            Assert.NotEmpty(vectors);

            foreach (var vector in vectors)
            {
                // Convert hex strings to byte arrays
                var key = Convert.FromHexString(vector.Key);
                var tweak = Convert.FromHexString(vector.Tweak);

                // Create appropriate cipher based on radix
                Core.FF3 cipher = vector.Radix switch
                {
                    10 => Core.FF3.Digits(key, tweak),
                    26 => Core.FF3.Radix26(key, tweak),
                    _ => throw new NotSupportedException($"Radix {vector.Radix} not implemented in factory methods")
                };

                using (cipher)
                {
                    // Test encryption
                    var encrypted = cipher.Encrypt(vector.Plaintext);
                    Assert.Equal(vector.Ciphertext, encrypted);

                    // Test decryption (round-trip)
                    var decrypted = cipher.Decrypt(encrypted);
                    Assert.Equal(vector.Plaintext, decrypted);
                }
            }
        }

        [Fact]
        public void TestBasicEncryptionDecryption()
        {
            var key = Convert.FromHexString("EF4359D8D580AA4F7F036D6F04FC6A94");
            var tweak = Convert.FromHexString("D8E7920AFA330A73");

            using var cipher = Core.FF3.Digits(key, tweak);

            var plaintext = "1234567890";
            var encrypted = cipher.Encrypt(plaintext);
            var decrypted = cipher.Decrypt(encrypted);

            Assert.Equal(plaintext, decrypted);
            Assert.NotEqual(plaintext, encrypted);
        }

        [Fact]
        public void TestDifferentAlphabets()
        {
            var key = Convert.FromHexString("EF4359D8D580AA4F7F036D6F04FC6A94");
            var tweak = Convert.FromHexString("D8E7920AFA330A73");

            // Test each alphabet type
            using var digits = Core.FF3.Digits(key, tweak);
            var result1 = digits.Encrypt("123456");
            Assert.NotEqual("123456", result1);

            using var hex = Core.FF3.HexLower(key, tweak);
            var result2 = hex.Encrypt("abcdef");
            Assert.NotEqual("abcdef", result2);

            using var base36 = Core.FF3.Base36Lower(key, tweak);
            var result3 = base36.Encrypt("hello123");
            Assert.NotEqual("hello123", result3);
        }
    }
}