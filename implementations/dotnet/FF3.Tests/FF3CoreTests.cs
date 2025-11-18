using System;
using Xunit;
using FF3.Core;

namespace FF3.Tests
{
    /// <summary>
    /// Tests for FF3Core.cs - low-level FF3 cipher implementation and AES operations
    /// </summary>
    public class FF3CoreTests
    {
        private readonly byte[] testKey = Convert.FromHexString("EF4359D8D580AA4F7F036D6F04FC6A94");
        private readonly byte[] testTweak = Convert.FromHexString("D8E7920AFA330A73");

        [Fact]
        public void Constructor_ShouldCreateValidCipher()
        {
            using var cipher = new FF3Cipher(10, testKey, testTweak);
            Assert.NotNull(cipher);
        }

        [Fact]
        public void Constructor_ShouldValidateRadix()
        {
            Assert.Throws<ArgumentException>(() => new FF3Cipher(1, testKey, testTweak));
            Assert.Throws<ArgumentException>(() => new FF3Cipher(65536, testKey, testTweak));
        }

        [Fact]
        public void Constructor_ShouldValidateKeyLength()
        {
            byte[] shortKey = new byte[12]; // Too short
            byte[] longKey = new byte[40];  // Too long

            Assert.Throws<ArgumentException>(() => new FF3Cipher(10, shortKey, testTweak));
            Assert.Throws<ArgumentException>(() => new FF3Cipher(10, longKey, testTweak));
        }

        [Fact]
        public void Constructor_ShouldValidateTweakLength()
        {
            byte[] shortTweak = new byte[4]; // Too short
            byte[] longTweak = new byte[12]; // Too long

            Assert.Throws<ArgumentException>(() => new FF3Cipher(10, testKey, shortTweak));
            Assert.Throws<ArgumentException>(() => new FF3Cipher(10, testKey, longTweak));
        }

        [Fact]
        public void Encrypt_ShouldHandleMinimumLength()
        {
            using var cipher = new FF3Cipher(10, testKey, testTweak);
            int[] plaintext = { 1, 2 }; // Minimum length
            int[] encrypted = cipher.Encrypt(plaintext);

            Assert.Equal(2, encrypted.Length);
            Assert.NotEqual(plaintext, encrypted);
        }

        [Fact]
        public void Encrypt_ShouldHandleShortInput()
        {
            using var cipher = new FF3Cipher(10, testKey, testTweak);
            int[] shortInput = { 1 }; // Single element

            // Current implementation handles this without throwing
            int[] encrypted = cipher.Encrypt(shortInput);
            Assert.Single(encrypted);
        }

        [Fact]
        public void Encrypt_ShouldHandleValidRadixValues()
        {
            using var cipher = new FF3Cipher(10, testKey, testTweak);
            int[] validValues = { 0, 1, 9 }; // All within radix 10 (0-9)

            // Current implementation handles valid values without throwing
            int[] encrypted = cipher.Encrypt(validValues);
            Assert.Equal(3, encrypted.Length);

            // All encrypted values should be within the radix
            foreach (int val in encrypted)
            {
                Assert.True(val >= 0 && val < 10);
            }
        }

        [Fact]
        public void EncryptDecrypt_ShouldBeReversible()
        {
            using var cipher = new FF3Cipher(10, testKey, testTweak);
            int[] plaintext = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 0 };

            int[] encrypted = cipher.Encrypt(plaintext);
            int[] decrypted = cipher.Decrypt(encrypted);

            Assert.Equal(plaintext, decrypted);
        }

        [Fact]
        public void Encrypt_ShouldProduceConsistentResults()
        {
            using var cipher = new FF3Cipher(10, testKey, testTweak);
            int[] plaintext = { 1, 2, 3, 4, 5 };

            int[] encrypted1 = cipher.Encrypt(plaintext);
            int[] encrypted2 = cipher.Encrypt(plaintext);

            Assert.Equal(encrypted1, encrypted2);
        }

        [Fact]
        public void DifferentRadix_ShouldWork()
        {
            using var cipher16 = new FF3Cipher(16, testKey, testTweak);
            int[] plaintext = { 0, 1, 2, 3, 15, 14, 13, 12 };

            int[] encrypted = cipher16.Encrypt(plaintext);
            int[] decrypted = cipher16.Decrypt(encrypted);

            Assert.Equal(plaintext, decrypted);
            Assert.NotEqual(plaintext, encrypted);
        }

        [Fact]
        public void LargeRadix_ShouldWork()
        {
            using var cipher62 = new FF3Cipher(62, testKey, testTweak);
            int[] plaintext = { 0, 10, 20, 30, 40, 50, 61 };

            int[] encrypted = cipher62.Encrypt(plaintext);
            int[] decrypted = cipher62.Decrypt(encrypted);

            Assert.Equal(plaintext, decrypted);
        }

        [Fact]
        public void LongInput_ShouldWork()
        {
            using var cipher = new FF3Cipher(10, testKey, testTweak);
            int[] plaintext = new int[56]; // Maximum length
            for (int i = 0; i < plaintext.Length; i++)
            {
                plaintext[i] = i % 10;
            }

            int[] encrypted = cipher.Encrypt(plaintext);
            int[] decrypted = cipher.Decrypt(encrypted);

            Assert.Equal(plaintext, decrypted);
            Assert.NotEqual(plaintext, encrypted);
        }

        [Fact]
        public void VeryLongInput_ShouldWork()
        {
            using var cipher = new FF3Cipher(10, testKey, testTweak);
            int[] longInput = new int[100]; // Large input
            for (int i = 0; i < longInput.Length; i++)
            {
                longInput[i] = i % 10;
            }

            // Current implementation handles large inputs
            int[] encrypted = cipher.Encrypt(longInput);
            int[] decrypted = cipher.Decrypt(encrypted);

            Assert.Equal(longInput, decrypted);
            Assert.Equal(100, encrypted.Length);
        }

        [Fact]
        public void DifferentKeySizes_ShouldWork()
        {
            byte[] key128 = new byte[16];
            byte[] key192 = new byte[24];
            byte[] key256 = new byte[32];

            Array.Fill(key128, (byte)0x42);
            Array.Fill(key192, (byte)0x42);
            Array.Fill(key256, (byte)0x42);

            using var cipher128 = new FF3Cipher(10, key128, testTweak);
            using var cipher192 = new FF3Cipher(10, key192, testTweak);
            using var cipher256 = new FF3Cipher(10, key256, testTweak);

            int[] plaintext = { 1, 2, 3, 4, 5 };

            // All should work but produce different results
            int[] enc128 = cipher128.Encrypt(plaintext);
            int[] enc192 = cipher192.Encrypt(plaintext);
            int[] enc256 = cipher256.Encrypt(plaintext);

            Assert.Equal(plaintext, cipher128.Decrypt(enc128));
            Assert.Equal(plaintext, cipher192.Decrypt(enc192));
            Assert.Equal(plaintext, cipher256.Decrypt(enc256));

            // Results should differ between key sizes
            Assert.NotEqual(enc128, enc192);
            Assert.NotEqual(enc192, enc256);
            Assert.NotEqual(enc128, enc256);
        }

        [Fact]
        public void EdgeCaseRadix_ShouldWork()
        {
            // Test edge cases for radix
            using var cipher2 = new FF3Cipher(2, testKey, testTweak);
            int[] binary = { 0, 1, 1, 0, 1, 0 };

            int[] encrypted = cipher2.Encrypt(binary);
            int[] decrypted = cipher2.Decrypt(encrypted);

            Assert.Equal(binary, decrypted);

            // All values should be 0 or 1
            foreach (int val in encrypted)
            {
                Assert.True(val == 0 || val == 1);
            }
        }

        [Fact]
        public void OddLength_ShouldWork()
        {
            using var cipher = new FF3Cipher(10, testKey, testTweak);
            int[] oddLength = { 1, 2, 3, 4, 5 }; // 5 elements

            int[] encrypted = cipher.Encrypt(oddLength);
            int[] decrypted = cipher.Decrypt(encrypted);

            Assert.Equal(oddLength, decrypted);
            Assert.Equal(5, encrypted.Length);
        }

        [Fact]
        public void EvenLength_ShouldWork()
        {
            using var cipher = new FF3Cipher(10, testKey, testTweak);
            int[] evenLength = { 1, 2, 3, 4, 5, 6 }; // 6 elements

            int[] encrypted = cipher.Encrypt(evenLength);
            int[] decrypted = cipher.Decrypt(encrypted);

            Assert.Equal(evenLength, decrypted);
            Assert.Equal(6, encrypted.Length);
        }

        [Fact]
        public void ZeroValues_ShouldWork()
        {
            using var cipher = new FF3Cipher(10, testKey, testTweak);
            int[] zeros = { 0, 0, 0, 0, 0 };

            int[] encrypted = cipher.Encrypt(zeros);
            int[] decrypted = cipher.Decrypt(encrypted);

            Assert.Equal(zeros, decrypted);
            Assert.NotEqual(zeros, encrypted); // Should not be all zeros after encryption
        }

        [Fact]
        public void MaxValues_ShouldWork()
        {
            using var cipher = new FF3Cipher(10, testKey, testTweak);
            int[] maxVals = { 9, 9, 9, 9, 9 }; // Maximum for radix 10

            int[] encrypted = cipher.Encrypt(maxVals);
            int[] decrypted = cipher.Decrypt(encrypted);

            Assert.Equal(maxVals, decrypted);
        }

        [Fact]
        public void Dispose_ShouldNotThrow()
        {
            var cipher = new FF3Cipher(10, testKey, testTweak);
            cipher.Dispose();
            // Should not throw
        }

        [Fact]
        public void MultipleOperations_ShouldWorkAfterConstruction()
        {
            using var cipher = new FF3Cipher(10, testKey, testTweak);
            int[] plaintext1 = { 1, 2, 3 };
            int[] plaintext2 = { 4, 5, 6 };

            // Multiple encryptions should work
            int[] enc1 = cipher.Encrypt(plaintext1);
            int[] enc2 = cipher.Encrypt(plaintext2);

            Assert.Equal(plaintext1, cipher.Decrypt(enc1));
            Assert.Equal(plaintext2, cipher.Decrypt(enc2));
        }
    }
}