using System;
using Xunit;
using FF3.Core;

namespace FF3.Tests
{
    /// <summary>
    /// Tests for FF3API.cs - public string-based interface and factory methods
    /// </summary>
    public class FF3APITests
    {
        private readonly byte[] testKey = Convert.FromHexString("EF4359D8D580AA4F7F036D6F04FC6A94");
        private readonly byte[] testTweak = Convert.FromHexString("D8E7920AFA330A73");

        [Fact]
        public void Constructor_ShouldCreateValidInstance()
        {
            using var ff3 = new Core.FF3(testKey, testTweak, FF3Alphabets.Digits);
            Assert.NotNull(ff3);
        }

        [Fact]
        public void Encrypt_ShouldReturnDifferentValue()
        {
            using var ff3 = new Core.FF3(testKey, testTweak, FF3Alphabets.Digits);
            string plaintext = "1234567890";
            string encrypted = ff3.Encrypt(plaintext);

            Assert.NotEqual(plaintext, encrypted);
            Assert.Equal(plaintext.Length, encrypted.Length);
        }

        [Fact]
        public void Decrypt_ShouldReturnOriginalValue()
        {
            using var ff3 = new Core.FF3(testKey, testTweak, FF3Alphabets.Digits);
            string plaintext = "1234567890";
            string encrypted = ff3.Encrypt(plaintext);
            string decrypted = ff3.Decrypt(encrypted);

            Assert.Equal(plaintext, decrypted);
        }

        [Fact]
        public void StringToArray_ShouldHandleInvalidCharacter()
        {
            using var ff3 = new Core.FF3(testKey, testTweak, FF3Alphabets.Digits);

            // Try to encrypt a string with character not in alphabet
            Assert.Throws<ArgumentException>(() => ff3.Encrypt("123A"));
        }

        [Fact]
        public void DigitsFactory_ShouldCreateWorkingCipher()
        {
            using var ff3 = Core.FF3.Digits(testKey, testTweak);
            string plaintext = "9876543210";
            string encrypted = ff3.Encrypt(plaintext);
            string decrypted = ff3.Decrypt(encrypted);

            Assert.Equal(plaintext, decrypted);
            Assert.NotEqual(plaintext, encrypted);
        }

        [Fact]
        public void HexLowerFactory_ShouldCreateWorkingCipher()
        {
            using var ff3 = Core.FF3.HexLower(testKey, testTweak);
            string plaintext = "abcdef0123";
            string encrypted = ff3.Encrypt(plaintext);
            string decrypted = ff3.Decrypt(encrypted);

            Assert.Equal(plaintext, decrypted);
            Assert.NotEqual(plaintext, encrypted);
        }

        [Fact]
        public void HexUpperFactory_ShouldCreateWorkingCipher()
        {
            using var ff3 = Core.FF3.HexUpper(testKey, testTweak);
            string plaintext = "ABCDEF0123";
            string encrypted = ff3.Encrypt(plaintext);
            string decrypted = ff3.Decrypt(encrypted);

            Assert.Equal(plaintext, decrypted);
            Assert.NotEqual(plaintext, encrypted);
        }

        [Fact]
        public void Base36LowerFactory_ShouldCreateWorkingCipher()
        {
            using var ff3 = Core.FF3.Base36Lower(testKey, testTweak);
            string plaintext = "hello123world";
            string encrypted = ff3.Encrypt(plaintext);
            string decrypted = ff3.Decrypt(encrypted);

            Assert.Equal(plaintext, decrypted);
            Assert.NotEqual(plaintext, encrypted);
        }

        [Fact]
        public void Base36UpperFactory_ShouldCreateWorkingCipher()
        {
            using var ff3 = Core.FF3.Base36Upper(testKey, testTweak);
            string plaintext = "HELLO123WORLD";
            string encrypted = ff3.Encrypt(plaintext);
            string decrypted = ff3.Decrypt(encrypted);

            Assert.Equal(plaintext, decrypted);
            Assert.NotEqual(plaintext, encrypted);
        }

        [Fact]
        public void Base62Factory_ShouldCreateWorkingCipher()
        {
            using var ff3 = Core.FF3.Base62(testKey, testTweak);
            string plaintext = "Hello123World";
            string encrypted = ff3.Encrypt(plaintext);
            string decrypted = ff3.Decrypt(encrypted);

            Assert.Equal(plaintext, decrypted);
            Assert.NotEqual(plaintext, encrypted);
        }

        [Fact]
        public void Radix26Factory_ShouldCreateWorkingCipher()
        {
            using var ff3 = Core.FF3.Radix26(testKey, testTweak);
            string plaintext = "0123456789abcdefghijklmnop";
            string encrypted = ff3.Encrypt(plaintext);
            string decrypted = ff3.Decrypt(encrypted);

            Assert.Equal(plaintext, decrypted);
            Assert.NotEqual(plaintext, encrypted);
        }

        [Fact]
        public void HexToBytes_ShouldConvertCorrectly()
        {
            string hex = "EF4359D8";
            byte[] expected = { 0xEF, 0x43, 0x59, 0xD8 };
            byte[] actual = Core.FF3.HexToBytes(hex);

            Assert.Equal(expected, actual);
        }

        [Fact]
        public void HexToBytes_ShouldHandleLowercase()
        {
            string hex = "abcdef";
            byte[] expected = { 0xAB, 0xCD, 0xEF };
            byte[] actual = Core.FF3.HexToBytes(hex);

            Assert.Equal(expected, actual);
        }

        [Fact]
        public void EncryptionConsistency_ShouldProduceSameResults()
        {
            string plaintext = "1234567890";

            using var ff3a = Core.FF3.Digits(testKey, testTweak);
            using var ff3b = Core.FF3.Digits(testKey, testTweak);

            string encrypted1 = ff3a.Encrypt(plaintext);
            string encrypted2 = ff3b.Encrypt(plaintext);

            Assert.Equal(encrypted1, encrypted2);
        }

        [Fact]
        public void DifferentKeys_ShouldProduceDifferentResults()
        {
            byte[] key2 = Convert.FromHexString("0123456789ABCDEF0123456789ABCDEF");
            string plaintext = "1234567890";

            using var ff3a = Core.FF3.Digits(testKey, testTweak);
            using var ff3b = Core.FF3.Digits(key2, testTweak);

            string encrypted1 = ff3a.Encrypt(plaintext);
            string encrypted2 = ff3b.Encrypt(plaintext);

            Assert.NotEqual(encrypted1, encrypted2);
        }

        [Fact]
        public void DifferentTweaks_ShouldProduceDifferentResults()
        {
            byte[] tweak2 = Convert.FromHexString("0123456789ABCDEF");
            string plaintext = "1234567890";

            using var ff3a = Core.FF3.Digits(testKey, testTweak);
            using var ff3b = Core.FF3.Digits(testKey, tweak2);

            string encrypted1 = ff3a.Encrypt(plaintext);
            string encrypted2 = ff3b.Encrypt(plaintext);

            Assert.NotEqual(encrypted1, encrypted2);
        }

        [Fact]
        public void CustomAlphabet_ShouldWork()
        {
            string customAlphabet = "ABCDEFGH"; // 8 characters
            using var ff3 = new Core.FF3(testKey, testTweak, customAlphabet);

            string plaintext = "ABCDEFGH";
            string encrypted = ff3.Encrypt(plaintext);
            string decrypted = ff3.Decrypt(encrypted);

            Assert.Equal(plaintext, decrypted);
            Assert.NotEqual(plaintext, encrypted);

            // All characters should be from the custom alphabet
            foreach (char c in encrypted)
            {
                Assert.Contains(c, customAlphabet);
            }
        }

        [Fact]
        public void Dispose_ShouldNotThrow()
        {
            var ff3 = Core.FF3.Digits(testKey, testTweak);
            ff3.Dispose();
            // Should not throw
        }
    }
}