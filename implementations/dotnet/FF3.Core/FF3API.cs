using System;
using System.Text;

namespace FF3.Core
{
    /// <summary>
    /// FF3 API - string-based interface
    /// </summary>
    public class FF3 : IDisposable
    {
        private readonly FF3Cipher cipher;
        private readonly string alphabet;

        public FF3(byte[] key, byte[] tweak, string alphabet)
        {
            this.alphabet = alphabet;
            this.cipher = new FF3Cipher(alphabet.Length, key, tweak);
        }

        public string Encrypt(string plaintext)
        {
            int[] digits = StringToArray(plaintext);
            int[] encrypted = cipher.Encrypt(digits);
            return ArrayToString(encrypted);
        }

        public string Decrypt(string ciphertext)
        {
            int[] digits = StringToArray(ciphertext);
            int[] decrypted = cipher.Decrypt(digits);
            return ArrayToString(decrypted);
        }

        private int[] StringToArray(string str)
        {
            int[] result = new int[str.Length];
            for (int i = 0; i < str.Length; i++)
            {
                char c = str[i];
                int index = alphabet.IndexOf(c);
                if (index == -1)
                {
                    throw new ArgumentException($"Invalid character: {c}");
                }
                result[i] = index;
            }
            return result;
        }

        private string ArrayToString(int[] array)
        {
            StringBuilder sb = new StringBuilder();
            foreach (int digit in array)
            {
                sb.Append(alphabet[digit]);
            }
            return sb.ToString();
        }

        // Factory methods
        public static FF3 Digits(byte[] key, byte[] tweak)
        {
            return new FF3(key, tweak, FF3Alphabets.Digits);
        }

        public static FF3 HexLower(byte[] key, byte[] tweak)
        {
            return new FF3(key, tweak, FF3Alphabets.HexLower);
        }

        public static FF3 HexUpper(byte[] key, byte[] tweak)
        {
            return new FF3(key, tweak, FF3Alphabets.HexUpper);
        }

        public static FF3 Base36Lower(byte[] key, byte[] tweak)
        {
            return new FF3(key, tweak, FF3Alphabets.Base36Lower);
        }

        public static FF3 Base36Upper(byte[] key, byte[] tweak)
        {
            return new FF3(key, tweak, FF3Alphabets.Base36Upper);
        }

        public static FF3 Base62(byte[] key, byte[] tweak)
        {
            return new FF3(key, tweak, FF3Alphabets.Base62);
        }

        public static FF3 Radix26(byte[] key, byte[] tweak)
        {
            return new FF3(key, tweak, FF3Alphabets.Radix26);
        }

        public static byte[] HexToBytes(string hex)
        {
            byte[] result = new byte[hex.Length / 2];
            for (int i = 0; i < hex.Length; i += 2)
            {
                result[i / 2] = Convert.ToByte(hex.Substring(i, 2), 16);
            }
            return result;
        }

        public void Dispose()
        {
            cipher?.Dispose();
        }
    }
}