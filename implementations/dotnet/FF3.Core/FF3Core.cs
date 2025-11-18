using System;
using System.Numerics;
using System.Security.Cryptography;

namespace FF3.Core
{
    /// <summary>
    /// Core FF3 cipher implementation - pure cryptographic math
    /// </summary>
    public class FF3Cipher : IDisposable
    {
        private readonly int radix;
        private readonly byte[] key;
        private readonly byte[] tweak;
        private readonly Aes aes;

        public FF3Cipher(int radix, byte[] key, byte[] tweak)
        {
            if (radix < 2 || radix > 62)
                throw new ArgumentException("Radix must be between 2 and 62");
            if (key.Length != 16 && key.Length != 24 && key.Length != 32)
                throw new ArgumentException("Key must be 16, 24, or 32 bytes");
            if (tweak.Length != 8)
                throw new ArgumentException("Tweak must be exactly 8 bytes");

            this.radix = radix;
            this.tweak = (byte[])tweak.Clone();

            // FF3 specification requires byte reversal of the key
            this.key = new byte[key.Length];
            for (int i = 0; i < key.Length; i++)
            {
                this.key[i] = key[key.Length - 1 - i];
            }

            this.aes = Aes.Create();
            this.aes.Mode = CipherMode.ECB;
            this.aes.Padding = PaddingMode.None;
            this.aes.Key = this.key;
        }

        public int[] Encrypt(int[] plaintext)
        {
            int n = plaintext.Length;
            int u = (n + 1) / 2;  // ceil(n/2)
            int v = n - u;        // floor(n/2)

            int[] A = new int[u];
            int[] B = new int[v];
            Array.Copy(plaintext, 0, A, 0, u);
            Array.Copy(plaintext, u, B, 0, v);

            // 8 Feistel rounds
            for (int i = 0; i < 8; i++)
            {
                if (i % 2 == 0)
                {
                    // Even round: use B to update A
                    byte[] W = CalculateW(tweak, i);
                    BigInteger P = CalculateP(i, W, B);
                    BigInteger m = BigInteger.Pow(radix, u);

                    // FF3 uses reversed digit order: NUM_radix(REV(A))
                    int[] reversedA = ReverseArray(A);
                    BigInteger aNum = ArrayToBigInt(reversedA);

                    // c = (NUM_radix(REV(A)) + NUM(S)) mod radix^u
                    BigInteger Y = (aNum + P) % m;

                    // C = REV(STR_radix(c))
                    int[] newDigits = BigIntToArray(Y, u);
                    A = ReverseArray(newDigits);
                }
                else
                {
                    // Odd round: use A to update B
                    byte[] W = CalculateW(tweak, i);
                    BigInteger P = CalculateP(i, W, A);
                    BigInteger m = BigInteger.Pow(radix, v);

                    int[] reversedB = ReverseArray(B);
                    BigInteger bNum = ArrayToBigInt(reversedB);

                    BigInteger Y = (bNum + P) % m;

                    int[] newDigits = BigIntToArray(Y, v);
                    B = ReverseArray(newDigits);
                }
            }

            // Combine A and B
            int[] result = new int[n];
            Array.Copy(A, 0, result, 0, u);
            Array.Copy(B, 0, result, u, v);
            return result;
        }

        public int[] Decrypt(int[] ciphertext)
        {
            int n = ciphertext.Length;
            int u = (n + 1) / 2;
            int v = n - u;

            int[] A = new int[u];
            int[] B = new int[v];
            Array.Copy(ciphertext, 0, A, 0, u);
            Array.Copy(ciphertext, u, B, 0, v);

            // 8 Feistel rounds in reverse
            for (int i = 7; i >= 0; i--)
            {
                if (i % 2 == 0)
                {
                    // Even round: use B to update A (reverse)
                    byte[] W = CalculateW(tweak, i);
                    BigInteger P = CalculateP(i, W, B);
                    BigInteger m = BigInteger.Pow(radix, u);

                    int[] reversedA = ReverseArray(A);
                    BigInteger aNum = ArrayToBigInt(reversedA);

                    // c = (NUM_radix(REV(A)) - NUM(S)) mod radix^u
                    BigInteger Y = (aNum - P) % m;
                    if (Y < 0) Y += m;

                    int[] newDigits = BigIntToArray(Y, u);
                    A = ReverseArray(newDigits);
                }
                else
                {
                    // Odd round: use A to update B (reverse)
                    byte[] W = CalculateW(tweak, i);
                    BigInteger P = CalculateP(i, W, A);
                    BigInteger m = BigInteger.Pow(radix, v);

                    int[] reversedB = ReverseArray(B);
                    BigInteger bNum = ArrayToBigInt(reversedB);

                    BigInteger Y = (bNum - P) % m;
                    if (Y < 0) Y += m;

                    int[] newDigits = BigIntToArray(Y, v);
                    B = ReverseArray(newDigits);
                }
            }

            int[] result = new int[n];
            Array.Copy(A, 0, result, 0, u);
            Array.Copy(B, 0, result, u, v);
            return result;
        }

        private byte[] CalculateW(byte[] tweak, int round)
        {
            byte[] w = new byte[4];
            if (round % 2 == 0)
            {
                // Even rounds: W = Tr (rightmost 4 bytes)
                Array.Copy(tweak, 4, w, 0, 4);
            }
            else
            {
                // Odd rounds: W = Tl (leftmost 4 bytes)
                Array.Copy(tweak, 0, w, 0, 4);
            }
            return w;
        }

        private BigInteger CalculateP(int round, byte[] w, int[] half)
        {
            // P = REVB(AES-ECB(K, REVB(W XOR [i]_4 || NUM_radix(REV(B)))))
            byte[] input = new byte[16];

            // First 4 bytes: W XOR with round number in the last byte
            Array.Copy(w, 0, input, 0, 4);
            input[3] ^= (byte)round;

            // Last 12 bytes: NUM_radix(REV(B))
            int[] reversedHalf = ReverseArray(half);
            BigInteger halfBigInt = ArrayToBigInt(reversedHalf);
            byte[] halfBytes = halfBigInt.ToByteArray();

            // Convert from little-endian to big-endian and pad to 12 bytes
            Array.Reverse(halfBytes);
            if (halfBytes.Length <= 12)
            {
                Array.Copy(halfBytes, 0, input, 16 - halfBytes.Length, halfBytes.Length);
            }
            else
            {
                // If too long, take the last 12 bytes
                Array.Copy(halfBytes, halfBytes.Length - 12, input, 4, 12);
            }

            // Apply FF3 byte reversal: REVB before AES
            byte[] reversedInput = ReverseBytes(input);

            // Encrypt with AES
            byte[] aesOutput = new byte[16];
            using (var encryptor = aes.CreateEncryptor())
            {
                encryptor.TransformBlock(reversedInput, 0, 16, aesOutput, 0);
            }

            // Apply FF3 byte reversal: REVB after AES
            byte[] output = ReverseBytes(aesOutput);

            // Convert to BigInteger (big-endian)
            Array.Reverse(output); // Convert to little-endian for BigInteger
            return new BigInteger(output, isUnsigned: true);
        }

        private int[] ReverseArray(int[] array)
        {
            int[] result = new int[array.Length];
            for (int i = 0; i < array.Length; i++)
            {
                result[i] = array[array.Length - 1 - i];
            }
            return result;
        }

        private byte[] ReverseBytes(byte[] array)
        {
            byte[] result = new byte[array.Length];
            for (int i = 0; i < array.Length; i++)
            {
                result[i] = array[array.Length - 1 - i];
            }
            return result;
        }

        private BigInteger ArrayToBigInt(int[] array)
        {
            BigInteger result = BigInteger.Zero;
            BigInteger radixBig = new BigInteger(radix);
            foreach (int digit in array)
            {
                result = result * radixBig + digit;
            }
            return result;
        }

        private int[] BigIntToArray(BigInteger num, int length)
        {
            int[] result = new int[length];
            BigInteger radixBig = new BigInteger(radix);

            for (int i = length - 1; i >= 0; i--)
            {
                result[i] = (int)(num % radixBig);
                num = num / radixBig;
            }

            return result;
        }

        public int Radix => radix;

        public void Dispose()
        {
            aes?.Dispose();
        }
    }
}