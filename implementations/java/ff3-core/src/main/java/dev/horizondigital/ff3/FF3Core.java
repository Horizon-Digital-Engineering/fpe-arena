package dev.horizondigital.ff3;

import javax.crypto.Cipher;
import javax.crypto.spec.SecretKeySpec;
import java.math.BigInteger;
import java.util.Arrays;

/**
 * Core FF3 cipher implementation - pure cryptographic math
 */
public class FF3Core {
    private final int radix;
    private final byte[] key;
    private final byte[] tweak;
    private final Cipher aes;

    public FF3Core(int radix, byte[] key, byte[] tweak) throws Exception {
        if (radix < 2 || radix > 62) {
            throw new IllegalArgumentException("Radix must be between 2 and 62");
        }
        if (key.length != 16 && key.length != 24 && key.length != 32) {
            throw new IllegalArgumentException("Key must be 16, 24, or 32 bytes");
        }
        if (tweak.length != 8) {
            throw new IllegalArgumentException("Tweak must be exactly 8 bytes");
        }

        this.radix = radix;
        this.tweak = tweak.clone();

        // FF3 specification requires byte reversal of the key
        this.key = new byte[key.length];
        for (int i = 0; i < key.length; i++) {
            this.key[i] = key[key.length - 1 - i];
        }

        this.aes = Cipher.getInstance("AES/ECB/NoPadding");
    }

    public int[] encrypt(int[] plaintext) throws Exception {
        int n = plaintext.length;
        int u = (n + 1) / 2;  // ceil(n/2)
        int v = n - u;        // floor(n/2)

        int[] A = Arrays.copyOfRange(plaintext, 0, u);
        int[] B = Arrays.copyOfRange(plaintext, u, n);

        // 8 Feistel rounds
        for (int i = 0; i < 8; i++) {
            if (i % 2 == 0) {
                // Even round: use B to update A
                byte[] W = calculateW(tweak, i, B);
                BigInteger P = calculateP(i, W, B);
                BigInteger m = BigInteger.valueOf(radix).pow(u);

                // FF3 uses reversed digit order: NUM_radix(REV(A))
                int[] reversedA = reverseArray(A);
                BigInteger aNum = arrayToBigInt(reversedA);

                // c = (NUM_radix(REV(A)) + NUM(S)) mod radix^u
                BigInteger Y = aNum.add(P).mod(m);

                // C = REV(STR_radix(c))
                int[] newDigits = bigIntToArray(Y, u);
                A = reverseArray(newDigits);
            } else {
                // Odd round: use A to update B
                byte[] W = calculateW(tweak, i, A);
                BigInteger P = calculateP(i, W, A);
                BigInteger m = BigInteger.valueOf(radix).pow(v);

                int[] reversedB = reverseArray(B);
                BigInteger bNum = arrayToBigInt(reversedB);

                BigInteger Y = bNum.add(P).mod(m);

                int[] newDigits = bigIntToArray(Y, v);
                B = reverseArray(newDigits);
            }
        }

        // Combine A and B
        int[] result = new int[n];
        System.arraycopy(A, 0, result, 0, u);
        System.arraycopy(B, 0, result, u, v);
        return result;
    }

    public int[] decrypt(int[] ciphertext) throws Exception {
        int n = ciphertext.length;
        int u = (n + 1) / 2;
        int v = n - u;

        int[] A = Arrays.copyOfRange(ciphertext, 0, u);
        int[] B = Arrays.copyOfRange(ciphertext, u, n);

        // 8 Feistel rounds in reverse
        for (int i = 7; i >= 0; i--) {
            if (i % 2 == 0) {
                // Even round: use B to update A (reverse)
                byte[] W = calculateW(tweak, i, B);
                BigInteger P = calculateP(i, W, B);
                BigInteger m = BigInteger.valueOf(radix).pow(u);

                int[] reversedA = reverseArray(A);
                BigInteger aNum = arrayToBigInt(reversedA);

                // c = (NUM_radix(REV(A)) - NUM(S)) mod radix^u
                BigInteger Y = aNum.subtract(P).mod(m);

                int[] newDigits = bigIntToArray(Y, u);
                A = reverseArray(newDigits);
            } else {
                // Odd round: use A to update B (reverse)
                byte[] W = calculateW(tweak, i, A);
                BigInteger P = calculateP(i, W, A);
                BigInteger m = BigInteger.valueOf(radix).pow(v);

                int[] reversedB = reverseArray(B);
                BigInteger bNum = arrayToBigInt(reversedB);

                BigInteger Y = bNum.subtract(P).mod(m);

                int[] newDigits = bigIntToArray(Y, v);
                B = reverseArray(newDigits);
            }
        }

        int[] result = new int[n];
        System.arraycopy(A, 0, result, 0, u);
        System.arraycopy(B, 0, result, u, v);
        return result;
    }

    private byte[] calculateW(byte[] tweak, int round, int[] half) {
        byte[] w = new byte[4];
        if (round % 2 == 0) {
            // Even rounds: W = Tr (rightmost 4 bytes)
            System.arraycopy(tweak, 4, w, 0, 4);
        } else {
            // Odd rounds: W = Tl (leftmost 4 bytes)
            System.arraycopy(tweak, 0, w, 0, 4);
        }
        return w;
    }

    private BigInteger calculateP(int round, byte[] w, int[] half) throws Exception {
        // P = REVB(AES-ECB(K, REVB(W XOR [i]_4 || NUM_radix(REV(B)))))
        byte[] input = new byte[16];

        // First 4 bytes: W XOR with round number in the last byte
        System.arraycopy(w, 0, input, 0, 4);
        input[3] ^= (byte) round;

        // Last 12 bytes: NUM_radix(REV(B))
        int[] reversedHalf = reverseArray(half);
        BigInteger halfBigInt = arrayToBigInt(reversedHalf);
        byte[] halfBytes = halfBigInt.toByteArray();

        // Pad to 12 bytes (big-endian)
        if (halfBytes.length <= 12) {
            System.arraycopy(halfBytes, 0, input, 16 - halfBytes.length, halfBytes.length);
        } else {
            // If too long, take the last 12 bytes
            System.arraycopy(halfBytes, halfBytes.length - 12, input, 4, 12);
        }

        // Apply FF3 byte reversal: REVB before AES
        byte[] reversedInput = reverseBytes(input);

        // Encrypt with AES
        SecretKeySpec keySpec = new SecretKeySpec(key, "AES");
        aes.init(Cipher.ENCRYPT_MODE, keySpec);
        byte[] aesOutput = aes.doFinal(reversedInput);

        // Apply FF3 byte reversal: REVB after AES
        byte[] output = reverseBytes(aesOutput);

        // Convert to BigInteger
        return new BigInteger(1, output);
    }

    private int[] reverseArray(int[] array) {
        int[] result = new int[array.length];
        for (int i = 0; i < array.length; i++) {
            result[i] = array[array.length - 1 - i];
        }
        return result;
    }

    private byte[] reverseBytes(byte[] array) {
        byte[] result = new byte[array.length];
        for (int i = 0; i < array.length; i++) {
            result[i] = array[array.length - 1 - i];
        }
        return result;
    }

    private BigInteger arrayToBigInt(int[] array) {
        BigInteger result = BigInteger.ZERO;
        BigInteger radixBig = BigInteger.valueOf(radix);
        for (int digit : array) {
            result = result.multiply(radixBig).add(BigInteger.valueOf(digit));
        }
        return result;
    }

    private int[] bigIntToArray(BigInteger num, int length) {
        int[] result = new int[length];
        BigInteger radixBig = BigInteger.valueOf(radix);

        for (int i = length - 1; i >= 0; i--) {
            result[i] = num.mod(radixBig).intValue();
            num = num.divide(radixBig);
        }

        return result;
    }

    public int getRadix() {
        return radix;
    }
}