package dev.horizondigital.ff3;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;
import static org.junit.jupiter.api.Assertions.*;

public class FF3CoreTests {

    @Test
    @DisplayName("Constructor validates radix range")
    void testConstructorValidatesRadix() {
        byte[] key = new byte[16];
        byte[] tweak = new byte[8];

        // Valid radix values should work
        assertDoesNotThrow(() -> new FF3Core(2, key, tweak));
        assertDoesNotThrow(() -> new FF3Core(10, key, tweak));
        assertDoesNotThrow(() -> new FF3Core(62, key, tweak));

        // Invalid radix values should throw
        assertThrows(IllegalArgumentException.class, () -> new FF3Core(1, key, tweak));
        assertThrows(IllegalArgumentException.class, () -> new FF3Core(63, key, tweak));
    }

    @Test
    @DisplayName("Constructor validates key length")
    void testConstructorValidatesKeyLength() {
        byte[] tweak = new byte[8];

        // Valid key lengths should work
        assertDoesNotThrow(() -> new FF3Core(10, new byte[16], tweak));
        assertDoesNotThrow(() -> new FF3Core(10, new byte[24], tweak));
        assertDoesNotThrow(() -> new FF3Core(10, new byte[32], tweak));

        // Invalid key lengths should throw
        assertThrows(IllegalArgumentException.class, () -> new FF3Core(10, new byte[15], tweak));
        assertThrows(IllegalArgumentException.class, () -> new FF3Core(10, new byte[17], tweak));
        assertThrows(IllegalArgumentException.class, () -> new FF3Core(10, new byte[33], tweak));
    }

    @Test
    @DisplayName("Constructor validates tweak length")
    void testConstructorValidatesTweakLength() {
        byte[] key = new byte[16];

        // Valid tweak length should work
        assertDoesNotThrow(() -> new FF3Core(10, key, new byte[8]));

        // Invalid tweak lengths should throw
        assertThrows(IllegalArgumentException.class, () -> new FF3Core(10, key, new byte[7]));
        assertThrows(IllegalArgumentException.class, () -> new FF3Core(10, key, new byte[9]));
    }

    @Test
    @DisplayName("Basic encrypt/decrypt roundtrip")
    void testBasicRoundtrip() throws Exception {
        byte[] key = hexToBytes("EF4359D8D580AA4F7F036D6F04FC6A94");
        byte[] tweak = hexToBytes("D8E7920AFA330A73");
        FF3Core cipher = new FF3Core(10, key, tweak);

        int[] plaintext = {8, 9, 0, 1, 2, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 0, 0, 0};
        int[] encrypted = cipher.encrypt(plaintext);
        int[] decrypted = cipher.decrypt(encrypted);

        assertArrayEquals(plaintext, decrypted);
        assertFalse(java.util.Arrays.equals(plaintext, encrypted)); // Should actually encrypt
    }

    @Test
    @DisplayName("Known test vector validation")
    void testKnownVector() throws Exception {
        // NIST test vector 1
        byte[] key = hexToBytes("EF4359D8D580AA4F7F036D6F04FC6A94");
        byte[] tweak = hexToBytes("D8E7920AFA330A73");
        FF3Core cipher = new FF3Core(10, key, tweak);

        int[] plaintext = {8, 9, 0, 1, 2, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 0, 0, 0};
        int[] expectedCiphertext = {7, 5, 0, 9, 1, 8, 8, 1, 4, 0, 5, 8, 6, 5, 4, 6, 0, 7};

        int[] actualCiphertext = cipher.encrypt(plaintext);
        assertArrayEquals(expectedCiphertext, actualCiphertext);
    }

    @Test
    @DisplayName("Multiple radix support")
    void testMultipleRadix() throws Exception {
        byte[] key = new byte[16];
        byte[] tweak = new byte[8];

        // Test various radix values
        for (int radix : new int[]{2, 8, 10, 16, 26, 36, 62}) {
            FF3Core cipher = new FF3Core(radix, key, tweak);
            assertEquals(radix, cipher.getRadix());

            // Test with simple data
            int[] plaintext = new int[8];
            for (int i = 0; i < plaintext.length; i++) {
                plaintext[i] = i % radix;
            }

            int[] encrypted = cipher.encrypt(plaintext);
            int[] decrypted = cipher.decrypt(encrypted);

            assertArrayEquals(plaintext, decrypted);

            // Verify all digits are within radix range
            for (int digit : encrypted) {
                assertTrue(digit >= 0 && digit < radix);
            }
        }
    }

    @Test
    @DisplayName("Different input lengths")
    void testDifferentLengths() throws Exception {
        byte[] key = new byte[16];
        byte[] tweak = new byte[8];
        FF3Core cipher = new FF3Core(10, key, tweak);

        // Test various lengths (FF3 should handle any reasonable length)
        for (int length : new int[]{6, 8, 10, 12, 16, 20, 24}) {
            int[] plaintext = new int[length];
            for (int i = 0; i < length; i++) {
                plaintext[i] = i % 10;
            }

            int[] encrypted = cipher.encrypt(plaintext);
            int[] decrypted = cipher.decrypt(encrypted);

            assertEquals(length, encrypted.length);
            assertArrayEquals(plaintext, decrypted);
        }
    }

    @Test
    @DisplayName("Key reversal implementation")
    void testKeyReversal() throws Exception {
        // FF3 spec requires key reversal
        byte[] originalKey = hexToBytes("000102030405060708090A0B0C0D0E0F");
        byte[] tweak = new byte[8];

        FF3Core cipher = new FF3Core(10, originalKey, tweak);
        int[] plaintext = {1, 2, 3, 4, 5, 6, 7, 8};

        // This should work without throwing (key reversal handled internally)
        int[] encrypted = cipher.encrypt(plaintext);
        int[] decrypted = cipher.decrypt(encrypted);
        assertArrayEquals(plaintext, decrypted);
    }

    @Test
    @DisplayName("Empty input validation")
    void testEmptyInput() throws Exception {
        byte[] key = new byte[16];
        byte[] tweak = new byte[8];
        FF3Core cipher = new FF3Core(10, key, tweak);

        int[] empty = new int[0];
        int[] encrypted = cipher.encrypt(empty);
        assertEquals(0, encrypted.length);

        int[] decrypted = cipher.decrypt(empty);
        assertEquals(0, decrypted.length);
    }

    @Test
    @DisplayName("Single character input")
    void testSingleCharacter() throws Exception {
        byte[] key = new byte[16];
        byte[] tweak = new byte[8];
        FF3Core cipher = new FF3Core(10, key, tweak);

        int[] single = {5};
        int[] encrypted = cipher.encrypt(single);
        int[] decrypted = cipher.decrypt(encrypted);

        assertEquals(1, encrypted.length);
        assertArrayEquals(single, decrypted);
    }

    @Test
    @DisplayName("Large input handling")
    void testLargeInput() throws Exception {
        byte[] key = new byte[16];
        byte[] tweak = new byte[8];
        FF3Core cipher = new FF3Core(10, key, tweak);

        // Test with reasonably large input
        int[] largeInput = new int[100];
        for (int i = 0; i < largeInput.length; i++) {
            largeInput[i] = i % 10;
        }

        int[] encrypted = cipher.encrypt(largeInput);
        int[] decrypted = cipher.decrypt(encrypted);

        assertEquals(100, encrypted.length);
        assertArrayEquals(largeInput, decrypted);
    }

    @Test
    @DisplayName("Deterministic encryption")
    void testDeterministicEncryption() throws Exception {
        byte[] key = new byte[16];
        byte[] tweak = new byte[8];
        FF3Core cipher = new FF3Core(10, key, tweak);

        int[] plaintext = {1, 2, 3, 4, 5, 6, 7, 8};

        // Multiple encryptions should produce same result
        int[] encrypted1 = cipher.encrypt(plaintext);
        int[] encrypted2 = cipher.encrypt(plaintext);

        assertArrayEquals(encrypted1, encrypted2);
    }

    @Test
    @DisplayName("Different keys produce different results")
    void testDifferentKeys() throws Exception {
        byte[] key1 = new byte[16];
        byte[] key2 = new byte[16];
        key2[0] = 1; // Make keys different
        byte[] tweak = new byte[8];

        FF3Core cipher1 = new FF3Core(10, key1, tweak);
        FF3Core cipher2 = new FF3Core(10, key2, tweak);

        int[] plaintext = {1, 2, 3, 4, 5, 6, 7, 8};

        int[] encrypted1 = cipher1.encrypt(plaintext);
        int[] encrypted2 = cipher2.encrypt(plaintext);

        assertFalse(java.util.Arrays.equals(encrypted1, encrypted2));
    }

    @Test
    @DisplayName("Different tweaks produce different results")
    void testDifferentTweaks() throws Exception {
        byte[] key = new byte[16];
        byte[] tweak1 = new byte[8];
        byte[] tweak2 = new byte[8];
        tweak2[0] = 1; // Make tweaks different

        FF3Core cipher1 = new FF3Core(10, key, tweak1);
        FF3Core cipher2 = new FF3Core(10, key, tweak2);

        int[] plaintext = {1, 2, 3, 4, 5, 6, 7, 8};

        int[] encrypted1 = cipher1.encrypt(plaintext);
        int[] encrypted2 = cipher2.encrypt(plaintext);

        assertFalse(java.util.Arrays.equals(encrypted1, encrypted2));
    }

    private static byte[] hexToBytes(String hex) {
        byte[] result = new byte[hex.length() / 2];
        for (int i = 0; i < hex.length(); i += 2) {
            result[i / 2] = (byte) Integer.parseInt(hex.substring(i, i + 2), 16);
        }
        return result;
    }
}