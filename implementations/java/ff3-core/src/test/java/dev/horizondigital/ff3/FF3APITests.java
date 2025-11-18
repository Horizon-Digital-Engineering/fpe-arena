package dev.horizondigital.ff3;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;
import static org.junit.jupiter.api.Assertions.*;

public class FF3APITests {

    @Test
    @DisplayName("Factory methods create correct instances")
    void testFactoryMethods() throws Exception {
        byte[] key = new byte[16];
        byte[] tweak = new byte[8];

        FF3API digits = FF3API.digits(key, tweak);
        assertEquals(FF3Alphabets.DIGITS, digits.getAlphabet());
        assertEquals(10, digits.getRadix());

        FF3API hexLower = FF3API.hexLower(key, tweak);
        assertEquals(FF3Alphabets.HEX_LOWER, hexLower.getAlphabet());
        assertEquals(16, hexLower.getRadix());

        FF3API base62 = FF3API.base62(key, tweak);
        assertEquals(FF3Alphabets.BASE62, base62.getAlphabet());
        assertEquals(62, base62.getRadix());
    }

    @Test
    @DisplayName("String to array conversion")
    void testStringConversion() throws Exception {
        byte[] key = new byte[16];
        byte[] tweak = new byte[8];
        FF3API api = new FF3API(key, tweak, "0123456789");

        // Test basic roundtrip
        String input = "1234567890";
        String encrypted = api.encrypt(input);
        String decrypted = api.decrypt(encrypted);

        assertEquals(input, decrypted);
        assertEquals(input.length(), encrypted.length());
    }

    @Test
    @DisplayName("Invalid character handling")
    void testInvalidCharacters() throws Exception {
        byte[] key = new byte[16];
        byte[] tweak = new byte[8];
        FF3API api = new FF3API(key, tweak, "0123456789");

        // Should throw on invalid character
        assertThrows(IllegalArgumentException.class, () -> api.encrypt("123A567"));
        assertThrows(IllegalArgumentException.class, () -> api.decrypt("123A567"));
    }

    @Test
    @DisplayName("All factory alphabets work")
    void testAllFactoryAlphabets() throws Exception {
        byte[] key = FF3API.hexToBytes("EF4359D8D580AA4F7F036D6F04FC6A94");
        byte[] tweak = FF3API.hexToBytes("D8E7920AFA330A73");

        // Test each factory method
        testFactoryAlphabet(FF3API.digits(key, tweak), "1234567890");
        testFactoryAlphabet(FF3API.hexLower(key, tweak), "0123456789abcdef");
        testFactoryAlphabet(FF3API.hexUpper(key, tweak), "0123456789ABCDEF");
        testFactoryAlphabet(FF3API.base36Lower(key, tweak), "0123456789abcdefghijklmnopqrstuvwxyz");
        testFactoryAlphabet(FF3API.base36Upper(key, tweak), "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ");
        testFactoryAlphabet(FF3API.base62(key, tweak), "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz");
        testFactoryAlphabet(FF3API.radix26(key, tweak), "0123456789abcdefghijklmnop");
    }

    private void testFactoryAlphabet(FF3API api, String testString) throws Exception {
        // Take valid characters from the test string that exist in alphabet
        StringBuilder validChars = new StringBuilder();
        for (char c : testString.toCharArray()) {
            if (api.getAlphabet().indexOf(c) != -1) {
                validChars.append(c);
                if (validChars.length() >= 8) break; // Get enough for testing
            }
        }

        if (validChars.length() >= 6) { // Need minimum length for FF3
            String plaintext = validChars.toString();
            String encrypted = api.encrypt(plaintext);
            String decrypted = api.decrypt(encrypted);

            assertEquals(plaintext, decrypted);
            assertEquals(plaintext.length(), encrypted.length());

            // Verify all encrypted characters are in alphabet
            for (char c : encrypted.toCharArray()) {
                assertTrue(api.getAlphabet().indexOf(c) != -1,
                        "Character '" + c + "' not found in alphabet: " + api.getAlphabet());
            }
        }
    }

    @Test
    @DisplayName("Custom alphabet support")
    void testCustomAlphabet() throws Exception {
        byte[] key = new byte[16];
        byte[] tweak = new byte[8];
        String customAlphabet = "ABCDEFGH";

        FF3API api = new FF3API(key, tweak, customAlphabet);
        assertEquals(customAlphabet, api.getAlphabet());
        assertEquals(8, api.getRadix());

        String plaintext = "ABCDEFGH";
        String encrypted = api.encrypt(plaintext);
        String decrypted = api.decrypt(encrypted);

        assertEquals(plaintext, decrypted);
        assertEquals(plaintext.length(), encrypted.length());

        // Verify all characters in result are from custom alphabet
        for (char c : encrypted.toCharArray()) {
            assertTrue(customAlphabet.indexOf(c) != -1);
        }
    }

    @Test
    @DisplayName("Hex utility function")
    void testHexToBytes() {
        // Test basic conversion
        byte[] result = FF3API.hexToBytes("00FF");
        assertArrayEquals(new byte[]{0x00, (byte) 0xFF}, result);

        // Test longer hex string
        byte[] key = FF3API.hexToBytes("EF4359D8D580AA4F7F036D6F04FC6A94");
        assertEquals(16, key.length);

        // Test empty string
        byte[] empty = FF3API.hexToBytes("");
        assertEquals(0, empty.length);
    }

    @Test
    @DisplayName("Known NIST vector through API")
    void testKnownVectorThroughAPI() throws Exception {
        // NIST test vector 1 through string API
        byte[] key = FF3API.hexToBytes("EF4359D8D580AA4F7F036D6F04FC6A94");
        byte[] tweak = FF3API.hexToBytes("D8E7920AFA330A73");

        FF3API api = FF3API.digits(key, tweak);
        String plaintext = "890121234567890000";
        String expectedCiphertext = "750918814058654607";

        String actualCiphertext = api.encrypt(plaintext);
        assertEquals(expectedCiphertext, actualCiphertext);

        // Verify decrypt works
        String decrypted = api.decrypt(actualCiphertext);
        assertEquals(plaintext, decrypted);
    }

    @Test
    @DisplayName("Format preservation")
    void testFormatPreservation() throws Exception {
        byte[] key = new byte[16];
        byte[] tweak = new byte[8];

        // Test that encryption preserves format (length and character domain)
        FF3API api = FF3API.digits(key, tweak);

        for (int length = 6; length <= 20; length++) {
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < length; i++) {
                sb.append((char) ('0' + (i % 10)));
            }
            String plaintext = sb.toString();

            String encrypted = api.encrypt(plaintext);

            // Same length
            assertEquals(plaintext.length(), encrypted.length());

            // All digits
            for (char c : encrypted.toCharArray()) {
                assertTrue(Character.isDigit(c));
            }

            // Roundtrip works
            assertEquals(plaintext, api.decrypt(encrypted));
        }
    }

    @Test
    @DisplayName("Different inputs produce different outputs")
    void testDifferentInputs() throws Exception {
        byte[] key = new byte[16];
        byte[] tweak = new byte[8];
        FF3API api = FF3API.digits(key, tweak);

        String input1 = "1234567890";
        String input2 = "1234567891";

        String encrypted1 = api.encrypt(input1);
        String encrypted2 = api.encrypt(input2);

        assertNotEquals(encrypted1, encrypted2);
    }

    @Test
    @DisplayName("Minimum length handling")
    void testMinimumLength() throws Exception {
        byte[] key = new byte[16];
        byte[] tweak = new byte[8];
        FF3API api = FF3API.digits(key, tweak);

        // Test very short strings (minimum practical length)
        String shortInput = "123456";
        String encrypted = api.encrypt(shortInput);
        String decrypted = api.decrypt(encrypted);

        assertEquals(shortInput, decrypted);
        assertEquals(6, encrypted.length());
    }

    @Test
    @DisplayName("Case sensitivity in alphabets")
    void testCaseSensitivity() throws Exception {
        byte[] key = new byte[16];
        byte[] tweak = new byte[8];

        // Test base62 which has both cases
        FF3API api = FF3API.base62(key, tweak);

        String mixedCase = "Hello123World";
        String encrypted = api.encrypt(mixedCase);
        String decrypted = api.decrypt(encrypted);

        assertEquals(mixedCase, decrypted);

        // Should preserve character domain
        for (char c : encrypted.toCharArray()) {
            assertTrue(api.getAlphabet().indexOf(c) != -1);
        }
    }

    @Test
    @DisplayName("Large string handling")
    void testLargeStrings() throws Exception {
        byte[] key = new byte[16];
        byte[] tweak = new byte[8];
        FF3API api = FF3API.digits(key, tweak);

        // Test reasonably large string
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < 100; i++) {
            sb.append(i % 10);
        }
        String largeInput = sb.toString();

        String encrypted = api.encrypt(largeInput);
        String decrypted = api.decrypt(encrypted);

        assertEquals(largeInput, decrypted);
        assertEquals(100, encrypted.length());
    }

    @Test
    @DisplayName("Alphabet validation in constructor")
    void testAlphabetValidation() throws Exception {
        byte[] key = new byte[16];
        byte[] tweak = new byte[8];

        // Empty alphabet should throw (radix 0 is invalid)
        assertThrows(IllegalArgumentException.class, () -> new FF3API(key, tweak, ""));

        // Single character alphabet should throw (radix 1 is invalid)
        assertThrows(IllegalArgumentException.class, () -> new FF3API(key, tweak, "A"));

        // Two character alphabet should work (minimum radix 2)
        assertDoesNotThrow(() -> new FF3API(key, tweak, "AB"));

        // Normal alphabets should work
        assertDoesNotThrow(() -> new FF3API(key, tweak, "0123456789"));
    }
}