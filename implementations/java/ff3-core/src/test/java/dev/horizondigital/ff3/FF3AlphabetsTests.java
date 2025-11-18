package dev.horizondigital.ff3;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;
import static org.junit.jupiter.api.Assertions.*;

public class FF3AlphabetsTests {

    @Test
    @DisplayName("Digits alphabet is correct")
    void testDigitsAlphabet() {
        assertEquals("0123456789", FF3Alphabets.DIGITS);
        assertEquals(10, FF3Alphabets.DIGITS.length());

        // Verify all expected characters are present
        for (char c = '0'; c <= '9'; c++) {
            assertTrue(FF3Alphabets.DIGITS.indexOf(c) != -1);
        }
    }

    @Test
    @DisplayName("Hex lower alphabet is correct")
    void testHexLowerAlphabet() {
        assertEquals("0123456789abcdef", FF3Alphabets.HEX_LOWER);
        assertEquals(16, FF3Alphabets.HEX_LOWER.length());

        // Verify all expected characters are present
        for (char c = '0'; c <= '9'; c++) {
            assertTrue(FF3Alphabets.HEX_LOWER.indexOf(c) != -1);
        }
        for (char c = 'a'; c <= 'f'; c++) {
            assertTrue(FF3Alphabets.HEX_LOWER.indexOf(c) != -1);
        }

        // Verify uppercase letters are NOT present
        for (char c = 'A'; c <= 'F'; c++) {
            assertFalse(FF3Alphabets.HEX_LOWER.indexOf(c) != -1);
        }
    }

    @Test
    @DisplayName("Hex upper alphabet is correct")
    void testHexUpperAlphabet() {
        assertEquals("0123456789ABCDEF", FF3Alphabets.HEX_UPPER);
        assertEquals(16, FF3Alphabets.HEX_UPPER.length());

        // Verify all expected characters are present
        for (char c = '0'; c <= '9'; c++) {
            assertTrue(FF3Alphabets.HEX_UPPER.indexOf(c) != -1);
        }
        for (char c = 'A'; c <= 'F'; c++) {
            assertTrue(FF3Alphabets.HEX_UPPER.indexOf(c) != -1);
        }

        // Verify lowercase letters are NOT present
        for (char c = 'a'; c <= 'f'; c++) {
            assertFalse(FF3Alphabets.HEX_UPPER.indexOf(c) != -1);
        }
    }

    @Test
    @DisplayName("Base36 lower alphabet is correct")
    void testBase36LowerAlphabet() {
        assertEquals("0123456789abcdefghijklmnopqrstuvwxyz", FF3Alphabets.BASE36_LOWER);
        assertEquals(36, FF3Alphabets.BASE36_LOWER.length());

        // Verify digits
        for (char c = '0'; c <= '9'; c++) {
            assertTrue(FF3Alphabets.BASE36_LOWER.indexOf(c) != -1);
        }

        // Verify lowercase letters
        for (char c = 'a'; c <= 'z'; c++) {
            assertTrue(FF3Alphabets.BASE36_LOWER.indexOf(c) != -1);
        }

        // Verify uppercase letters are NOT present
        for (char c = 'A'; c <= 'Z'; c++) {
            assertFalse(FF3Alphabets.BASE36_LOWER.indexOf(c) != -1);
        }
    }

    @Test
    @DisplayName("Base36 upper alphabet is correct")
    void testBase36UpperAlphabet() {
        assertEquals("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ", FF3Alphabets.BASE36_UPPER);
        assertEquals(36, FF3Alphabets.BASE36_UPPER.length());

        // Verify digits
        for (char c = '0'; c <= '9'; c++) {
            assertTrue(FF3Alphabets.BASE36_UPPER.indexOf(c) != -1);
        }

        // Verify uppercase letters
        for (char c = 'A'; c <= 'Z'; c++) {
            assertTrue(FF3Alphabets.BASE36_UPPER.indexOf(c) != -1);
        }

        // Verify lowercase letters are NOT present
        for (char c = 'a'; c <= 'z'; c++) {
            assertFalse(FF3Alphabets.BASE36_UPPER.indexOf(c) != -1);
        }
    }

    @Test
    @DisplayName("Base62 alphabet is correct")
    void testBase62Alphabet() {
        assertEquals("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz", FF3Alphabets.BASE62);
        assertEquals(62, FF3Alphabets.BASE62.length());

        // Verify digits
        for (char c = '0'; c <= '9'; c++) {
            assertTrue(FF3Alphabets.BASE62.indexOf(c) != -1);
        }

        // Verify uppercase letters
        for (char c = 'A'; c <= 'Z'; c++) {
            assertTrue(FF3Alphabets.BASE62.indexOf(c) != -1);
        }

        // Verify lowercase letters
        for (char c = 'a'; c <= 'z'; c++) {
            assertTrue(FF3Alphabets.BASE62.indexOf(c) != -1);
        }
    }

    @Test
    @DisplayName("Radix26 alphabet is correct")
    void testRadix26Alphabet() {
        assertEquals("0123456789abcdefghijklmnop", FF3Alphabets.RADIX26);
        assertEquals(26, FF3Alphabets.RADIX26.length());

        // Verify it contains digits 0-9
        for (char c = '0'; c <= '9'; c++) {
            assertTrue(FF3Alphabets.RADIX26.indexOf(c) != -1);
        }

        // Verify it contains letters a-p (16 letters)
        for (char c = 'a'; c <= 'p'; c++) {
            assertTrue(FF3Alphabets.RADIX26.indexOf(c) != -1);
        }

        // Verify it does NOT contain letters q-z
        for (char c = 'q'; c <= 'z'; c++) {
            assertFalse(FF3Alphabets.RADIX26.indexOf(c) != -1);
        }
    }

    @Test
    @DisplayName("All alphabets have unique characters")
    void testUniqueCharacters() {
        testAlphabetUniqueness(FF3Alphabets.DIGITS);
        testAlphabetUniqueness(FF3Alphabets.HEX_LOWER);
        testAlphabetUniqueness(FF3Alphabets.HEX_UPPER);
        testAlphabetUniqueness(FF3Alphabets.BASE36_LOWER);
        testAlphabetUniqueness(FF3Alphabets.BASE36_UPPER);
        testAlphabetUniqueness(FF3Alphabets.BASE62);
        testAlphabetUniqueness(FF3Alphabets.RADIX26);
    }

    private void testAlphabetUniqueness(String alphabet) {
        for (int i = 0; i < alphabet.length(); i++) {
            char c = alphabet.charAt(i);
            int firstIndex = alphabet.indexOf(c);
            int lastIndex = alphabet.lastIndexOf(c);
            assertEquals(firstIndex, lastIndex, "Duplicate character '" + c + "' in alphabet: " + alphabet);
        }
    }

    @Test
    @DisplayName("Alphabets start with digits")
    void testAlphabetsStartWithDigits() {
        // Most FF3 alphabets conventionally start with digits 0-9
        assertTrue(FF3Alphabets.DIGITS.startsWith("0123456789"));
        assertTrue(FF3Alphabets.HEX_LOWER.startsWith("0123456789"));
        assertTrue(FF3Alphabets.HEX_UPPER.startsWith("0123456789"));
        assertTrue(FF3Alphabets.BASE36_LOWER.startsWith("0123456789"));
        assertTrue(FF3Alphabets.BASE36_UPPER.startsWith("0123456789"));
        assertTrue(FF3Alphabets.BASE62.startsWith("0123456789"));
        assertTrue(FF3Alphabets.RADIX26.startsWith("0123456789"));
    }

    @Test
    @DisplayName("Alphabet lengths match expected radix values")
    void testRadixValues() {
        assertEquals(10, FF3Alphabets.DIGITS.length());
        assertEquals(16, FF3Alphabets.HEX_LOWER.length());
        assertEquals(16, FF3Alphabets.HEX_UPPER.length());
        assertEquals(36, FF3Alphabets.BASE36_LOWER.length());
        assertEquals(36, FF3Alphabets.BASE36_UPPER.length());
        assertEquals(62, FF3Alphabets.BASE62.length());
        assertEquals(26, FF3Alphabets.RADIX26.length());
    }

    @Test
    @DisplayName("Alphabets are constants")
    void testConstantFields() {
        // Verify fields can't be reassigned (they're final)
        // This is checked at compile time, but we can verify they're not null
        assertNotNull(FF3Alphabets.DIGITS);
        assertNotNull(FF3Alphabets.HEX_LOWER);
        assertNotNull(FF3Alphabets.HEX_UPPER);
        assertNotNull(FF3Alphabets.BASE36_LOWER);
        assertNotNull(FF3Alphabets.BASE36_UPPER);
        assertNotNull(FF3Alphabets.BASE62);
        assertNotNull(FF3Alphabets.RADIX26);
    }

    @Test
    @DisplayName("Base62 alphabet ordering")
    void testBase62Ordering() {
        // Verify Base62 follows standard ordering: digits, uppercase, lowercase
        String expected = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
        assertEquals(expected, FF3Alphabets.BASE62);

        // Verify ordering is consistent
        assertTrue(FF3Alphabets.BASE62.indexOf('0') < FF3Alphabets.BASE62.indexOf('A'));
        assertTrue(FF3Alphabets.BASE62.indexOf('Z') < FF3Alphabets.BASE62.indexOf('a'));
        assertTrue(FF3Alphabets.BASE62.indexOf('9') < FF3Alphabets.BASE62.indexOf('A'));
    }

    @Test
    @DisplayName("Hex alphabets contain valid hex characters only")
    void testHexAlphabetsValidation() {
        // HEX_LOWER should only contain 0-9, a-f
        for (char c : FF3Alphabets.HEX_LOWER.toCharArray()) {
            assertTrue((c >= '0' && c <= '9') || (c >= 'a' && c <= 'f'),
                    "Invalid hex character in HEX_LOWER: " + c);
        }

        // HEX_UPPER should only contain 0-9, A-F
        for (char c : FF3Alphabets.HEX_UPPER.toCharArray()) {
            assertTrue((c >= '0' && c <= '9') || (c >= 'A' && c <= 'F'),
                    "Invalid hex character in HEX_UPPER: " + c);
        }
    }

    @Test
    @DisplayName("Constructor is private")
    void testPrivateConstructor() {
        // Verify the class has a private constructor (utility class pattern)
        try {
            java.lang.reflect.Constructor<?>[] constructors = FF3Alphabets.class.getDeclaredConstructors();
            assertEquals(1, constructors.length);
            assertTrue(java.lang.reflect.Modifier.isPrivate(constructors[0].getModifiers()));
        } catch (Exception e) {
            fail("Unable to verify private constructor: " + e.getMessage());
        }
    }
}