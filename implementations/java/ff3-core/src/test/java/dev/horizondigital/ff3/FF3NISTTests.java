package dev.horizondigital.ff3;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;
import static org.junit.jupiter.api.Assertions.*;

import java.io.File;

public class FF3NISTTests {
    private static final ObjectMapper objectMapper = new ObjectMapper();

    private String getTestVectorFilePath() {
        String vectorFile = System.getenv("FF3_TEST_VECTORS_PATH");
        if (vectorFile == null || vectorFile.isEmpty()) {
            // Fallback to relative path for local development
            vectorFile = "../../shared/test-vectors/nist_ff3_official_vectors.json";
            File file = new File(vectorFile);
            if (!file.exists()) {
                vectorFile = "../../../../shared/test-vectors/nist_ff3_official_vectors.json";
            }
        }
        return vectorFile;
    }

    @Test
    @DisplayName("All NIST test vectors pass")
    void testAllNISTVectors() throws Exception {
        String vectorFile = getTestVectorFilePath();
        File file = new File(vectorFile);

        if (!file.exists()) {
            fail("NIST test vector file not found. Expected at: " + vectorFile +
                 ". Set FF3_TEST_VECTORS_PATH environment variable or ensure file exists at relative path.");
        }

        JsonNode root = objectMapper.readTree(file);
        JsonNode vectors = root.get("vectors");
        int passed = 0;
        int total = 0;

        for (JsonNode vector : vectors) {
            total++;
            String keyHex = vector.get("key").asText();
            String tweakHex = vector.get("tweak").asText();
            int radix = vector.get("radix").asInt();
            String plaintext = vector.get("plaintext").asText();
            String expectedCiphertext = vector.get("ciphertext").asText();

            try {
                byte[] key = FF3API.hexToBytes(keyHex);
                byte[] tweak = FF3API.hexToBytes(tweakHex);
                String alphabet = getAlphabetForRadix(radix);

                FF3API ff3 = new FF3API(key, tweak, alphabet);
                String actualCiphertext = ff3.encrypt(plaintext);

                assertEquals(expectedCiphertext, actualCiphertext,
                        String.format("Test vector %d failed: expected %s, got %s (radix %d, plaintext: %s)",
                                total, expectedCiphertext, actualCiphertext, radix, plaintext));

                // Also test decrypt roundtrip
                String decrypted = ff3.decrypt(actualCiphertext);
                assertEquals(plaintext, decrypted,
                        String.format("Test vector %d decrypt failed: expected %s, got %s",
                                total, plaintext, decrypted));

                passed++;
            } catch (Exception e) {
                fail(String.format("Test vector %d threw exception: %s (radix %d, plaintext: %s)",
                        total, e.getMessage(), radix, plaintext));
            }
        }

        System.out.printf("NIST FF3 Test Results: %d/%d passed%n", passed, total);
        assertEquals(total, passed, "Some NIST test vectors failed");
        assertTrue(total >= 15, "Expected at least 15 NIST test vectors");
    }

    @Test
    @DisplayName("Specific NIST test vector 1")
    void testSpecificVector1() throws Exception {
        // Test vector 1 from NIST SP 800-38G
        byte[] key = FF3API.hexToBytes("EF4359D8D580AA4F7F036D6F04FC6A94");
        byte[] tweak = FF3API.hexToBytes("D8E7920AFA330A73");
        FF3API ff3 = FF3API.digits(key, tweak);

        String plaintext = "890121234567890000";
        String expectedCiphertext = "750918814058654607";

        String actualCiphertext = ff3.encrypt(plaintext);
        assertEquals(expectedCiphertext, actualCiphertext);

        String decrypted = ff3.decrypt(actualCiphertext);
        assertEquals(plaintext, decrypted);
    }

    @Test
    @DisplayName("NIST test vector with different radix")
    void testDifferentRadixVector() throws Exception {
        // Find a test vector with radix != 10
        String vectorFile = getTestVectorFilePath();
        File file = new File(vectorFile);

        if (!file.exists()) {
            return; // Skip if file not found
        }

        JsonNode root = objectMapper.readTree(file);
        JsonNode vectors = root.get("vectors");
        boolean foundNonDecimalVector = false;

        for (JsonNode vector : vectors) {
            int radix = vector.get("radix").asInt();
            if (radix != 10) {
                foundNonDecimalVector = true;

                String keyHex = vector.get("key").asText();
                String tweakHex = vector.get("tweak").asText();
                String plaintext = vector.get("plaintext").asText();
                String expectedCiphertext = vector.get("ciphertext").asText();

                byte[] key = FF3API.hexToBytes(keyHex);
                byte[] tweak = FF3API.hexToBytes(tweakHex);
                String alphabet = getAlphabetForRadix(radix);

                FF3API ff3 = new FF3API(key, tweak, alphabet);
                String actualCiphertext = ff3.encrypt(plaintext);

                assertEquals(expectedCiphertext, actualCiphertext,
                        String.format("Non-decimal radix test failed: radix %d", radix));

                String decrypted = ff3.decrypt(actualCiphertext);
                assertEquals(plaintext, decrypted);

                break; // Test first non-decimal vector found
            }
        }

        assertTrue(foundNonDecimalVector, "No non-decimal radix test vectors found");
    }

    @Test
    @DisplayName("NIST vectors with different key lengths")
    void testDifferentKeyLengths() throws Exception {
        String vectorFile = getTestVectorFilePath();
        File file = new File(vectorFile);

        if (!file.exists()) {
            return; // Skip if file not found
        }

        JsonNode root = objectMapper.readTree(file);
        JsonNode vectors = root.get("vectors");
        boolean foundAES128 = false, foundAES192 = false, foundAES256 = false;

        for (JsonNode vector : vectors) {
            String keyHex = vector.get("key").asText();
            int keyLength = keyHex.length() / 2; // Convert hex chars to bytes

            String tweakHex = vector.get("tweak").asText();
            int radix = vector.get("radix").asInt();
            String plaintext = vector.get("plaintext").asText();
            String expectedCiphertext = vector.get("ciphertext").asText();

            byte[] key = FF3API.hexToBytes(keyHex);
            byte[] tweak = FF3API.hexToBytes(tweakHex);
            String alphabet = getAlphabetForRadix(radix);

            FF3API ff3 = new FF3API(key, tweak, alphabet);
            String actualCiphertext = ff3.encrypt(plaintext);

            assertEquals(expectedCiphertext, actualCiphertext,
                    String.format("Key length %d test failed", keyLength));

            // Track which key lengths we've tested
            switch (keyLength) {
                case 16: foundAES128 = true; break;
                case 24: foundAES192 = true; break;
                case 32: foundAES256 = true; break;
            }
        }

        assertTrue(foundAES128, "No AES-128 (16-byte key) test vectors found");
        // AES-192 and AES-256 may not be in all test vector sets
    }

    @Test
    @DisplayName("NIST vector format preservation")
    void testFormatPreservation() throws Exception {
        String vectorFile = getTestVectorFilePath();
        File file = new File(vectorFile);

        if (!file.exists()) {
            return; // Skip if file not found
        }

        JsonNode root = objectMapper.readTree(file);
        JsonNode vectors = root.get("vectors");

        for (JsonNode vector : vectors) {
            String keyHex = vector.get("key").asText();
            String tweakHex = vector.get("tweak").asText();
            int radix = vector.get("radix").asInt();
            String plaintext = vector.get("plaintext").asText();

            byte[] key = FF3API.hexToBytes(keyHex);
            byte[] tweak = FF3API.hexToBytes(tweakHex);
            String alphabet = getAlphabetForRadix(radix);

            FF3API ff3 = new FF3API(key, tweak, alphabet);
            String ciphertext = ff3.encrypt(plaintext);

            // Verify format preservation
            assertEquals(plaintext.length(), ciphertext.length(),
                    "Length not preserved for radix " + radix);

            // Verify all characters are in the alphabet
            for (char c : ciphertext.toCharArray()) {
                assertTrue(alphabet.indexOf(c) != -1,
                        String.format("Character '%c' not in alphabet for radix %d", c, radix));
            }
        }
    }

    @Test
    @DisplayName("Verify test vector file format")
    void testVectorFileFormat() throws Exception {
        String vectorFile = getTestVectorFilePath();
        File file = new File(vectorFile);

        if (!file.exists()) {
            return; // Skip if file not found
        }

        JsonNode root = objectMapper.readTree(file);
        JsonNode vectors = root.get("vectors");
        assertTrue(vectors.isArray(), "Test vector file should contain an array");
        assertTrue(vectors.size() > 0, "Test vector file should not be empty");

        // Verify first vector has required fields
        JsonNode firstVector = vectors.get(0);
        assertTrue(firstVector.has("key"), "Test vector should have 'key' field");
        assertTrue(firstVector.has("tweak"), "Test vector should have 'tweak' field");
        assertTrue(firstVector.has("radix"), "Test vector should have 'radix' field");
        assertTrue(firstVector.has("plaintext"), "Test vector should have 'plaintext' field");
        assertTrue(firstVector.has("ciphertext"), "Test vector should have 'ciphertext' field");
    }

    private String getAlphabetForRadix(int radix) {
        switch (radix) {
            case 10: return FF3Alphabets.DIGITS;
            case 16: return FF3Alphabets.HEX_LOWER;
            case 26: return FF3Alphabets.RADIX26;
            case 36: return FF3Alphabets.BASE36_LOWER;
            case 62: return FF3Alphabets.BASE62;
            default:
                throw new IllegalArgumentException("Unsupported radix: " + radix);
        }
    }
}