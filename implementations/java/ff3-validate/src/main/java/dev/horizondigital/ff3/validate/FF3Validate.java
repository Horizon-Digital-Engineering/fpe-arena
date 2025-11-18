package dev.horizondigital.ff3.validate;

import dev.horizondigital.ff3.FF3API;
import dev.horizondigital.ff3.FF3Alphabets;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.io.File;

@SuppressWarnings("java:S106") // System.out is appropriate for validation tool output
public class FF3Validate {
    private static final ObjectMapper objectMapper = new ObjectMapper();

    public static void main(String[] args) {
        try {
            String vectorFile = args.length > 0 ? args[0] : "../../../shared/test-vectors/nist_ff3_official_vectors.json";

            File file = new File(vectorFile);
            if (!file.exists()) {
                System.err.println("Error: Test vector file not found: " + vectorFile);
                System.exit(1);
            }

            System.out.println("FF3 NIST Validation Tool");
            System.out.println("======================");
            System.out.printf("Vector file: %s%n%n", vectorFile);

            JsonNode root = objectMapper.readTree(file);
            JsonNode vectors = root.get("vectors");
            int passed = 0;
            int total = 0;

            for (JsonNode vector : vectors) {
                total++;
                try {
                    String keyHex = vector.get("key").asText();
                    String tweakHex = vector.get("tweak").asText();
                    int radix = vector.get("radix").asInt();
                    String plaintext = vector.get("plaintext").asText();
                    String expectedCiphertext = vector.get("ciphertext").asText();

                    byte[] key = FF3API.hexToBytes(keyHex);
                    byte[] tweak = FF3API.hexToBytes(tweakHex);
                    String alphabet = getAlphabetForRadix(radix);

                    FF3API ff3 = new FF3API(key, tweak, alphabet);
                    String actualCiphertext = ff3.encrypt(plaintext);
                    String decrypted = ff3.decrypt(actualCiphertext);

                    boolean encryptPassed = expectedCiphertext.equals(actualCiphertext);
                    boolean roundtripPassed = plaintext.equals(decrypted);
                    boolean testPassed = encryptPassed && roundtripPassed;

                    if (testPassed) passed++;

                    System.out.printf("Test %2d (radix %2d): %-8s | Encrypt: %-4s | Roundtrip: %-4s%n",
                            total, radix, testPassed ? "PASS" : "FAIL",
                            encryptPassed ? "PASS" : "FAIL",
                            roundtripPassed ? "PASS" : "FAIL");

                    if (!testPassed) {
                        System.out.printf("  Input:    %s%n", plaintext);
                        System.out.printf("  Expected: %s%n", expectedCiphertext);
                        System.out.printf("  Actual:   %s%n", actualCiphertext);
                        System.out.printf("  Decrypted: %s%n", decrypted);
                        System.out.println();
                    }

                } catch (Exception e) {
                    System.out.printf("Test %2d: ERROR - %s%n", total, e.getMessage());
                }
            }

            System.out.println("======================");
            System.out.printf("Results: %d/%d passed (%.1f%%)%n", passed, total, (double) passed / total * 100);

            if (passed == total) {
                System.out.println("✓ All NIST test vectors passed!");
                System.exit(0);
            } else {
                System.out.println("✗ Some test vectors failed!");
                System.exit(1);
            }

        } catch (Exception e) {
            System.err.println("Error: " + e.getMessage());
            System.exit(1);
        }
    }

    private static String getAlphabetForRadix(int radix) {
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