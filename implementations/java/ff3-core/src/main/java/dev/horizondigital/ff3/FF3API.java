package dev.horizondigital.ff3;

/**
 * FF3 API - string-based interface with factory methods
 */
public class FF3API {
    private final FF3Core cipher;
    private final String alphabet;

    public FF3API(byte[] key, byte[] tweak, String alphabet) throws Exception {
        this.alphabet = alphabet;
        this.cipher = new FF3Core(alphabet.length(), key, tweak);
    }

    public String encrypt(String plaintext) throws Exception {
        int[] digits = stringToArray(plaintext);
        int[] encrypted = cipher.encrypt(digits);
        return arrayToString(encrypted);
    }

    public String decrypt(String ciphertext) throws Exception {
        int[] digits = stringToArray(ciphertext);
        int[] decrypted = cipher.decrypt(digits);
        return arrayToString(decrypted);
    }

    private int[] stringToArray(String str) {
        int[] result = new int[str.length()];
        for (int i = 0; i < str.length(); i++) {
            char c = str.charAt(i);
            int index = alphabet.indexOf(c);
            if (index == -1) {
                throw new IllegalArgumentException("Invalid character: " + c);
            }
            result[i] = index;
        }
        return result;
    }

    private String arrayToString(int[] array) {
        StringBuilder sb = new StringBuilder();
        for (int digit : array) {
            sb.append(alphabet.charAt(digit));
        }
        return sb.toString();
    }

    // Factory methods using predefined alphabets
    public static FF3API digits(byte[] key, byte[] tweak) throws Exception {
        return new FF3API(key, tweak, FF3Alphabets.DIGITS);
    }

    public static FF3API hexLower(byte[] key, byte[] tweak) throws Exception {
        return new FF3API(key, tweak, FF3Alphabets.HEX_LOWER);
    }

    public static FF3API hexUpper(byte[] key, byte[] tweak) throws Exception {
        return new FF3API(key, tweak, FF3Alphabets.HEX_UPPER);
    }

    public static FF3API base36Lower(byte[] key, byte[] tweak) throws Exception {
        return new FF3API(key, tweak, FF3Alphabets.BASE36_LOWER);
    }

    public static FF3API base36Upper(byte[] key, byte[] tweak) throws Exception {
        return new FF3API(key, tweak, FF3Alphabets.BASE36_UPPER);
    }

    public static FF3API base62(byte[] key, byte[] tweak) throws Exception {
        return new FF3API(key, tweak, FF3Alphabets.BASE62);
    }

    public static FF3API radix26(byte[] key, byte[] tweak) throws Exception {
        return new FF3API(key, tweak, FF3Alphabets.RADIX26);
    }

    // Utility method
    public static byte[] hexToBytes(String hex) {
        byte[] result = new byte[hex.length() / 2];
        for (int i = 0; i < hex.length(); i += 2) {
            result[i / 2] = (byte) Integer.parseInt(hex.substring(i, i + 2), 16);
        }
        return result;
    }

    public String getAlphabet() {
        return alphabet;
    }

    public int getRadix() {
        return cipher.getRadix();
    }
}