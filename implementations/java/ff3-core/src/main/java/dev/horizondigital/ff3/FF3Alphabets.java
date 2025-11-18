package dev.horizondigital.ff3;

/**
 * FF3 Alphabets - predefined character sets
 */
public final class FF3Alphabets {

    public static final String DIGITS = "0123456789";
    public static final String HEX_LOWER = "0123456789abcdef";
    public static final String HEX_UPPER = "0123456789ABCDEF";
    public static final String BASE36_LOWER = "0123456789abcdefghijklmnopqrstuvwxyz";
    public static final String BASE36_UPPER = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    public static final String BASE62 = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
    public static final String RADIX26 = "0123456789abcdefghijklmnop";

    private FF3Alphabets() {
        // Utility class - prevent instantiation
    }
}