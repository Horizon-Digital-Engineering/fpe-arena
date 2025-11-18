package dev.horizondigital.ff3.stresstest;

import dev.horizondigital.ff3.FF3API;
import dev.horizondigital.ff3.FF3Alphabets;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Random;

@SuppressWarnings("java:S106")
public final class FF3StressTest {
    private static final Map<String, String> ALPHABET_MAP = Map.of(
        "digits", FF3Alphabets.DIGITS,
        "hex-lower", FF3Alphabets.HEX_LOWER,
        "hex-upper", FF3Alphabets.HEX_UPPER,
        "base36-lower", FF3Alphabets.BASE36_LOWER,
        "base36-upper", FF3Alphabets.BASE36_UPPER,
        "base62", FF3Alphabets.BASE62
    );

    private static final class Options {
        int iterations = 1000;
        List<String> alphabets = new ArrayList<>(Arrays.asList("digits", "hex-lower", "base36-lower", "base62"));
        int minLength = 6;
        int maxLength = 20;
        boolean quick = false;
        Long seed = null;
        boolean help = false;
    }

    public static void main(String[] args) {
        Options opts;
        try {
            opts = parseOptions(args);
        } catch (IllegalArgumentException ex) {
            System.err.println("Error: " + ex.getMessage());
            System.exit(1);
            return;
        }

        if (opts.help) {
            printUsage();
            return;
        }

        if (opts.quick) {
            opts.iterations = 100;
        }

        try {
            validate(opts);
        } catch (IllegalArgumentException ex) {
            System.err.println("Error: " + ex.getMessage());
            System.exit(1);
            return;
        }

        Random rng = opts.seed != null ? new Random(opts.seed) : new Random();

        System.out.println("FF3 Stress Test v1.0");
        System.out.println("====================\n");
        System.out.println("Warning: FF3 was withdrawn by NIST; run for education and research only.\n");
        System.out.println("Test configuration");
        System.out.printf("  Iterations per alphabet: %d%n", opts.iterations);
        System.out.println("  Random key/tweak generation: enabled");
        System.out.printf("  String length range: %d-%d characters%n", opts.minLength, opts.maxLength);
        System.out.printf("  Alphabets: %s%n%n", String.join(", ", opts.alphabets));

        long start = System.currentTimeMillis();
        int totalTests = 0;
        int totalFailures = 0;

        for (String name : opts.alphabets) {
            String charset = ALPHABET_MAP.get(name);
            AlphabetResult result = stressAlphabet(name, charset, opts, rng);
            totalTests += result.tests;
            totalFailures += result.failures;
        }

        long durationMs = System.currentTimeMillis() - start;
        System.out.println("Summary");
        System.out.printf("  Total tests: %d%n", totalTests);
        System.out.printf("  Failures: %d%n", totalFailures);
        System.out.printf("  Duration: %d ms%n", durationMs);
        if (durationMs > 0) {
            double throughput = totalTests * 1000.0 / durationMs;
            System.out.printf("  Throughput: %.2f tests/sec%n", throughput);
        }
        if (totalFailures == 0) {
            System.out.println("  Result: all stress tests passed");
        } else {
            System.out.println("  Result: failures detected");
            System.exit(1);
        }
    }

    private record AlphabetResult(int tests, int failures) {}

    private static AlphabetResult stressAlphabet(String name, String charset, Options opts, Random rng) {
        if (charset == null) {
            throw new IllegalArgumentException("Unknown alphabet: " + name);
        }

        System.out.printf("Testing %s...%n", name);
        System.out.printf("  Alphabet: %s (radix %d)%n", charset, charset.length());

        int passed = 0;
        int failed = 0;
        int total = opts.iterations;
        int interval = Math.max(1, total / 10);

        for (int i = 0; i < total; i++) {
            byte[] key = randomBytes(rng, 16);
            byte[] tweak = randomBytes(rng, 8);
            int len = rng.nextInt(opts.maxLength - opts.minLength + 1) + opts.minLength;
            String plaintext = generatePlaintext(charset, len, rng);

            try {
                FF3API cipher = new FF3API(key, tweak, charset);
                String ciphertext = cipher.encrypt(plaintext);
                String decrypted = cipher.decrypt(ciphertext);
                if (!plaintext.equals(decrypted)) {
                    failed++;
                    printFailure(key, tweak, plaintext, ciphertext, decrypted, "round-trip mismatch");
                } else {
                    passed++;
                }
            } catch (Exception ex) {
                failed++;
                printFailure(key, tweak, plaintext, "", null, ex.getMessage());
            }

            if ((i + 1) % interval == 0 || i + 1 == total) {
                int percent = (i + 1) * 100 / total;
                System.out.printf("  Progress: %d/%d (%d%%)%n", i + 1, total, percent);
            }
        }

        System.out.printf("  Passed: %d/%d%n", passed, total);
        System.out.printf("  Failed: %d/%d%n%n", failed, total);
        return new AlphabetResult(total, failed);
    }

    private static byte[] randomBytes(Random rng, int length) {
        byte[] bytes = new byte[length];
        rng.nextBytes(bytes);
        return bytes;
    }

    private static String generatePlaintext(String alphabet, int length, Random rng) {
        StringBuilder sb = new StringBuilder(length);
        for (int i = 0; i < length; i++) {
            sb.append(alphabet.charAt(rng.nextInt(alphabet.length())));
        }
        return sb.toString();
    }

    private static void printFailure(byte[] key, byte[] tweak, String plain, String cipher, String decrypted, String detail) {
        System.out.println("  Round-trip failed:");
        System.out.printf("    Key: %s%n", toHex(key));
        System.out.printf("    Tweak: %s%n", toHex(tweak));
        System.out.printf("    Plaintext: \"%s\"%n", plain);
        if (!cipher.isEmpty()) {
            System.out.printf("    Ciphertext: \"%s\"%n", cipher);
        }
        if (decrypted != null) {
            System.out.printf("    Decrypted: \"%s\"%n", decrypted);
        }
        if (detail != null && !detail.isEmpty()) {
            System.out.printf("    Detail: %s%n", detail);
        }
    }

    private static String toHex(byte[] bytes) {
        StringBuilder sb = new StringBuilder(bytes.length * 2);
        for (byte b : bytes) {
            sb.append(String.format(Locale.ROOT, "%02x", b));
        }
        return sb.toString();
    }

    private static Options parseOptions(String[] args) {
        Options opts = new Options();
        for (int i = 0; i < args.length; i++) {
            String arg = args[i];
            switch (arg) {
                case "--help", "-h" -> opts.help = true;
                case "--alphabets" -> {
                    if (i + 1 >= args.length) fail("--alphabets requires a value");
                    String list = args[++i];
                    opts.alphabets.clear();
                    for (String name : list.split(",")) {
                        String trimmed = name.trim().toLowerCase(Locale.ROOT);
                        if (!trimmed.isEmpty()) {
                            opts.alphabets.add(trimmed);
                        }
                    }
                    if (opts.alphabets.isEmpty()) fail("--alphabets list cannot be empty");
                }
                case "--min-length" -> {
                    opts.minLength = parsePositiveInt(args, ++i, "--min-length");
                }
                case "--max-length" -> {
                    opts.maxLength = parsePositiveInt(args, ++i, "--max-length");
                }
                case "--quick" -> opts.quick = true;
                case "--seed" -> {
                    opts.seed = (long) parseNonNegativeInt(args, ++i, "--seed");
                }
                default -> {
                    if (arg.startsWith("--")) {
                        fail("Unknown option: " + arg);
                    } else {
                        int iterations = Integer.parseInt(arg);
                        if (iterations <= 0) fail("iterations must be greater than 0");
                        opts.iterations = iterations;
                    }
                }
            }
        }
        return opts;
    }

    private static void validate(Options opts) {
        if (opts.iterations <= 0) fail("iterations must be greater than 0");
        if (opts.minLength < 2) fail("--min-length must be at least 2");
        if (opts.maxLength < opts.minLength) fail("--max-length must be greater than or equal to --min-length");
        for (String name : opts.alphabets) {
            if (!ALPHABET_MAP.containsKey(name)) {
                fail("Unknown alphabet: " + name);
            }
        }
    }

    private static int parsePositiveInt(String[] args, int index, String flag) {
        if (index >= args.length) fail(flag + " requires a value");
        int value = Integer.parseInt(args[index]);
        if (value <= 0) fail(flag + " must be greater than 0");
        return value;
    }

    private static int parseNonNegativeInt(String[] args, int index, String flag) {
        if (index >= args.length) fail(flag + " requires a value");
        int value = Integer.parseInt(args[index]);
        if (value < 0) fail(flag + " must be non-negative");
        return value;
    }

    private static void fail(String message) {
        throw new IllegalArgumentException(message);
    }

    private static void printUsage() {
        System.out.println("FF3 Stress Test Tool\n");
        System.out.println("Usage: ff3-stresstest [OPTIONS] [ITERATIONS]\n");
        System.out.println("Options:");
        System.out.println("  --alphabets LIST      Comma-separated list (default: digits,hex-lower,base36-lower,base62)");
        System.out.println("  --min-length N        Minimum plaintext length (default: 6)");
        System.out.println("  --max-length N        Maximum plaintext length (default: 20)");
        System.out.println("  --quick               Run 100 iterations (fast test)");
        System.out.println("  --seed N              Random seed for reproducibility");
    }
}
