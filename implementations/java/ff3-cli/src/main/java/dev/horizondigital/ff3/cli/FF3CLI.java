package dev.horizondigital.ff3.cli;

import dev.horizondigital.ff3.FF3API;
import dev.horizondigital.ff3.FF3Alphabets;

@SuppressWarnings("java:S106") // System.out is appropriate for CLI application output
public class FF3CLI {

    public static void main(String[] args) {
        try {
            Options opts = parseArgs(args);

            if (opts.help) {
                showUsage();
                return;
            }

            // Validate required arguments
            if (opts.encryptText == null && opts.decryptText == null) {
                System.err.println("Error: Either --encrypt or --decrypt must be specified");
                showUsage();
                System.exit(1);
            }

            if (opts.keyHex == null || opts.tweakHex == null) {
                System.err.println("Error: Key (-k) and tweak (-t) are required");
                showUsage();
                System.exit(1);
            }

            // Parse key and tweak
            byte[] key = FF3API.hexToBytes(opts.keyHex);
            byte[] tweak = FF3API.hexToBytes(opts.tweakHex);

            // Validate key length
            if (key.length != 16 && key.length != 24 && key.length != 32) {
                System.err.printf("Error: Key must be 16, 24, or 32 bytes (32, 48, or 64 hex chars), got %d bytes%n", key.length);
                System.exit(1);
            }

            // Validate tweak length
            if (tweak.length != 8) {
                System.err.printf("Error: Tweak must be 8 bytes (16 hex chars), got %d bytes%n", tweak.length);
                System.exit(1);
            }

            // Create alphabet
            String alphabet = createAlphabet(opts.alphabetType, opts.customCharset);

            // Create cipher
            FF3API ff3 = new FF3API(key, tweak, alphabet);

            // Encrypt or decrypt
            if (opts.encryptText != null) {
                String result = ff3.encrypt(opts.encryptText);
                System.out.println(result);
            } else if (opts.decryptText != null) {
                String result = ff3.decrypt(opts.decryptText);
                System.out.println(result);
            }

        } catch (Exception e) {
            System.err.println("Error: " + e.getMessage());
            System.exit(1);
        }
    }

    private static Options parseArgs(String[] args) {
        Options opts = new Options();

        for (int i = 0; i < args.length; i++) {
            String arg = args[i];
            switch (arg) {
                case "-h":
                case "--help":
                    opts.help = true;
                    return opts;
                case "-e":
                case "--encrypt":
                    if (i + 1 >= args.length) {
                        throw new IllegalArgumentException("Missing value for " + arg);
                    }
                    opts.encryptText = args[++i];
                    break;
                case "-d":
                case "--decrypt":
                    if (i + 1 >= args.length) {
                        throw new IllegalArgumentException("Missing value for " + arg);
                    }
                    opts.decryptText = args[++i];
                    break;
                case "-k":
                case "--key":
                    if (i + 1 >= args.length) {
                        throw new IllegalArgumentException("Missing value for " + arg);
                    }
                    opts.keyHex = args[++i];
                    break;
                case "-t":
                case "--tweak":
                    if (i + 1 >= args.length) {
                        throw new IllegalArgumentException("Missing value for " + arg);
                    }
                    opts.tweakHex = args[++i];
                    break;
                case "-a":
                case "--alphabet":
                    if (i + 1 >= args.length) {
                        throw new IllegalArgumentException("Missing value for " + arg);
                    }
                    opts.alphabetType = args[++i];
                    break;
                case "-c":
                case "--custom":
                    if (i + 1 >= args.length) {
                        throw new IllegalArgumentException("Missing value for " + arg);
                    }
                    opts.customCharset = args[++i];
                    break;
                default:
                    throw new IllegalArgumentException("Unknown option: " + arg);
            }
        }

        return opts;
    }

    private static String createAlphabet(String alphabetType, String customCharset) {
        if (customCharset != null && !customCharset.isEmpty()) {
            return customCharset;
        }

        if (alphabetType == null) {
            alphabetType = "digits";
        }

        switch (alphabetType.toLowerCase()) {
            case "digits":
                return FF3Alphabets.DIGITS;
            case "hex":
            case "hex-lower":
                return FF3Alphabets.HEX_LOWER;
            case "hex-upper":
                return FF3Alphabets.HEX_UPPER;
            case "base36":
            case "base36-lower":
                return FF3Alphabets.BASE36_LOWER;
            case "base36-upper":
                return FF3Alphabets.BASE36_UPPER;
            case "base62":
                return FF3Alphabets.BASE62;
            default:
                throw new IllegalArgumentException("Unknown alphabet type: " + alphabetType);
        }
    }

    private static void showUsage() {
        System.out.println("FF3 CLI - Format Preserving Encryption");
        System.out.println();
        System.out.println("Usage: ff3-cli [OPTIONS]");
        System.out.println();
        System.out.println("Options:");
        System.out.println("  -e, --encrypt TEXT      Encrypt the given text");
        System.out.println("  -d, --decrypt TEXT      Decrypt the given text");
        System.out.println("  -k, --key HEX           AES key in hex format (32/48/64 hex chars)");
        System.out.println("  -t, --tweak HEX         Tweak in hex format (16 hex chars)");
        System.out.println("  -a, --alphabet TYPE     Alphabet type:");
        System.out.println("                            digits (default)");
        System.out.println("                            hex-lower");
        System.out.println("                            hex-upper");
        System.out.println("                            base36-lower");
        System.out.println("                            base36-upper");
        System.out.println("                            base62");
        System.out.println("  -c, --custom CHARSET    Custom alphabet charset");
        System.out.println("  -h, --help              Show this help message");
        System.out.println();
        System.out.println("Examples:");
        System.out.println("  ff3-cli -e \"1234567890\" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73");
        System.out.println("  ff3-cli -d \"7501889140\" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73");
        System.out.println();
    }

    private static class Options {
        boolean help = false;
        String encryptText = null;
        String decryptText = null;
        String keyHex = null;
        String tweakHex = null;
        String alphabetType = "digits";
        String customCharset = null;
    }
}
