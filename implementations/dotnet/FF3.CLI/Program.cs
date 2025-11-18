using System;
using FF3.Core;

namespace FF3.CLI
{
    class Program
    {
        static int Main(string[] args)
        {
            try
            {
                var opts = ParseArgs(args);

                if (opts.Help)
                {
                    ShowUsage();
                    return 0;
                }

                // Validate required arguments
                if (opts.EncryptText == null && opts.DecryptText == null)
                {
                    Console.Error.WriteLine("Error: Either --encrypt or --decrypt must be specified");
                    ShowUsage();
                    return 1;
                }

                if (opts.KeyHex == null || opts.TweakHex == null)
                {
                    Console.Error.WriteLine("Error: Key (-k) and tweak (-t) are required");
                    ShowUsage();
                    return 1;
                }

                // Parse key and tweak
                byte[] key = Core.FF3.HexToBytes(opts.KeyHex);
                byte[] tweak = Core.FF3.HexToBytes(opts.TweakHex);

                // Validate key length
                if (key.Length != 16 && key.Length != 24 && key.Length != 32)
                {
                    Console.Error.WriteLine($"Error: Key must be 16, 24, or 32 bytes (32, 48, or 64 hex chars), got {key.Length} bytes");
                    return 1;
                }

                // Validate tweak length
                if (tweak.Length != 8)
                {
                    Console.Error.WriteLine($"Error: Tweak must be 8 bytes (16 hex chars), got {tweak.Length} bytes");
                    return 1;
                }

                // Create cipher
                using var cipher = CreateCipher(key, tweak, opts.AlphabetType, opts.CustomCharset);

                // Encrypt or decrypt
                if (opts.EncryptText != null)
                {
                    string result = cipher.Encrypt(opts.EncryptText);
                    Console.WriteLine(result);
                }
                else if (opts.DecryptText != null)
                {
                    string result = cipher.Decrypt(opts.DecryptText);
                    Console.WriteLine(result);
                }

                return 0;
            }
            catch (Exception ex)
            {
                Console.Error.WriteLine($"Error: {ex.Message}");
                return 1;
            }
        }

        static Options ParseArgs(string[] args)
        {
            var opts = new Options();

            for (int i = 0; i < args.Length; i++)
            {
                string arg = args[i];
                switch (arg)
                {
                    case "-h":
                    case "--help":
                        opts.Help = true;
                        return opts;
                    case "-e":
                    case "--encrypt":
                        if (i + 1 >= args.Length)
                            throw new ArgumentException($"Missing value for {arg}");
                        opts.EncryptText = args[++i];
                        break;
                    case "-d":
                    case "--decrypt":
                        if (i + 1 >= args.Length)
                            throw new ArgumentException($"Missing value for {arg}");
                        opts.DecryptText = args[++i];
                        break;
                    case "-k":
                    case "--key":
                        if (i + 1 >= args.Length)
                            throw new ArgumentException($"Missing value for {arg}");
                        opts.KeyHex = args[++i];
                        break;
                    case "-t":
                    case "--tweak":
                        if (i + 1 >= args.Length)
                            throw new ArgumentException($"Missing value for {arg}");
                        opts.TweakHex = args[++i];
                        break;
                    case "-a":
                    case "--alphabet":
                        if (i + 1 >= args.Length)
                            throw new ArgumentException($"Missing value for {arg}");
                        opts.AlphabetType = args[++i];
                        break;
                    case "-c":
                    case "--custom":
                        if (i + 1 >= args.Length)
                            throw new ArgumentException($"Missing value for {arg}");
                        opts.CustomCharset = args[++i];
                        break;
                    default:
                        throw new ArgumentException($"Unknown option: {arg}");
                }
            }

            return opts;
        }

        static Core.FF3 CreateCipher(byte[] key, byte[] tweak, string alphabetType, string? customCharset)
        {
            if (!string.IsNullOrEmpty(customCharset))
            {
                return new Core.FF3(key, tweak, customCharset);
            }

            return alphabetType.ToLower() switch
            {
                "digits" => Core.FF3.Digits(key, tweak),
                "hex" or "hex-lower" => Core.FF3.HexLower(key, tweak),
                "hex-upper" => Core.FF3.HexUpper(key, tweak),
                "base36" or "base36-lower" => Core.FF3.Base36Lower(key, tweak),
                "base36-upper" => Core.FF3.Base36Upper(key, tweak),
                "base62" => Core.FF3.Base62(key, tweak),
                _ => throw new ArgumentException($"Unknown alphabet type: {alphabetType}")
            };
        }

        static void ShowUsage()
        {
            Console.WriteLine("FF3 CLI - Format Preserving Encryption");
            Console.WriteLine();
            Console.WriteLine("Usage: ff3-cli [OPTIONS]");
            Console.WriteLine();
            Console.WriteLine("Options:");
            Console.WriteLine("  -e, --encrypt TEXT      Encrypt the given text");
            Console.WriteLine("  -d, --decrypt TEXT      Decrypt the given text");
            Console.WriteLine("  -k, --key HEX           AES key in hex format (32/48/64 hex chars)");
            Console.WriteLine("  -t, --tweak HEX         Tweak in hex format (16 hex chars)");
            Console.WriteLine("  -a, --alphabet TYPE     Alphabet type:");
            Console.WriteLine("                            digits (default)");
            Console.WriteLine("                            hex-lower");
            Console.WriteLine("                            hex-upper");
            Console.WriteLine("                            base36-lower");
            Console.WriteLine("                            base36-upper");
            Console.WriteLine("                            base62");
            Console.WriteLine("  -c, --custom CHARSET    Custom alphabet charset");
            Console.WriteLine("  -h, --help              Show this help message");
            Console.WriteLine();
            Console.WriteLine("Examples:");
            Console.WriteLine("  ff3-cli -e \"1234567890\" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73");
            Console.WriteLine("  ff3-cli -d \"7501889140\" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73");
            Console.WriteLine();
        }

        class Options
        {
            public bool Help { get; set; }
            public string? EncryptText { get; set; }
            public string? DecryptText { get; set; }
            public string? KeyHex { get; set; }
            public string? TweakHex { get; set; }
            public string AlphabetType { get; set; } = "digits";
            public string? CustomCharset { get; set; }
        }
    }
}
