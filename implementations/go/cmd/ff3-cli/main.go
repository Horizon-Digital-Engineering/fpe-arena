// FF3 CLI - Command-line tool for FF3 encryption/decryption
package main

import (
	"encoding/hex"
	"flag"
	"fmt"
	"os"

	"go.horizondigital.dev/fpe-ff3/pkg/ff3"
)

func printUsage() {
	fmt.Fprintln(os.Stderr, "FF3 CLI - Format Preserving Encryption")
	fmt.Fprintln(os.Stderr)
	fmt.Fprintln(os.Stderr, "Usage: ff3-cli [OPTIONS]")
	fmt.Fprintln(os.Stderr)
	fmt.Fprintln(os.Stderr, "Options:")
	fmt.Fprintln(os.Stderr, "  -e, --encrypt TEXT      Encrypt the given text")
	fmt.Fprintln(os.Stderr, "  -d, --decrypt TEXT      Decrypt the given text")
	fmt.Fprintln(os.Stderr, "  -k, --key HEX           AES key in hex format (32/48/64 hex chars)")
	fmt.Fprintln(os.Stderr, "  -t, --tweak HEX         Tweak in hex format (16 hex chars)")
	fmt.Fprintln(os.Stderr, "  -a, --alphabet TYPE     Alphabet type:")
	fmt.Fprintln(os.Stderr, "                            digits (default)")
	fmt.Fprintln(os.Stderr, "                            hex-lower")
	fmt.Fprintln(os.Stderr, "                            hex-upper")
	fmt.Fprintln(os.Stderr, "                            base36-lower")
	fmt.Fprintln(os.Stderr, "                            base36-upper")
	fmt.Fprintln(os.Stderr, "                            base62")
	fmt.Fprintln(os.Stderr, "  -c, --custom CHARSET    Custom alphabet charset")
	fmt.Fprintln(os.Stderr, "  -h, --help              Show this help message")
	fmt.Fprintln(os.Stderr)
	fmt.Fprintln(os.Stderr, "Examples:")
	fmt.Fprintln(os.Stderr, "  ff3-cli -e \"1234567890\" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73")
	fmt.Fprintln(os.Stderr, "  ff3-cli -d \"7501889140\" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73")
	fmt.Fprintln(os.Stderr)
}

func createCipher(key, tweak []byte, alphabetType, customCharset string) (*ff3.FF3, error) {
	if customCharset != "" {
		spec := ff3.AlphabetSpec{Charset: customCharset}
		return ff3.FromSpec(key, tweak, spec)
	}

	switch alphabetType {
	case "digits":
		return ff3.Digits(key, tweak)
	case "hex-lower":
		return ff3.HexLower(key, tweak)
	case "hex-upper":
		return ff3.HexUpper(key, tweak)
	case "base36-lower":
		return ff3.Base36Lower(key, tweak)
	case "base36-upper":
		return ff3.Base36Upper(key, tweak)
	case "base62":
		return ff3.Base62(key, tweak)
	default:
		return nil, fmt.Errorf("unknown alphabet type: %s", alphabetType)
	}
}

func main() {
	var (
		encryptText   string
		decryptText   string
		keyHex        string
		tweakHex      string
		alphabetType  string = "digits"
		customCharset string
		help          bool
	)

	flag.StringVar(&encryptText, "e", "", "Encrypt the given text")
	flag.StringVar(&encryptText, "encrypt", "", "Encrypt the given text")
	flag.StringVar(&decryptText, "d", "", "Decrypt the given text")
	flag.StringVar(&decryptText, "decrypt", "", "Decrypt the given text")
	flag.StringVar(&keyHex, "k", "", "AES key in hex format")
	flag.StringVar(&keyHex, "key", "", "AES key in hex format")
	flag.StringVar(&tweakHex, "t", "", "Tweak in hex format")
	flag.StringVar(&tweakHex, "tweak", "", "Tweak in hex format")
	flag.StringVar(&alphabetType, "a", "digits", "Alphabet type")
	flag.StringVar(&alphabetType, "alphabet", "digits", "Alphabet type")
	flag.StringVar(&customCharset, "c", "", "Custom alphabet charset")
	flag.StringVar(&customCharset, "custom", "", "Custom alphabet charset")
	flag.BoolVar(&help, "h", false, "Show help message")
	flag.BoolVar(&help, "help", false, "Show help message")

	flag.Usage = printUsage
	flag.Parse()

	if help {
		printUsage()
		return
	}

	if encryptText == "" && decryptText == "" {
		printUsage()
		return
	}

	if keyHex == "" || tweakHex == "" {
		fmt.Println("Error: Key and tweak are required")
		return
	}

	// Parse key and tweak
	key, err := hex.DecodeString(keyHex)
	if err != nil || (len(key) != 16 && len(key) != 24 && len(key) != 32) {
		fmt.Println("Error: Key must be 16, 24, or 32 bytes (32, 48, or 64 hex chars)")
		return
	}

	tweak, err := hex.DecodeString(tweakHex)
	if err != nil || len(tweak) != 8 {
		fmt.Println("Error: Tweak must be 8 bytes (16 hex chars)")
		return
	}

	// Create cipher
	cipher, err := createCipher(key, tweak, alphabetType, customCharset)
	if err != nil {
		fmt.Printf("Error: %v\n", err)
		return
	}

	// Encrypt or decrypt
	if encryptText != "" {
		result, err := cipher.Encrypt(encryptText, nil)
		if err != nil {
			fmt.Printf("Error: %v\n", err)
			return
		}
		fmt.Println(result)
	} else if decryptText != "" {
		result, err := cipher.Decrypt(decryptText, nil)
		if err != nil {
			fmt.Printf("Error: %v\n", err)
			return
		}
		fmt.Println(result)
	}
}
