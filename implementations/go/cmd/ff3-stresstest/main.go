package main

import (
	crand "crypto/rand"
	"encoding/hex"
	"flag"
	"fmt"
	mathrand "math/rand"
	"os"
	"strings"
	"time"

	ff3 "go.horizondigital.dev/fpe-ff3/pkg/ff3"
)

const (
	defaultIterations = 1000
	defaultMinLength  = 6
	defaultMaxLength  = 20
)

type options struct {
	iterations   int
	alphabets    []string
	minLength    int
	maxLength    int
	quick        bool
	seedProvided bool
	seed         int64
}

func parseOptions() (options, bool) {
	var opts options

	alphabetsFlag := flag.String("alphabets", "", "comma-separated list (default: digits,hex-lower,base36-lower,base62)")
	minLengthFlag := flag.Int("min-length", defaultMinLength, "minimum plaintext length")
	maxLengthFlag := flag.Int("max-length", defaultMaxLength, "maximum plaintext length")
	quickFlag := flag.Bool("quick", false, "run 100 iterations (fast test)")
	seedFlag := flag.Int64("seed", -1, "random seed for reproducibility")

	flag.Usage = func() {
		fmt.Fprintf(os.Stderr, "FF3 Stress Test Tool\n\n")
		fmt.Fprintf(os.Stderr, "Usage: %s [OPTIONS] [ITERATIONS]\n\n", os.Args[0])
		fmt.Fprintf(os.Stderr, "Options:\n")
		fmt.Fprintf(os.Stderr, "  --alphabets LIST      Comma-separated list (default: digits,hex-lower,base36-lower,base62)\n")
		fmt.Fprintf(os.Stderr, "  --min-length N        Minimum plaintext length (default: %d)\n", defaultMinLength)
		fmt.Fprintf(os.Stderr, "  --max-length N        Maximum plaintext length (default: %d)\n", defaultMaxLength)
		fmt.Fprintf(os.Stderr, "  --quick               Run 100 iterations (fast test)\n")
		fmt.Fprintf(os.Stderr, "  --seed N              Random seed for reproducibility\n")
	}

	flag.Parse()

	opts.iterations = defaultIterations
	if flag.NArg() > 0 {
		it, err := parsePositiveInt(flag.Arg(0))
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error: %v\n", err)
			return opts, false
		}
		opts.iterations = it
	}

	opts.minLength = *minLengthFlag
	opts.maxLength = *maxLengthFlag

	if opts.minLength < 2 {
		fmt.Fprintf(os.Stderr, "Error: --min-length must be at least 2.\n")
		return opts, false
	}
	if opts.maxLength < opts.minLength {
		fmt.Fprintf(os.Stderr, "Error: --max-length must be greater than or equal to --min-length.\n")
		return opts, false
	}
	if opts.iterations <= 0 {
		fmt.Fprintf(os.Stderr, "Error: iterations must be greater than 0.\n")
		return opts, false
	}

	if *alphabetsFlag == "" {
		opts.alphabets = []string{"digits", "hex-lower", "base36-lower", "base62"}
	} else {
		parts := strings.Split(*alphabetsFlag, ",")
		for _, p := range parts {
			p = strings.TrimSpace(p)
			if p != "" {
				opts.alphabets = append(opts.alphabets, p)
			}
		}
		if len(opts.alphabets) == 0 {
			fmt.Fprintf(os.Stderr, "Error: --alphabets list cannot be empty.\n")
			return opts, false
		}
	}

	if *quickFlag {
		opts.quick = true
		opts.iterations = 100
	}

	if *seedFlag >= 0 {
		opts.seedProvided = true
		opts.seed = *seedFlag
	}

	return opts, true
}

func parsePositiveInt(value string) (int, error) {
	var x int
	_, err := fmt.Sscanf(value, "%d", &x)
	if err != nil || x <= 0 {
		return 0, fmt.Errorf("invalid positive integer: %s", value)
	}
	return x, nil
}

type alphabet struct {
	name    string
	charset string
	spec    ff3.AlphabetSpec
}

func buildAlphabetMap() map[string]alphabet {
	return map[string]alphabet{
		"digits":       {name: "digits", charset: ff3.AlphaDigits, spec: ff3.SpecDigits},
		"hex-lower":    {name: "hex-lower", charset: ff3.AlphaHexLower, spec: ff3.SpecHexLower},
		"hex-upper":    {name: "hex-upper", charset: ff3.AlphaHexUpper, spec: ff3.SpecHexUpper},
		"base36-lower": {name: "base36-lower", charset: ff3.AlphaBase36Low, spec: ff3.SpecBase36Low},
		"base36-upper": {name: "base36-upper", charset: ff3.AlphaBase36Up, spec: ff3.SpecBase36Up},
		"base62":       {name: "base62", charset: ff3.AlphaBase62, spec: ff3.SpecBase62},
	}
}

func main() {
	opts, ok := parseOptions()
	if !ok {
		os.Exit(1)
	}

	alphabetMap := buildAlphabetMap()
	selected := make([]alphabet, 0, len(opts.alphabets))
	for _, name := range opts.alphabets {
		info, exists := alphabetMap[name]
		if !exists {
			fmt.Fprintf(os.Stderr, "Error: unknown alphabet '%s'.\n", name)
			os.Exit(1)
		}
		selected = append(selected, info)
	}

	var rng *mathrand.Rand
	if opts.seedProvided {
		rng = mathrand.New(mathrand.NewSource(opts.seed))
	} else {
		rng = mathrand.New(mathrand.NewSource(time.Now().UnixNano()))
	}

	fmt.Println("FF3 Stress Test v1.0")
	fmt.Println("====================")
	fmt.Println()
	fmt.Println("Warning: FF3 was withdrawn by NIST; run for education and research only.")
	fmt.Println()
	fmt.Println("Test configuration")
	fmt.Printf("  Iterations per alphabet: %d\n", opts.iterations)
	fmt.Println("  Random key/tweak generation: enabled")
	fmt.Printf("  String length range: %d-%d characters\n", opts.minLength, opts.maxLength)
	fmt.Printf("  Alphabets: %s\n", strings.Join(opts.alphabets, ", "))
	fmt.Println()

	totalTests := 0
	totalFailures := 0

	startAll := time.Now()

	for _, alpha := range selected {
		fmt.Printf("Testing %s...\n", alpha.name)
		fmt.Printf("  Alphabet: %s (radix %d)\n", alpha.charset, len(alpha.charset))

		passed := 0
		failed := 0
		interval := max(1, opts.iterations/10)

		for i := 0; i < opts.iterations; i++ {
			key := randomBytes(16)
			tweak := randomBytes(8)
			length := rng.Intn(opts.maxLength-opts.minLength+1) + opts.minLength
			plaintext := generatePlaintext(alpha.charset, length, rng)

			cipher, err := ff3.FromSpec(key, tweak, alpha.spec)
			if err != nil {
				failed++
				printFailure(key, tweak, plaintext, "", "", "cipher initialization failed")
				continue
			}

			ciphertext, err := cipher.Encrypt(plaintext, nil)
			if err != nil {
				failed++
				printFailure(key, tweak, plaintext, "", "", err.Error())
				continue
			}

			decrypted, err := cipher.Decrypt(ciphertext, nil)
			if err != nil {
				failed++
				printFailure(key, tweak, plaintext, ciphertext, "", err.Error())
				continue
			}
			if decrypted != plaintext {
				failed++
				printFailure(key, tweak, plaintext, ciphertext, decrypted, "round-trip mismatch")
				continue
			}

			passed++

			if (i+1)%interval == 0 || i+1 == opts.iterations {
				percent := ((i + 1) * 100) / opts.iterations
				fmt.Printf("  Progress: %d/%d (%d%%)\n", i+1, opts.iterations, percent)
			}
		}

		fmt.Printf("  Passed: %d/%d\n", passed, passed+failed)
		fmt.Printf("  Failed: %d/%d\n\n", failed, passed+failed)

		totalTests += passed + failed
		totalFailures += failed
	}

	selapsed := time.Since(startAll)
	fmt.Println("Summary")
	fmt.Printf("  Total tests: %d\n", totalTests)
	fmt.Printf("  Failures: %d\n", totalFailures)
	fmt.Printf("  Duration: %d ms\n", selapsed.Milliseconds())
	if selapsed.Milliseconds() > 0 {
		ops := float64(totalTests) * 1000.0 / float64(selapsed.Milliseconds())
		fmt.Printf("  Throughput: %.2f tests/sec\n", ops)
	}
	if totalFailures == 0 {
		fmt.Println("  Result: all stress tests passed")
		os.Exit(0)
	}
	fmt.Println("  Result: failures detected")
	os.Exit(1)
}

func randomBytes(length int) []byte {
	buf := make([]byte, length)
	if _, err := crand.Read(buf); err != nil {
		panic(err)
	}
	return buf
}

func generatePlaintext(charset string, length int, rng *mathrand.Rand) string {
	var sb strings.Builder
	sb.Grow(length)
	for i := 0; i < length; i++ {
		idx := rng.Intn(len(charset))
		sb.WriteByte(charset[idx])
	}
	return sb.String()
}

func printFailure(key, tweak []byte, plaintext, ciphertext, decrypted, detail string) {
	fmt.Println("  Round-trip failed:")
	fmt.Printf("    Key: %s\n", hex.EncodeToString(key))
	fmt.Printf("    Tweak: %s\n", hex.EncodeToString(tweak))
	fmt.Printf("    Plaintext: \"%s\"\n", plaintext)
	if ciphertext != "" {
		fmt.Printf("    Ciphertext: \"%s\"\n", ciphertext)
	}
	if decrypted != "" {
		fmt.Printf("    Decrypted: \"%s\"\n", decrypted)
	}
	if detail != "" {
		fmt.Printf("    Detail: \"%s\"\n", detail)
	}
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}
