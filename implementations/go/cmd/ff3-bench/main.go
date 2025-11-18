// ff3-bench - Micro-benchmarking CLI with warmup
package main

import (
	"encoding/hex"
	"encoding/json"
	"flag"
	"fmt"
	"os"
	"runtime"
	"strconv"
	"strings"
	"time"

	ff3 "go.horizondigital.dev/fpe-ff3/pkg/ff3"
)

type BenchmarkParameters struct {
	Alphabet        string `json:"alphabet"`
	Radix           int    `json:"radix"`
	Length          int    `json:"length"`
	KeyBits         int    `json:"key_bits"`
	KeyFingerprint  string `json:"key_fingerprint"`
	Tweak           string `json:"tweak"`
}

type BenchmarkResult struct {
	Name       string               `json:"name"`
	TestCase   string               `json:"test_case"`
	Parameters BenchmarkParameters  `json:"parameters"`
	Iterations int                  `json:"iterations"`
	ElapsedNs  int64                `json:"elapsed_ns"`
	NsPerOp    float64              `json:"ns_per_op"`
	OpsPerSec  float64              `json:"ops_per_sec"`
	Checksum   string               `json:"checksum"`
}

type PlatformInfo struct {
	OS    string `json:"os"`
	Arch  string `json:"arch"`
	CPU   string `json:"cpu"`
	Cores int    `json:"cores"`
}

type MetadataInfo struct {
	Version   string       `json:"version"`
	Timestamp string       `json:"timestamp"`
	Language  string       `json:"language"`
	Runtime   string       `json:"runtime"`
	Platform  PlatformInfo `json:"platform"`
}

type ConfigurationInfo struct {
	Seed             int `json:"seed"`
	WarmupIterations int `json:"warmup_iterations"`
}

type SummaryInfo struct {
	TotalTests       int     `json:"total_tests"`
	TotalDurationSec float64 `json:"total_duration_sec"`
	Checksum         string  `json:"checksum"`
}

type BenchmarkReport struct {
	Metadata      MetadataInfo      `json:"metadata"`
	Configuration ConfigurationInfo `json:"configuration"`
	Benchmarks    []BenchmarkResult `json:"benchmarks"`
	Summary       SummaryInfo       `json:"summary"`
}

type BenchmarkConfig struct {
	Alphabet   string   `json:"alphabet"`
	Radix      int      `json:"radix"`
	Lengths    []int    `json:"lengths"`
	Cases      []string `json:"cases"`
	Iterations int      `json:"iterations"`
	Warmup     int      `json:"warmup"`
	Key        string   `json:"key"`
	Tweak      string   `json:"tweak"`
	Seed       int      `json:"seed"`
}

func main() {
	configFile := flag.String("config", "", "JSON config file")
	alphabetName := flag.String("alphabet", "digits", "alphabet: digits|hex|base36|base62|custom")
	radix := flag.Int("radix", 10, "radix (2..62), used when alphabet=custom")
	lengthsCSV := flag.String("lengths", "9,12,16", "comma-separated list of plaintext lengths")
	casesCSV := flag.String("cases", "enc,dec,roundtrip", "comma-separated list of cases")
	iterations := flag.Int("iterations", 200000, "measured iterations per case")
	warmup := flag.Int("warmup", 20000, "warmup iterations per case")
	keyHex := flag.String("key", "000102030405060708090A0B0C0D0E0F", "hex-encoded key")
	tweakHex := flag.String("tweak", "00010203040506A1", "hex-encoded tweak")
	seed := flag.Int("seed", 42, "random seed for reproducibility")
	quick := flag.Bool("quick", false, "reduce iterations and warmup for faster runs")
	jsonOut := flag.String("json-out", "", "write results JSON to file")
	verbose := flag.Bool("verbose", false, "show progress messages")
	flag.Parse()

	// Load config file if provided
	if *configFile != "" {
		data, err := os.ReadFile(*configFile)
		if err != nil {
			fmt.Fprintf(os.Stderr, "ERR reading config file: %v\n", err)
			os.Exit(2)
		}
		var config BenchmarkConfig
		if err := json.Unmarshal(data, &config); err != nil {
			fmt.Fprintf(os.Stderr, "ERR parsing config file: %v\n", err)
			os.Exit(2)
		}
		// Override defaults with config file values
		if config.Alphabet != "" {
			*alphabetName = config.Alphabet
		}
		if config.Radix > 0 {
			*radix = config.Radix
		}
		if len(config.Lengths) > 0 {
			*lengthsCSV = intsToCSV(config.Lengths)
		}
		if len(config.Cases) > 0 {
			*casesCSV = strings.Join(config.Cases, ",")
		}
		if config.Iterations > 0 {
			*iterations = config.Iterations
		}
		if config.Warmup > 0 {
			*warmup = config.Warmup
		}
		if config.Key != "" {
			*keyHex = config.Key
		}
		if config.Tweak != "" {
			*tweakHex = config.Tweak
		}
		if config.Seed > 0 {
			*seed = config.Seed
		}
	}

	if *quick {
		*iterations = *iterations / 10
		*warmup = *warmup / 10
	}

	lengths := parseLengths(*lengthsCSV)
	cases := parseCases(*casesCSV)

	key, err := hex.DecodeString(*keyHex)
	if err != nil {
		fmt.Fprintf(os.Stderr, "ERR decoding key: %v\n", err)
		os.Exit(2)
	}

	tweak, err := hex.DecodeString(*tweakHex)
	if err != nil {
		fmt.Fprintf(os.Stderr, "ERR decoding tweak: %v\n", err)
		os.Exit(2)
	}

	cipher, err := buildCipher(*radix, *alphabetName, key, tweak)
	if err != nil {
		fmt.Fprintf(os.Stderr, "ERR building cipher: %v\n", err)
		os.Exit(2)
	}

	var benchmarks []BenchmarkResult
	checksum := uint32(0)
	totalStart := time.Now()

	for _, length := range lengths {
		for _, benchCase := range cases {
			if *verbose {
				fmt.Fprintf(os.Stderr, "Running %s_len%d_radix%d...\n", benchCase, length, *radix)
			}
			result, cs := runBenchmark(cipher, benchCase, length, *radix, *alphabetName, *iterations, *warmup, key, *tweakHex, *seed)
			benchmarks = append(benchmarks, result)
			checksum ^= cs
		}
	}

	totalDuration := time.Since(totalStart)

	// Get platform info
	osName := runtime.GOOS
	archName := runtime.GOARCH
	cpuName := "unknown" // Go doesn't provide easy CPU model detection
	cores := runtime.NumCPU()

	// Generate report
	report := BenchmarkReport{
		Metadata: MetadataInfo{
			Version:   "1.0",
			Timestamp: time.Now().UTC().Format(time.RFC3339),
			Language:  "go",
			Runtime:   runtime.Version(),
			Platform: PlatformInfo{
				OS:    osName,
				Arch:  archName,
				CPU:   cpuName,
				Cores: cores,
			},
		},
		Configuration: ConfigurationInfo{
			Seed:             *seed,
			WarmupIterations: *warmup,
		},
		Benchmarks: benchmarks,
		Summary: SummaryInfo{
			TotalTests:       len(benchmarks),
			TotalDurationSec: totalDuration.Seconds(),
			Checksum:         fmt.Sprintf("%08x", checksum),
		},
	}

	// Output results
	output, err := json.MarshalIndent(report, "", "  ")
	if err != nil {
		fmt.Fprintf(os.Stderr, "ERR marshaling JSON: %v\n", err)
		os.Exit(2)
	}

	if *jsonOut != "" {
		err := os.WriteFile(*jsonOut, output, 0644)
		if err != nil {
			fmt.Fprintf(os.Stderr, "ERR writing output: %v\n", err)
			os.Exit(2)
		}
	} else {
		os.Stdout.Write(output)
		fmt.Println()
	}
}

func runBenchmark(cipher *ff3.FF3, benchCase string, length, radix int, alphabetName string, n, warmup int, key []byte, tweakHex string, seed int) (BenchmarkResult, uint32) {
	// Generate test inputs
	inputs := generateInputs(64, length, radix, alphabetName, seed) // ring buffer of 64 inputs
	var precomputedCTs []string

	// Pre-compute ciphertexts for decrypt benchmarks
	if benchCase == "dec" {
		for _, pt := range inputs {
			ct, _ := cipher.Encrypt(pt, nil)
			precomputedCTs = append(precomputedCTs, ct)
		}
	}

	// Warmup phase
	for i := 0; i < warmup; i++ {
		pt := inputs[i%len(inputs)]
		switch benchCase {
		case "enc":
			cipher.Encrypt(pt, nil)
		case "dec":
			ct := precomputedCTs[i%len(precomputedCTs)]
			cipher.Decrypt(ct, nil)
		case "roundtrip":
			ct, _ := cipher.Encrypt(pt, nil)
			cipher.Decrypt(ct, nil)
		}
	}

	// Measured phase
	checksum := uint32(0)
	start := time.Now()

	for i := 0; i < n; i++ {
		pt := inputs[i%len(inputs)]
		switch benchCase {
		case "enc":
			result, _ := cipher.Encrypt(pt, nil)
			checksum ^= hashString(result)
		case "dec":
			ct := precomputedCTs[i%len(precomputedCTs)]
			result, _ := cipher.Decrypt(ct, nil)
			checksum ^= hashString(result)
		case "roundtrip":
			ct, _ := cipher.Encrypt(pt, nil)
			result, _ := cipher.Decrypt(ct, nil)
			checksum ^= hashString(result)
		}
	}

	elapsed := time.Since(start)
	nsPerOp := float64(elapsed.Nanoseconds()) / float64(n)
	opsPerSec := float64(n) / elapsed.Seconds()

	// Create key fingerprint (first 8 hex chars)
	keyFingerprint := fmt.Sprintf("%X", key[:min(4, len(key))])

	benchResult := BenchmarkResult{
		Name:     fmt.Sprintf("%s_len%d_radix%d", benchCase, length, radix),
		TestCase: benchCase,
		Parameters: BenchmarkParameters{
			Alphabet:       alphabetName,
			Radix:          radix,
			Length:         length,
			KeyBits:        len(key) * 8,
			KeyFingerprint: keyFingerprint,
			Tweak:          tweakHex,
		},
		Iterations: n,
		ElapsedNs:  elapsed.Nanoseconds(),
		NsPerOp:    nsPerOp,
		OpsPerSec:  opsPerSec,
		Checksum:   fmt.Sprintf("%08x", checksum),
	}

	return benchResult, checksum
}

func buildCipher(radix int, alphabetName string, key, tweak []byte) (*ff3.FF3, error) {
	switch alphabetName {
	case "digits":
		return ff3.Digits(key, tweak)
	case "hex":
		return ff3.HexLower(key, tweak)
	case "base36":
		return ff3.Base36Lower(key, tweak)
	case "base62":
		return ff3.FromSpec(key, tweak, ff3.SpecBase62)
	case "custom":
		var spec ff3.AlphabetSpec
		switch {
		case radix <= 10:
			spec = ff3.AlphabetSpec{Charset: ff3.AlphaDigits[:radix]}
		case radix <= 36:
			spec = ff3.AlphabetSpec{Charset: ff3.AlphaBase36Low[:radix]}
		case radix <= 62:
			spec = ff3.AlphabetSpec{Charset: ff3.AlphaBase62[:radix]}
		default:
			return nil, fmt.Errorf("unsupported radix %d (max 62)", radix)
		}
		return ff3.FromSpec(key, tweak, spec)
	default:
		return nil, fmt.Errorf("unsupported alphabet: %s", alphabetName)
	}
}

func generateInputs(count, length, radix int, alphabetName string, seed int) []string {
	var charset string
	switch alphabetName {
	case "digits":
		charset = ff3.AlphaDigits
	case "hex":
		charset = ff3.AlphaHexLower
	case "base36":
		charset = ff3.AlphaBase36Low
	case "base62":
		charset = ff3.AlphaBase62
	case "custom":
		switch {
		case radix <= 10:
			charset = ff3.AlphaDigits[:radix]
		case radix <= 36:
			charset = ff3.AlphaBase36Low[:radix]
		case radix <= 62:
			charset = ff3.AlphaBase62[:radix]
		default:
			charset = ff3.AlphaBase62[:min(radix, 62)]
		}
	default:
		charset = ff3.AlphaDigits
	}

	inputs := make([]string, count)
	// Use seed-based generation for reproducibility
	hashSeed := uint32(seed)
	for i := 0; i < count; i++ {
		result := make([]byte, length)
		for j := 0; j < length; j++ {
			// Simple LCG for deterministic pseudo-random
			hashSeed = hashSeed*1664525 + 1013904223
			result[j] = charset[int(hashSeed)%len(charset)]
		}
		inputs[i] = string(result)
	}
	return inputs
}

func intsToCSV(ints []int) string {
	strs := make([]string, len(ints))
	for i, v := range ints {
		strs[i] = strconv.Itoa(v)
	}
	return strings.Join(strs, ",")
}

func parseLengths(csv string) []int {
	var lengths []int
	for _, part := range strings.Split(csv, ",") {
		if length, err := strconv.Atoi(strings.TrimSpace(part)); err == nil && length > 0 {
			lengths = append(lengths, length)
		}
	}
	if len(lengths) == 0 {
		lengths = []int{9}
	}
	return lengths
}

func parseCases(csv string) []string {
	var cases []string
	valid := map[string]bool{"enc": true, "dec": true, "roundtrip": true}
	for _, part := range strings.Split(csv, ",") {
		case_name := strings.TrimSpace(part)
		if valid[case_name] {
			cases = append(cases, case_name)
		}
	}
	if len(cases) == 0 {
		cases = []string{"enc"}
	}
	return cases
}

func hashString(s string) uint32 {
	h := uint32(0)
	for _, b := range []byte(s) {
		h = h*31 + uint32(b)
	}
	return h
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}