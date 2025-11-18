// ff3-validate - NIST FF3 vector correctness validation CLI
package main

import (
	"encoding/hex"
	"encoding/json"
	"flag"
	"fmt"
	"os"
	"strings"

	ff3 "go.horizondigital.dev/fpe-ff3/pkg/ff3"
)

type NISTVector struct {
	Sample     int    `json:"sample"`
	Algorithm  string `json:"algorithm"`
	Key        string `json:"key"`
	Radix      int    `json:"radix"`
	Plaintext  string `json:"plaintext"`
	Tweak      string `json:"tweak"`
	Ciphertext string `json:"ciphertext"`
}

type NISTDoc struct {
	Description string       `json:"description"`
	Source      string       `json:"source"`
	Vectors     []NISTVector `json:"vectors"`
}

type ResultLine struct {
	CaseID string `json:"case_id"`
	Stage  string `json:"stage"`
	OK     bool   `json:"ok"`
	Msg    string `json:"msg,omitempty"`
}

func main() {
	vectorsPath := flag.String("vectors", "../../shared/test-vectors/nist_ff3_official_vectors.json", "path to NIST FF3 vectors json")
	casePrefix := flag.String("case", "", "optional filter: prefix on Algorithm or Sample")
	failFast := flag.Bool("fail-fast", false, "stop on first failure")
	verbose := flag.Bool("verbose", false, "show success cases too")
	jsonOut := flag.String("json-out", "", "write results to file instead of stdout")
	flag.Parse()

	doc, err := loadVectors(*vectorsPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "ERR load vectors: %v\n", err)
		os.Exit(2)
	}

	var results []ResultLine
	failures := 0

	for _, v := range doc.Vectors {
		caseID := fmt.Sprintf("%s#%d", v.Algorithm, v.Sample)
		if *casePrefix != "" && !strings.HasPrefix(caseID, *casePrefix) && !strings.HasPrefix(v.Algorithm, *casePrefix) {
			continue
		}

		key, err := hex.DecodeString(v.Key)
		if err != nil {
			result := emitResult(caseID, "decode-key", false, err.Error())
			results = append(results, result)
			failures++
			if *failFast {
				break
			}
			continue
		}

		tweak, err := hex.DecodeString(v.Tweak)
		if err != nil {
			result := emitResult(caseID, "decode-tweak", false, err.Error())
			results = append(results, result)
			failures++
			if *failFast {
				break
			}
			continue
		}

		cipher, err := buildCipher(v.Radix, key, tweak)
		if err != nil {
			result := emitResult(caseID, "build-cipher", false, err.Error())
			results = append(results, result)
			failures++
			if *failFast {
				break
			}
			continue
		}

		// Test encryption
		ct, err := cipher.Encrypt(v.Plaintext, nil)
		if err != nil {
			result := emitResult(caseID, "encrypt", false, err.Error())
			results = append(results, result)
			failures++
			if *failFast {
				break
			}
			continue
		}

		encryptOK := (ct == v.Ciphertext)
		if !encryptOK {
			result := emitResult(caseID, "encrypt", false, fmt.Sprintf("expected %s, got %s", v.Ciphertext, ct))
			results = append(results, result)
			failures++
		} else if *verbose {
			result := emitResult(caseID, "encrypt", true, "")
			results = append(results, result)
		}

		// Test roundtrip
		pt, err := cipher.Decrypt(ct, nil)
		if err != nil {
			result := emitResult(caseID, "decrypt", false, err.Error())
			results = append(results, result)
			failures++
			if *failFast {
				break
			}
			continue
		}

		roundtripOK := (pt == v.Plaintext)
		if !roundtripOK {
			result := emitResult(caseID, "roundtrip", false, fmt.Sprintf("expected %s, got %s", v.Plaintext, pt))
			results = append(results, result)
			failures++
		} else if *verbose {
			result := emitResult(caseID, "roundtrip", true, "")
			results = append(results, result)
		}

		if *failFast && (!encryptOK || !roundtripOK) {
			break
		}
	}

	// Output results
	output := outputJSON(results)
	if *jsonOut != "" {
		err := os.WriteFile(*jsonOut, output, 0644)
		if err != nil {
			fmt.Fprintf(os.Stderr, "ERR writing output: %v\n", err)
			os.Exit(2)
		}
	} else {
		os.Stdout.Write(output)
	}

	if failures > 0 {
		os.Exit(1)
	}
}

func loadVectors(path string) (*NISTDoc, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	var doc NISTDoc
	if err := json.Unmarshal(data, &doc); err != nil {
		return nil, err
	}
	return &doc, nil
}

func buildCipher(radix int, key, tweak []byte) (*ff3.FF3, error) {
	switch radix {
	case 10:
		return ff3.Digits(key, tweak)
	case 16:
		return ff3.HexLower(key, tweak)
	case 36:
		return ff3.Base36Lower(key, tweak)
	case 62:
		return ff3.FromSpec(key, tweak, ff3.SpecBase62)
	default:
		// Handle arbitrary radix by using appropriate alphabet
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
	}
}

func emitResult(caseID, stage string, ok bool, msg string) ResultLine {
	return ResultLine{
		CaseID: caseID,
		Stage:  stage,
		OK:     ok,
		Msg:    msg,
	}
}

func outputJSON(results []ResultLine) []byte {
	var lines []string
	for _, r := range results {
		b, _ := json.Marshal(r)
		lines = append(lines, string(b))
	}
	return []byte(strings.Join(lines, "\n") + "\n")
}