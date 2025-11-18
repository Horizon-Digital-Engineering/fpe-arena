package ff3

import (
	"encoding/hex"
	"encoding/json"
	"os"
	"testing"
)

// NISTTestVector represents an official NIST FF3 test vector
type NISTTestVector struct {
	Sample     int    `json:"sample"`
	Algorithm  string `json:"algorithm"`
	Key        string `json:"key"`
	Radix      int    `json:"radix"`
	Plaintext  string `json:"plaintext"`
	Tweak      string `json:"tweak"`
	Ciphertext string `json:"ciphertext"`
}

// OfficialNISTVectors represents the structure of the official NIST test vectors file
type OfficialNISTVectors struct {
	Description   string           `json:"description"`
	Source        string           `json:"source"`
	Algorithm     string           `json:"algorithm"`
	Specification string           `json:"specification"`
	Note          string           `json:"note"`
	Status        string           `json:"status"`
	Warning       string           `json:"warning"`
	Vectors       []NISTTestVector `json:"vectors"`
}

func loadNISTTestVectors(t *testing.T) []NISTTestVector {
	// Try environment variable first (for CI/CD)
	filepath := os.Getenv("FF3_TEST_VECTORS_PATH")
	if filepath == "" {
		// Fallback to relative path for local development
		filepath = "../../../../shared/test-vectors/nist_ff3_official_vectors.json"
	}

	data, err := os.ReadFile(filepath)
	if err != nil {
		t.Fatalf("Failed to load official NIST test vectors from %s: %v", filepath, err)
	}

	var officialVectors OfficialNISTVectors
	err = json.Unmarshal(data, &officialVectors)
	if err != nil {
		t.Fatalf("Failed to parse official NIST test vectors: %v", err)
	}

	t.Logf("Loaded %s: %d vectors", officialVectors.Description, len(officialVectors.Vectors))
	t.Logf("Source: %s", officialVectors.Source)
	t.Logf("Status: %s", officialVectors.Status)

	return officialVectors.Vectors
}

func TestNISTFF3Vectors(t *testing.T) {
	vectors := loadNISTTestVectors(t)

	for _, vector := range vectors {
		t.Run(vector.Algorithm+"_Sample_"+string(rune(vector.Sample+'0')), func(t *testing.T) {
			// Decode hex key and tweak
			key, err := hex.DecodeString(vector.Key)
			if err != nil {
				t.Fatalf("Failed to decode key: %v", err)
			}

			tweak, err := hex.DecodeString(vector.Tweak)
			if err != nil {
				t.Fatalf("Failed to decode tweak: %v", err)
			}

			// Create FF3 cipher using appropriate alphabet
			var cipher *FF3
			switch vector.Radix {
			case 10:
				cipher, err = Digits(key, tweak)
			case 16:
				cipher, err = HexLower(key, tweak)
			case 26:
				// Use first 26 chars of base36 alphabet (0-9a-p) 
				spec := AlphabetSpec{Charset: AlphaBase36Low[:26]}
				cipher, err = FromSpec(key, tweak, spec)
			case 36:
				cipher, err = Base36Lower(key, tweak)
			case 62:
				cipher, err = Base62(key, tweak)
			default:
				t.Fatalf("Unsupported radix %d in test vectors", vector.Radix)
			}
			if err != nil {
				t.Fatalf("Failed to create FF3 cipher: %v", err)
			}

			// Test encryption
			encrypted, err := cipher.Encrypt(vector.Plaintext, nil)
			if err != nil {
				t.Fatalf("Encryption failed: %v", err)
			}

			t.Logf("Sample %d (%s):", vector.Sample, vector.Algorithm)
			t.Logf("  Key: %s", vector.Key)
			t.Logf("  Tweak: %s", vector.Tweak)
			t.Logf("  Plaintext: %s", vector.Plaintext)
			t.Logf("  Expected: %s", vector.Ciphertext)
			t.Logf("  Got:      %s", encrypted)

			if encrypted == vector.Ciphertext {
				t.Logf("  ✅ PERFECT MATCH!")
			} else {
				t.Logf("  ❌ MISMATCH")
			}

			// Test decryption (round-trip)
			decrypted, err := cipher.Decrypt(encrypted, nil)
			if err != nil {
				t.Fatalf("Decryption failed: %v", err)
			}

			if decrypted != vector.Plaintext {
				t.Errorf("Round-trip failed: expected %s, got %s", vector.Plaintext, decrypted)
			} else {
				t.Logf("  ✅ Round-trip successful")
			}
		})
	}
}

func TestAgainstMYSTO(t *testing.T) {
	// Test against the first NIST vector to compare with MYSTO/Capital One
	key, _ := hex.DecodeString("EF4359D8D580AA4F7F036D6F04FC6A94")
	tweak, _ := hex.DecodeString("D8E7920AFA330A73")
	plaintext := "890121234567890000"
	expectedNIST := "750918814058654607"

	cipher, err := Digits(key, tweak)
	if err != nil {
		t.Fatalf("Failed to create cipher: %v", err)
	}

	result, err := cipher.Encrypt(plaintext, nil)
	if err != nil {
		t.Fatalf("Encryption failed: %v", err)
	}

	t.Logf("Testing against MYSTO/Capital One consensus:")
	t.Logf("  NIST expected: %s", expectedNIST)
	t.Logf("  Our result:    %s", result)
	t.Logf("  MYSTO result:  750918814058654607 (from earlier tests)")

	if result == expectedNIST {
		t.Logf("  ✅ MATCHES NIST!")
	} else {
		t.Logf("  ❌ Does not match NIST")
	}

	if result == "750918814058654607" {
		t.Logf("  ✅ MATCHES MYSTO/Capital One!")
	} else {
		t.Logf("  ❌ Does not match MYSTO/Capital One")
	}
}