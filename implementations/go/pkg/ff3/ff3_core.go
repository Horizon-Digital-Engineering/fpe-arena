// Package ff3 provides the core FF3 Format Preserving Encryption algorithm.
// This file contains the pure cryptographic implementation.
package ff3

import (
	"crypto/aes"
	"crypto/cipher"
	"fmt"
	"math/big"
)

// FF3Cipher represents the core FF3 cipher implementation
type FF3Cipher struct {
	radix  int
	aes    cipher.Block
	tweak  []byte
	minLen int
	maxLen int
}

// NewFF3Cipher creates a new FF3 cipher with the specified radix, key, and tweak.
// The radix must be between 2 and 62 for this experimental repo.
// The key must be 16, 24, or 32 bytes (AES-128, AES-192, or AES-256).
// The tweak must be exactly 8 bytes (64 bits) for FF3.
func NewFF3Cipher(radix int, key, tweak []byte) (*FF3Cipher, error) {
	// Validate radix - relaxed for experimental repo
	if radix < 2 || radix > 62 {
		return nil, fmt.Errorf("radix must be between 2 and 62, got %d", radix)
	}

	// Validate key length
	if len(key) != 16 && len(key) != 24 && len(key) != 32 {
		return nil, fmt.Errorf("key length must be 16, 24, or 32 bytes, got %d", len(key))
	}

	// Validate tweak length (FF3 requires exactly 64 bits = 8 bytes)
	if len(tweak) != 8 {
		return nil, fmt.Errorf("tweak must be exactly 8 bytes (64 bits) for FF3, got %d", len(tweak))
	}

	// Create AES cipher with FF3 byte-reversal convention
	aesCipher, err := createAESCipher(key)
	if err != nil {
		return nil, fmt.Errorf("failed to create AES cipher: %w", err)
	}

	// Copy tweak to avoid external modifications
	tweakCopy := make([]byte, len(tweak))
	copy(tweakCopy, tweak)

	// Calculate maximum length for FF3 (more permissive for experimental use)
	maxLen := 56 // Conservative maximum for most practical use cases
	if radix >= 2 && radix <= 36 {
		// For small radixes, allow reasonable lengths
		maxLen = 32
	}

	return &FF3Cipher{
		radix:  radix,
		aes:    aesCipher,
		tweak:  tweakCopy,
		minLen: 2,      // Minimum length for FF3
		maxLen: maxLen, // Maximum length based on practical constraints
	}, nil
}

// ff3Encrypt performs the core FF3 encryption algorithm
// Input: plaintext as int array, tweak bytes
// Output: ciphertext as int array
func (c *FF3Cipher) ff3Encrypt(plaintext []int, tweak []byte) []int {
	n := len(plaintext)
	
	// Split plaintext into left and right halves (u = ceil(n/2), v = n - u)
	u := (n + 1) / 2  // This gives ceil(n/2)
	v := n - u
	
	A := make([]int, u)
	B := make([]int, v)
	copy(A, plaintext[:u])
	copy(B, plaintext[u:])

	// Perform 8 Feistel rounds according to FF3 specification
	for i := 0; i < 8; i++ {
		if i%2 == 0 {
			// Even round: use B to update A
			W := c.calculateW(tweak, i, B)
			P := c.calculateP(i, W, B)
			m := c.calculateModulus(u)
			
			// FF3 uses reversed digit order: NUM_radix(REV(A))
			reversedA := c.reverseDigits(A)
			aNum := c.numArrayToBigInt(reversedA)
			
			// c = (NUM_radix(REV(A)) + NUM(S)) mod radix^u
			Y := new(big.Int).Add(aNum, P)
			Y.Mod(Y, m)
			
			// C = REV(STR_radix(c))
			newDigits := c.bigIntToNumArray(Y, u)
			A = c.reverseDigits(newDigits)
		} else {
			// Odd round: use A to update B  
			W := c.calculateW(tweak, i, A)
			P := c.calculateP(i, W, A)
			m := c.calculateModulus(v)
			
			// FF3 uses reversed digit order: NUM_radix(REV(B))
			reversedB := c.reverseDigits(B)
			bNum := c.numArrayToBigInt(reversedB)
			
			// c = (NUM_radix(REV(B)) + NUM(S)) mod radix^v
			Y := new(big.Int).Add(bNum, P)
			Y.Mod(Y, m)
			
			// C = REV(STR_radix(c))
			newDigits := c.bigIntToNumArray(Y, v)
			B = c.reverseDigits(newDigits)
		}
	}

	// Combine final result
	result := make([]int, n)
	copy(result, A)
	copy(result[u:], B)
	return result
}

// ff3Decrypt performs the core FF3 decryption algorithm
// Input: ciphertext as int array, tweak bytes
// Output: plaintext as int array
func (c *FF3Cipher) ff3Decrypt(ciphertext []int, tweak []byte) []int {
	n := len(ciphertext)
	
	// Split ciphertext into left and right halves (u = ceil(n/2), v = n - u)
	u := (n + 1) / 2  // This gives ceil(n/2)
	v := n - u
	
	A := make([]int, u)
	B := make([]int, v)
	copy(A, ciphertext[:u])
	copy(B, ciphertext[u:])

	// Perform 8 Feistel rounds in reverse according to FF3 specification
	for i := 7; i >= 0; i-- {
		if i%2 == 0 {
			// Even round: use B to recover A
			W := c.calculateW(tweak, i, B)
			P := c.calculateP(i, W, B)
			m := c.calculateModulus(u)
			
			// FF3 decryption: NUM_radix(REV(A)) = (c - NUM(S)) mod radix^u
			reversedA := c.reverseDigits(A)
			aNum := c.numArrayToBigInt(reversedA)
			
			Y := new(big.Int).Sub(aNum, P)
			Y.Mod(Y, m)
			
			// A = REV(STR_radix(result))
			newDigits := c.bigIntToNumArray(Y, u)
			A = c.reverseDigits(newDigits)
		} else {
			// Odd round: use A to recover B
			W := c.calculateW(tweak, i, A)
			P := c.calculateP(i, W, A)
			m := c.calculateModulus(v)
			
			// FF3 decryption: NUM_radix(REV(B)) = (c - NUM(S)) mod radix^v
			reversedB := c.reverseDigits(B)
			bNum := c.numArrayToBigInt(reversedB)
			
			Y := new(big.Int).Sub(bNum, P)
			Y.Mod(Y, m)
			
			// B = REV(STR_radix(result))
			newDigits := c.bigIntToNumArray(Y, v)
			B = c.reverseDigits(newDigits)
		}
	}

	// Combine final result
	result := make([]int, n)
	copy(result, A)
	copy(result[u:], B)
	return result
}

// calculateW calculates the W parameter for FF3 round function
func (c *FF3Cipher) calculateW(tweak []byte, round int, B []int) []byte {
	// NIST FF3 W calculation: split 8-byte tweak into Tl (first 4 bytes) and Tr (last 4 bytes)
	W := make([]byte, 4)
	
	if round%2 == 0 {
		// Even rounds: W = Tr (rightmost 4 bytes)
		copy(W, tweak[4:8])
	} else {
		// Odd rounds: W = Tl (leftmost 4 bytes)
		copy(W, tweak[:4])
	}
	
	return W
}

// calculateP calculates the P parameter using FF3 specification with byte reversal
func (c *FF3Cipher) calculateP(round int, W []byte, B []int) *big.Int {
	// NIST FF3 P calculation with proper byte reversal
	// P = REVB(AES-ECB(K, REVB(W XOR [i]_4 || NUM_radix(REV(B)))))
	
	input := make([]byte, 16)
	
	// First 4 bytes: W XOR with round number in the last byte of W
	copy(input[:4], W)
	input[3] ^= byte(round)
	
	// Last 12 bytes: NUM_radix(REV(B)) - reverse digits of B first
	reversedB := c.reverseDigits(B)
	bBigInt := c.numArrayToBigInt(reversedB)
	bBytes := bBigInt.Bytes()
	
	// Pad to 12 bytes (big-endian)
	bPadded := make([]byte, 12)
	if len(bBytes) > 0 {
		copy(bPadded[12-len(bBytes):], bBytes)
	}
	copy(input[4:], bPadded)
	
	// Apply FF3 byte reversal convention: REVB before AES
	reversedInput := c.reverseBytes(input)
	
	// Encrypt with AES
	aesOutput := make([]byte, 16)
	c.aes.Encrypt(aesOutput, reversedInput)
	
	// Apply FF3 byte reversal convention: REVB after AES
	output := c.reverseBytes(aesOutput)
	
	// Convert to big integer
	P := new(big.Int).SetBytes(output)
	return P
}

// calculateModulus calculates radix^length for modular arithmetic
func (c *FF3Cipher) calculateModulus(length int) *big.Int {
	// Calculate radix^length
	radixBig := big.NewInt(int64(c.radix))
	lengthBig := big.NewInt(int64(length))
	
	result := new(big.Int).Exp(radixBig, lengthBig, nil)
	return result
}

// numArrayToBigInt converts array of digits to big integer
func (c *FF3Cipher) numArrayToBigInt(nums []int) *big.Int {
	result := big.NewInt(0)
	radixBig := big.NewInt(int64(c.radix))
	
	for _, num := range nums {
		result.Mul(result, radixBig)
		result.Add(result, big.NewInt(int64(num)))
	}
	
	return result
}

// bigIntToNumArray converts big integer to array of digits with specified length
func (c *FF3Cipher) bigIntToNumArray(num *big.Int, length int) []int {
	result := make([]int, length)
	radixBig := big.NewInt(int64(c.radix))
	temp := new(big.Int).Set(num)
	
	for i := length - 1; i >= 0; i-- {
		remainder := new(big.Int)
		temp.DivMod(temp, radixBig, remainder)
		result[i] = int(remainder.Int64())
	}
	
	return result
}

// combineTweaks combines base tweak with additional tweak for domain separation
func (c *FF3Cipher) combineTweaks(additionalTweak []byte) []byte {
	if additionalTweak == nil || len(additionalTweak) == 0 {
		return c.tweak
	}
	
	// XOR additional tweak with base tweak for variation
	combined := make([]byte, 8) // Always 8 bytes for FF3
	copy(combined, c.tweak)
	
	// XOR with additional tweak bytes
	for i := 0; i < len(additionalTweak) && i < 8; i++ {
		combined[i] ^= additionalTweak[i]
	}
	
	return combined
}

// EncryptDigits runs the core FF3 over base-radix digits
func (c *FF3Cipher) EncryptDigits(nums []int, additionalTweak []byte) []int {
	return c.ff3Encrypt(nums, c.combineTweaks(additionalTweak))
}

// DecryptDigits runs the core FF3 decryption over base-radix digits
func (c *FF3Cipher) DecryptDigits(nums []int, additionalTweak []byte) []int {
	return c.ff3Decrypt(nums, c.combineTweaks(additionalTweak))
}

// GetMinLen returns the minimum length constraint
func (c *FF3Cipher) GetMinLen() int {
	return c.minLen
}

// GetMaxLen returns the maximum length constraint
func (c *FF3Cipher) GetMaxLen() int {
	return c.maxLen
}

// createAESCipher creates AES cipher with FF3 byte-reversal convention
func createAESCipher(key []byte) (cipher.Block, error) {
	// Reverse key bytes for FF3 byte-reversal convention
	reversedKey := make([]byte, len(key))
	for i := 0; i < len(key); i++ {
		reversedKey[i] = key[len(key)-1-i]
	}

	// Create AES cipher with reversed key
	return aes.NewCipher(reversedKey)
}

// Helper functions for FF3 digit and byte reversal

// reverseDigits reverses an array of digits
func (c *FF3Cipher) reverseDigits(digits []int) []int {
	reversed := make([]int, len(digits))
	for i := 0; i < len(digits); i++ {
		reversed[i] = digits[len(digits)-1-i]
	}
	return reversed
}

// reverseBytes reverses an array of bytes
func (c *FF3Cipher) reverseBytes(bytes []byte) []byte {
	reversed := make([]byte, len(bytes))
	for i := 0; i < len(bytes); i++ {
		reversed[i] = bytes[len(bytes)-1-i]
	}
	return reversed
}