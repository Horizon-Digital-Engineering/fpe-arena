package ff3

import (
	"fmt"
)

type FF3 struct {
	core  *FF3Cipher
	spec  AlphabetSpec
	alpha []rune
	index map[rune]int
}

// FromSpec creates a new FF3 cipher from an alphabet specification.
func FromSpec(key, tweak []byte, spec AlphabetSpec) (*FF3, error) {
	alphaRunes := []rune(spec.Charset)
	if len(alphaRunes) < 2 {
		return nil, fmt.Errorf("alphabet must have >=2 chars")
	}
	radix := len(alphaRunes)

	// Allow radix up to 62 for this experimental repo.
	core, err := NewFF3Cipher(radix, key, tweak)
	if err != nil {
		return nil, err
	}

	idx := make(map[rune]int, radix)
	for i, r := range alphaRunes {
		idx[r] = i
	}

	return &FF3{core: core, spec: spec, alpha: alphaRunes, index: idx}, nil
}

// Convenience helpers for standard alphabets:
func Digits(key, tweak []byte) (*FF3, error) {
	return FromSpec(key, tweak, SpecDigits)
}
func HexLower(key, tweak []byte) (*FF3, error) {
	return FromSpec(key, tweak, SpecHexLower)
}
func HexUpper(key, tweak []byte) (*FF3, error) {
	return FromSpec(key, tweak, SpecHexUpper)
}
func Base36Lower(key, tweak []byte) (*FF3, error) {
	return FromSpec(key, tweak, SpecBase36Low)
}
func Base36Upper(key, tweak []byte) (*FF3, error) {
	return FromSpec(key, tweak, SpecBase36Up)
}
func Base62(key, tweak []byte) (*FF3, error) {
	return FromSpec(key, tweak, SpecBase62)
}

// Backward compatibility aliases
func Hex(key, tweak []byte) (*FF3, error) {
	return HexLower(key, tweak)
}
func Base36(key, tweak []byte) (*FF3, error) {
	return Base36Lower(key, tweak)
}

// ---- string <-> digits ----

func (c *FF3) toDigits(s string) ([]int, error) {
	rs := []rune(s)
	out := make([]int, len(rs))
	for i, r := range rs {
		v, ok := c.index[r]
		if !ok {
			return nil, fmt.Errorf("invalid char %q at pos %d for this alphabet", r, i)
		}
		out[i] = v
	}
	return out, nil
}

func (c *FF3) fromDigits(d []int) (string, error) {
	out := make([]rune, len(d))
	for i, v := range d {
		if v < 0 || v >= len(c.alpha) {
			return "", fmt.Errorf("digit %d out of range for radix %d", v, len(c.alpha))
		}
		out[i] = c.alpha[v]
	}
	return string(out), nil
}

// ---- public API ----

func (c *FF3) Encrypt(plaintext string, additionalTweak []byte) (string, error) {
	nums, err := c.toDigits(plaintext)
	if err != nil {
		return "", err
	}
	n := len(nums)
	if n < c.core.GetMinLen() || n > c.core.GetMaxLen() {
		return "", fmt.Errorf("length %d out of bounds [%d,%d]", n, c.core.GetMinLen(), c.core.GetMaxLen())
	}
	out := c.core.EncryptDigits(nums, additionalTweak)
	return c.fromDigits(out)
}

func (c *FF3) Decrypt(ciphertext string, additionalTweak []byte) (string, error) {
	nums, err := c.toDigits(ciphertext)
	if err != nil {
		return "", err
	}
	n := len(nums)
	if n < c.core.GetMinLen() || n > c.core.GetMaxLen() {
		return "", fmt.Errorf("length %d out of bounds [%d,%d]", n, c.core.GetMinLen(), c.core.GetMaxLen())
	}
	out := c.core.DecryptDigits(nums, additionalTweak)
	return c.fromDigits(out)
}