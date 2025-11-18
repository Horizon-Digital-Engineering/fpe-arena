package ff3

// Canonical alphabets used by test vectors (order is part of the spec!)
const (
	AlphaDigits    = "0123456789"                                                 // radix 10
	AlphaHexLower  = "0123456789abcdef"                                          // radix 16 lowercase
	AlphaHexUpper  = "0123456789ABCDEF"                                          // radix 16 uppercase
	AlphaBase36Low = "0123456789abcdefghijklmnopqrstuvwxyz"                      // radix 36 lowercase
	AlphaBase36Up  = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"                      // radix 36 uppercase
	AlphaBase62    = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" // radix 62 (numbers, UPPER, lower)
)

// AlphabetSpec defines a charset - NO NORMALIZATION, pure format preservation
type AlphabetSpec struct {
	Charset string
}

// Built-in specs - alphabet defines what's valid, preserve exactly what user inputs
var (
	SpecDigits    = AlphabetSpec{Charset: AlphaDigits}
	SpecHexLower  = AlphabetSpec{Charset: AlphaHexLower}
	SpecHexUpper  = AlphabetSpec{Charset: AlphaHexUpper}
	SpecBase36Low = AlphabetSpec{Charset: AlphaBase36Low}
	SpecBase36Up  = AlphabetSpec{Charset: AlphaBase36Up}
	SpecBase62    = AlphabetSpec{Charset: AlphaBase62}
)