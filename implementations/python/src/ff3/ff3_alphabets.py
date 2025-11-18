"""
FF3 Alphabets - Canonical alphabets for cross-language compatibility.
"""

# Canonical alphabets used by test vectors (order is part of the spec!)
ALPHA_DIGITS = "0123456789"                                                 # radix 10
ALPHA_HEX_LOWER = "0123456789abcdef"                                          # radix 16 lowercase
ALPHA_HEX_UPPER = "0123456789ABCDEF"                                          # radix 16 uppercase
ALPHA_BASE36_LOW = "0123456789abcdefghijklmnopqrstuvwxyz"                      # radix 36 lowercase
ALPHA_BASE36_UP = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"                      # radix 36 uppercase
ALPHA_BASE62 = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" # radix 62


class AlphabetSpec:
    """Alphabet specification - NO NORMALIZATION, pure format preservation."""

    def __init__(self, charset: str):
        """
        Create an alphabet specification for use with FF3 ciphers.
        Stores the character set exactly as provided with no normalization or validation.
        The charset defines the valid characters and their ordering for digit conversion.
        Order matters: position in charset determines the numeric value for each character.
        """
        self.charset = charset


# Built-in specs - alphabet defines what's valid, preserve exactly what user inputs
SPEC_DIGITS = AlphabetSpec(ALPHA_DIGITS)
SPEC_HEX_LOWER = AlphabetSpec(ALPHA_HEX_LOWER)
SPEC_HEX_UPPER = AlphabetSpec(ALPHA_HEX_UPPER)
SPEC_BASE36_LOW = AlphabetSpec(ALPHA_BASE36_LOW)
SPEC_BASE36_UP = AlphabetSpec(ALPHA_BASE36_UP)
SPEC_BASE62 = AlphabetSpec(ALPHA_BASE62)