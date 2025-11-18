"""
FF3 API - Public interface with string ↔ digits conversion.
"""

from typing import Optional, Dict, List
try:
    from .ff3_core import FF3Cipher
    from .ff3_alphabets import (
        AlphabetSpec,
        SPEC_DIGITS, SPEC_HEX_LOWER, SPEC_HEX_UPPER,
        SPEC_BASE36_LOW, SPEC_BASE36_UP, SPEC_BASE62,
        ALPHA_BASE36_LOW
    )
except ImportError:
    from ff3_core import FF3Cipher
    from ff3_alphabets import (
        AlphabetSpec,
        SPEC_DIGITS, SPEC_HEX_LOWER, SPEC_HEX_UPPER,
        SPEC_BASE36_LOW, SPEC_BASE36_UP, SPEC_BASE62,
        ALPHA_BASE36_LOW
    )


class FF3:
    """Lightweight cipher with alphabet-specific string conversion."""
    
    def __init__(self, core: FF3Cipher, spec: AlphabetSpec):
        """
        Initialize a lightweight cipher wrapper that handles string-to-digits conversion.
        Combines the core FF3 cryptographic engine with alphabet-specific character mapping.
        Creates efficient lookup tables for fast character-to-index and index-to-character conversion.
        Provides a clean API for encrypting/decrypting strings while preserving exact format.
        """
        self.core = core
        self.spec = spec
        self.alpha = list(spec.charset)
        self.index = {char: i for i, char in enumerate(self.alpha)}
    
    def _to_digits(self, s: str) -> List[int]:
        """
        Convert input string to array of integer digits using the cipher's alphabet.
        Each character is mapped to its position in the alphabet (0-based indexing).
        Validates that all characters exist in the defined alphabet.
        Returns digit array suitable for FF3 core cryptographic operations.
        """
        digits = []
        for i, char in enumerate(s):
            if char not in self.index:
                raise ValueError(f"invalid char '{char}' at pos {i} for this alphabet")
            digits.append(self.index[char])
        return digits
    
    def _from_digits(self, digits: List[int]) -> str:
        """
        Convert array of integer digits back to string using the cipher's alphabet.
        Each digit index is mapped to its corresponding character in the alphabet.
        Validates that all digit values are within the valid range for the alphabet.
        Returns the reconstructed string preserving exact format and character set.
        """
        chars = []
        for digit in digits:
            if digit < 0 or digit >= len(self.alpha):
                raise ValueError(f"digit {digit} out of range for radix {len(self.alpha)}")
            chars.append(self.alpha[digit])
        return ''.join(chars)
    
    def encrypt(self, plaintext: str, additional_tweak: Optional[bytes] = None) -> str:
        """
        Encrypt a plaintext string using FF3 while preserving exact format and character set.
        Converts string to digits, applies FF3 encryption, then converts back to string.
        Optional additional_tweak allows context-specific encryption for domain separation.
        Output has same length and uses same alphabet as input (format-preserving property).
        """
        digits = self._to_digits(plaintext)
        n = len(digits)
        
        if n < self.core.get_min_len() or n > self.core.get_max_len():
            raise ValueError(f"length {n} out of bounds [{self.core.get_min_len()},{self.core.get_max_len()}]")
        
        encrypted_digits = self.core.encrypt_digits(digits, additional_tweak)
        return self._from_digits(encrypted_digits)
    
    def decrypt(self, ciphertext: str, additional_tweak: Optional[bytes] = None) -> str:
        """
        Decrypt a ciphertext string using FF3 to recover the original plaintext.
        Converts string to digits, applies FF3 decryption, then converts back to string.
        Must use the same additional_tweak (if any) that was used during encryption.
        Returns original plaintext with exact same format and character set as before encryption.
        """
        digits = self._to_digits(ciphertext)
        n = len(digits)
        
        if n < self.core.get_min_len() or n > self.core.get_max_len():
            raise ValueError(f"length {n} out of bounds [{self.core.get_min_len()},{self.core.get_max_len()}]")
        
        decrypted_digits = self.core.decrypt_digits(digits, additional_tweak)
        return self._from_digits(decrypted_digits)


# Single entrypoint
def from_spec(key: bytes, tweak: bytes, spec: AlphabetSpec) -> FF3:
    """
    Create a new FF3 cipher instance from a custom alphabet specification.
    Validates the alphabet has sufficient characters (>=2) for meaningful encryption.
    Constructs the core FF3 cipher with the alphabet's radix and wraps it in CipherLite.
    This is the most flexible constructor allowing any custom character set.
    """
    if len(spec.charset) < 2:
        raise ValueError("alphabet must have >=2 chars")
    
    radix = len(spec.charset)
    core = FF3Cipher(radix, key, tweak)
    return FF3(core, spec)


# Convenience helpers for standard alphabets
def digits(key: bytes, tweak: bytes) -> FF3:
    """Create cipher for decimal digits (0-9, radix 10)."""
    return from_spec(key, tweak, SPEC_DIGITS)


def hex_lower(key: bytes, tweak: bytes) -> FF3:
    """Create cipher for lowercase hex (0-9a-f)."""
    return from_spec(key, tweak, SPEC_HEX_LOWER)


def hex_upper(key: bytes, tweak: bytes) -> FF3:
    """Create cipher for uppercase hex (0-9A-F)."""
    return from_spec(key, tweak, SPEC_HEX_UPPER)


def base36_lower(key: bytes, tweak: bytes) -> FF3:
    """Create cipher for lowercase base36 (0-9a-z)."""
    return from_spec(key, tweak, SPEC_BASE36_LOW)


def base36_upper(key: bytes, tweak: bytes) -> FF3:
    """Create cipher for uppercase base36 (0-9A-Z)."""
    return from_spec(key, tweak, SPEC_BASE36_UP)


def base62(key: bytes, tweak: bytes) -> FF3:
    """Create cipher for base62 (0-9A-Za-z, radix 62)."""
    return from_spec(key, tweak, SPEC_BASE62)


# Backward compatibility aliases
def hex(key: bytes, tweak: bytes) -> FF3:
    """Create cipher for hex (lowercase, for compatibility)."""
    return hex_lower(key, tweak)


def base36(key: bytes, tweak: bytes) -> FF3:
    """Create cipher for base36 (lowercase, for compatibility)."""
    return base36_lower(key, tweak)