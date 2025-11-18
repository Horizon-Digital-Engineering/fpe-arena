"""
Unit tests for ff3_core.py - Low-level FF3 cipher operations.
"""

import pytest
import sys
import os

# Add parent directory to path for imports
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))

from src.ff3.ff3_core import FF3Cipher


class TestFF3Core:
    """Test cases for FF3Cipher core functionality."""

    def setup_method(self):
        """Set up test fixtures."""
        self.key = bytes.fromhex("EF4359D8D580AA4F7F036D6F04FC6A94")
        self.tweak = bytes.fromhex("D8E7920AFA330A73")

    def test_constructor_valid_inputs(self):
        """Test FF3Cipher constructor with valid inputs."""
        cipher = FF3Cipher(10, self.key, self.tweak)
        assert cipher is not None
        assert cipher.radix == 10

    def test_constructor_invalid_key_length(self):
        """Test FF3Cipher constructor with invalid key length."""
        short_key = bytes.fromhex("EF4359D8")  # Too short

        with pytest.raises(ValueError, match="key length must be"):
            FF3Cipher(10, short_key, self.tweak)

    def test_constructor_invalid_tweak_length(self):
        """Test FF3Cipher constructor with invalid tweak length."""
        short_tweak = bytes.fromhex("D8E7")  # Too short

        with pytest.raises(ValueError, match="tweak must be exactly"):
            FF3Cipher(10, self.key, short_tweak)

    def test_constructor_invalid_radix_too_small(self):
        """Test FF3Cipher constructor with radix too small."""
        with pytest.raises(ValueError, match="radix must be"):
            FF3Cipher(1, self.key, self.tweak)

    def test_constructor_invalid_radix_too_large(self):
        """Test FF3Cipher constructor with radix too large."""
        with pytest.raises(ValueError, match="radix must be"):
            FF3Cipher(100, self.key, self.tweak)

    def test_encrypt_digits_basic_functionality(self):
        """Test basic encrypt_digits functionality."""
        cipher = FF3Cipher(10, self.key, self.tweak)
        plaintext_digits = [1, 2, 3, 4, 5, 6, 7, 8, 9, 0]

        ciphertext_digits = cipher.encrypt_digits(plaintext_digits)

        # Verify it's different from plaintext
        assert ciphertext_digits != plaintext_digits
        # Verify same length (format preservation)
        assert len(ciphertext_digits) == len(plaintext_digits)
        # Verify all digits are valid for radix 10
        for digit in ciphertext_digits:
            assert 0 <= digit < 10

    def test_decrypt_digits_basic_functionality(self):
        """Test basic decrypt_digits functionality."""
        cipher = FF3Cipher(10, self.key, self.tweak)
        plaintext_digits = [1, 2, 3, 4, 5, 6, 7, 8, 9, 0]

        ciphertext_digits = cipher.encrypt_digits(plaintext_digits)
        decrypted_digits = cipher.decrypt_digits(ciphertext_digits)

        assert decrypted_digits == plaintext_digits

    def test_roundtrip_various_lengths(self):
        """Test encrypt/decrypt roundtrip for various digit array lengths."""
        cipher = FF3Cipher(10, self.key, self.tweak)

        test_cases = [
            [1, 2, 3, 4, 5, 6],                    # Minimum length
            [1, 2, 3, 4, 5, 6, 7],                # 7 digits
            [1, 2, 3, 4, 5, 6, 7, 8],            # 8 digits
            [1, 2, 3, 4, 5, 6, 7, 8, 9],         # 9 digits
            [1, 2, 3, 4, 5, 6, 7, 8, 9, 0],      # 10 digits
            [1, 2, 3, 4, 5] * 3,                   # 15 digits
            [1, 2, 3, 4, 5] * 4,                   # 20 digits
        ]

        for plaintext_digits in test_cases:
            ciphertext_digits = cipher.encrypt_digits(plaintext_digits)
            decrypted_digits = cipher.decrypt_digits(ciphertext_digits)

            assert decrypted_digits == plaintext_digits, f"Roundtrip failed for length {len(plaintext_digits)}"
            assert len(ciphertext_digits) == len(plaintext_digits), f"Length not preserved for {plaintext_digits}"

    def test_different_key_sizes(self):
        """Test FF3Cipher with different AES key sizes."""
        test_keys = [
            bytes.fromhex("EF4359D8D580AA4F7F036D6F04FC6A94"),  # 128-bit (16 bytes)
            bytes.fromhex("EF4359D8D580AA4F7F036D6F04FC6A942B7E151628AED2A6"),    # 192-bit (24 bytes)
            bytes.fromhex("EF4359D8D580AA4F7F036D6F04FC6A942B7E151628AED2A6ABF7158809CF4F3C"),  # 256-bit (32 bytes)
        ]

        plaintext_digits = [1, 2, 3, 4, 5, 6, 7, 8, 9, 0]

        for key in test_keys:
            cipher = FF3Cipher(10, key, self.tweak)
            ciphertext_digits = cipher.encrypt_digits(plaintext_digits)
            decrypted_digits = cipher.decrypt_digits(ciphertext_digits)

            assert decrypted_digits == plaintext_digits, f"Roundtrip failed for key size {len(key)*8} bits"

    def test_edge_case_minimum_length(self):
        """Test edge case with minimum allowed digit array length."""
        cipher = FF3Cipher(10, self.key, self.tweak)
        plaintext_digits = [1, 2, 3, 4, 5, 6]  # Minimum length

        ciphertext_digits = cipher.encrypt_digits(plaintext_digits)
        decrypted_digits = cipher.decrypt_digits(ciphertext_digits)

        assert decrypted_digits == plaintext_digits
        assert len(ciphertext_digits) == 6

    def test_edge_case_repeated_digits(self):
        """Test edge case with repeated digits."""
        cipher = FF3Cipher(10, self.key, self.tweak)
        plaintext_digits = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1]  # All same digit

        ciphertext_digits = cipher.encrypt_digits(plaintext_digits)
        decrypted_digits = cipher.decrypt_digits(ciphertext_digits)

        assert decrypted_digits == plaintext_digits
        # Encryption should change repeated digits
        assert ciphertext_digits != plaintext_digits

    def test_length_constraints(self):
        """Test length constraint methods."""
        cipher = FF3Cipher(10, self.key, self.tweak)

        min_len = cipher.get_min_len()
        max_len = cipher.get_max_len()

        assert min_len >= 2  # FF3 minimum
        assert max_len <= 32  # FF3 maximum for radix 10
        assert min_len <= max_len

    def test_different_radix_values(self):
        """Test FF3Cipher with different radix values."""
        test_cases = [
            (2, [1, 0, 1, 0, 1, 0, 1, 0]),           # Binary
            (16, [13, 14, 10, 13, 11, 14, 14, 15]),  # Hex (d=13, e=14, a=10, f=15)
            (36, [7, 4, 11, 11, 24, 1, 2, 3]),       # Base36 (h=17->7 mod 36, etc.)
        ]

        for radix, plaintext_digits in test_cases:
            # Ensure all digits are valid for the radix
            valid_digits = [d % radix for d in plaintext_digits]

            cipher = FF3Cipher(radix, self.key, self.tweak)
            ciphertext_digits = cipher.encrypt_digits(valid_digits)
            decrypted_digits = cipher.decrypt_digits(ciphertext_digits)

            assert decrypted_digits == valid_digits, f"Roundtrip failed for radix {radix}"
            # Verify all output digits are valid for the radix
            for digit in ciphertext_digits:
                assert 0 <= digit < radix, f"Invalid digit {digit} for radix {radix}"