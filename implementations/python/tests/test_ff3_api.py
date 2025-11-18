"""
Unit tests for ff3_api.py - High-level string interface and factory functions.
"""

import pytest
import sys
import os

# Add parent directory to path for imports
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))

from src.ff3.ff3_api import (
    FF3,
    digits,
    hex_lower,
    hex_upper,
    base36_lower,
    base36_upper,
    base62,
    from_spec,
)
from src.ff3.ff3_alphabets import AlphabetSpec


class TestFF3API:
    """Test cases for FF3 API and factory functions."""

    def setup_method(self):
        """Set up test fixtures."""
        self.key = bytes.fromhex("EF4359D8D580AA4F7F036D6F04FC6A94")
        self.tweak = bytes.fromhex("D8E7920AFA330A73")

    def test_digits_factory(self):
        """Test digits() factory function."""
        cipher = digits(self.key, self.tweak)
        assert isinstance(cipher, FF3)

        # Test with digit string
        plaintext = "1234567890"
        ciphertext = cipher.encrypt(plaintext)
        decrypted = cipher.decrypt(ciphertext)

        assert decrypted == plaintext
        assert len(ciphertext) == len(plaintext)
        # All characters should be digits
        for char in ciphertext:
            assert char in "0123456789"

    def test_hex_lower_factory(self):
        """Test hex_lower() factory function."""
        cipher = hex_lower(self.key, self.tweak)
        assert isinstance(cipher, FF3)

        # Test with hex string
        plaintext = "deadbeef123456"
        ciphertext = cipher.encrypt(plaintext)
        decrypted = cipher.decrypt(ciphertext)

        assert decrypted == plaintext
        # All characters should be hex lowercase
        for char in ciphertext:
            assert char in "0123456789abcdef"

    def test_hex_upper_factory(self):
        """Test hex_upper() factory function."""
        cipher = hex_upper(self.key, self.tweak)
        assert isinstance(cipher, FF3)

        # Test with hex string
        plaintext = "DEADBEEF123456"
        ciphertext = cipher.encrypt(plaintext)
        decrypted = cipher.decrypt(ciphertext)

        assert decrypted == plaintext
        # All characters should be hex uppercase
        for char in ciphertext:
            assert char in "0123456789ABCDEF"

    def test_base36_lower_factory(self):
        """Test base36_lower() factory function."""
        cipher = base36_lower(self.key, self.tweak)
        assert isinstance(cipher, FF3)

        # Test with base36 string
        plaintext = "hello123world"
        ciphertext = cipher.encrypt(plaintext)
        decrypted = cipher.decrypt(ciphertext)

        assert decrypted == plaintext
        # All characters should be base36 lowercase
        for char in ciphertext:
            assert char in "0123456789abcdefghijklmnopqrstuvwxyz"

    def test_base36_upper_factory(self):
        """Test base36_upper() factory function."""
        cipher = base36_upper(self.key, self.tweak)
        assert isinstance(cipher, FF3)

        # Test with base36 string
        plaintext = "HELLO123WORLD"
        ciphertext = cipher.encrypt(plaintext)
        decrypted = cipher.decrypt(ciphertext)

        assert decrypted == plaintext
        # All characters should be base36 uppercase
        for char in ciphertext:
            assert char in "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"

    def test_base62_factory(self):
        """Test base62() factory function."""
        cipher = base62(self.key, self.tweak)
        assert isinstance(cipher, FF3)

        # Test with base62 string
        plaintext = "Hello123World"
        ciphertext = cipher.encrypt(plaintext)
        decrypted = cipher.decrypt(ciphertext)

        assert decrypted == plaintext
        # All characters should be base62
        for char in ciphertext:
            assert char in "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

    def test_from_spec_custom_alphabet(self):
        """Test from_spec() with custom alphabet."""
        custom_spec = AlphabetSpec("ABCDEFGH")
        cipher = from_spec(self.key, self.tweak, custom_spec)
        assert isinstance(cipher, FF3)

        # Test with custom alphabet string
        plaintext = "ABCDEFGH"
        ciphertext = cipher.encrypt(plaintext)
        decrypted = cipher.decrypt(ciphertext)

        assert decrypted == plaintext
        # All characters should be from custom alphabet
        for char in ciphertext:
            assert char in "ABCDEFGH"

    def test_factory_functions_return_different_ciphers(self):
        """Test that different factory functions return ciphers with different alphabets."""
        digits_cipher = digits(self.key, self.tweak)
        hex_cipher = hex_lower(self.key, self.tweak)
        base62_cipher = base62(self.key, self.tweak)

        # Same plaintext should encrypt differently with different alphabets
        # (when the alphabets don't overlap or have different sizes)
        plaintext = "123456"

        digits_result = digits_cipher.encrypt(plaintext)
        hex_result = hex_cipher.encrypt(plaintext)

        # Results should be different due to different radix
        assert digits_result != hex_result

    def test_ff3_class_direct_usage(self):
        """Test FF3 class can be used directly."""
        spec = AlphabetSpec("0123456789")
        from src.ff3.ff3_core import FF3Cipher
        core = FF3Cipher(10, self.key, self.tweak)
        cipher = FF3(core, spec)

        plaintext = "1234567890"
        ciphertext = cipher.encrypt(plaintext)
        decrypted = cipher.decrypt(ciphertext)

        assert decrypted == plaintext

    def test_format_preservation_across_alphabets(self):
        """Test format preservation works across different alphabets."""
        test_cases = [
            (digits, "1234567890"),
            (hex_lower, "abcdef1234"),
            (base36_lower, "hello123"),
            (base62, "Hello123"),
        ]

        for factory, plaintext in test_cases:
            cipher = factory(self.key, self.tweak)
            ciphertext = cipher.encrypt(plaintext)
            decrypted = cipher.decrypt(ciphertext)

            assert decrypted == plaintext
            assert len(ciphertext) == len(plaintext)

    def test_error_handling_invalid_input(self):
        """Test error handling for invalid input to various ciphers."""
        # Test digits cipher with non-digit input
        digits_cipher = digits(self.key, self.tweak)
        with pytest.raises(ValueError):
            digits_cipher.encrypt("12345a")

        # Test hex cipher with invalid hex input
        hex_cipher = hex_lower(self.key, self.tweak)
        with pytest.raises(ValueError):
            hex_cipher.encrypt("12345g")

    def test_roundtrip_consistency_all_factories(self):
        """Test roundtrip consistency for all factory functions."""
        factories_and_inputs = [
            (digits, "1234567890"),
            (hex_lower, "abcdef1234"),
            (hex_upper, "ABCDEF1234"),
            (base36_lower, "hello123world"),
            (base36_upper, "HELLO123WORLD"),
            (base62, "Hello123World456"),
        ]

        for factory, plaintext in factories_and_inputs:
            cipher = factory(self.key, self.tweak)

            # Multiple roundtrips should be consistent
            for _ in range(3):
                ciphertext = cipher.encrypt(plaintext)
                decrypted = cipher.decrypt(ciphertext)
                assert decrypted == plaintext

    def test_different_keys_produce_different_results(self):
        """Test that different keys produce different ciphertext."""
        key1 = bytes.fromhex("EF4359D8D580AA4F7F036D6F04FC6A94")
        key2 = bytes.fromhex("2B7E151628AED2A6ABF7158809CF4F3C")

        cipher1 = digits(key1, self.tweak)
        cipher2 = digits(key2, self.tweak)

        plaintext = "1234567890"
        result1 = cipher1.encrypt(plaintext)
        result2 = cipher2.encrypt(plaintext)

        # Different keys should produce different results
        assert result1 != result2

    def test_different_tweaks_produce_different_results(self):
        """Test that different tweaks produce different ciphertext."""
        tweak1 = bytes.fromhex("D8E7920AFA330A73")
        tweak2 = bytes.fromhex("9A768A23B1C2D3E4")

        cipher1 = digits(self.key, tweak1)
        cipher2 = digits(self.key, tweak2)

        plaintext = "1234567890"
        result1 = cipher1.encrypt(plaintext)
        result2 = cipher2.encrypt(plaintext)

        # Different tweaks should produce different results
        assert result1 != result2