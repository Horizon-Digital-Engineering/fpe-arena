"""
Unit tests for ff3_alphabets.py - Alphabet specifications and constants.
"""

import pytest
import sys
import os

# Add parent directory to path for imports
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))

from src.ff3.ff3_alphabets import (
    AlphabetSpec,
    ALPHA_DIGITS,
    ALPHA_HEX_LOWER,
    ALPHA_HEX_UPPER,
    ALPHA_BASE36_LOW,
    ALPHA_BASE36_UP,
    ALPHA_BASE62,
    SPEC_DIGITS,
    SPEC_HEX_LOWER,
    SPEC_HEX_UPPER,
    SPEC_BASE36_LOW,
    SPEC_BASE36_UP,
    SPEC_BASE62,
)


class TestAlphabetSpec:
    """Test cases for AlphabetSpec class."""

    def test_alphabet_spec_basic_creation(self):
        """Test basic AlphabetSpec creation."""
        charset = "0123456789"
        spec = AlphabetSpec(charset)

        assert spec.charset == charset
        # No radix property in this implementation

    def test_alphabet_spec_charset_length(self):
        """Test that charset length is preserved correctly."""
        test_cases = [
            "01",
            "0123456789",
            "0123456789abcdef",
            "0123456789abcdefghijklmnopqrstuvwxyz",
            "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz",
        ]

        for charset in test_cases:
            spec = AlphabetSpec(charset)
            assert len(spec.charset) == len(charset), f"Failed for charset of length {len(charset)}"

    def test_alphabet_spec_custom_charset(self):
        """Test AlphabetSpec with custom charset."""
        custom_charset = "ABCDEFGH"
        spec = AlphabetSpec(custom_charset)

        assert spec.charset == custom_charset

    def test_alphabet_spec_empty_charset(self):
        """Test AlphabetSpec with empty charset (allowed in this implementation)."""
        spec = AlphabetSpec("")
        assert spec.charset == ""

    def test_alphabet_spec_with_duplicates(self):
        """Test AlphabetSpec with duplicate characters (allowed, no validation)."""
        spec = AlphabetSpec("0123456789a9")  # '9' appears twice
        assert spec.charset == "0123456789a9"

    def test_alphabet_spec_single_character(self):
        """Test AlphabetSpec with single character (allowed in this implementation)."""
        spec = AlphabetSpec("0")
        assert spec.charset == "0"


class TestAlphabetConstants:
    """Test cases for predefined alphabet constants."""

    def test_alpha_digits_constant(self):
        """Test ALPHA_DIGITS constant."""
        assert ALPHA_DIGITS == "0123456789"
        assert len(ALPHA_DIGITS) == 10

    def test_alpha_hex_lower_constant(self):
        """Test ALPHA_HEX_LOWER constant."""
        assert ALPHA_HEX_LOWER == "0123456789abcdef"
        assert len(ALPHA_HEX_LOWER) == 16

    def test_alpha_hex_upper_constant(self):
        """Test ALPHA_HEX_UPPER constant."""
        assert ALPHA_HEX_UPPER == "0123456789ABCDEF"
        assert len(ALPHA_HEX_UPPER) == 16

    def test_alpha_base36_low_constant(self):
        """Test ALPHA_BASE36_LOW constant."""
        expected = "0123456789abcdefghijklmnopqrstuvwxyz"
        assert ALPHA_BASE36_LOW == expected
        assert len(ALPHA_BASE36_LOW) == 36

    def test_alpha_base36_up_constant(self):
        """Test ALPHA_BASE36_UP constant."""
        expected = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        assert ALPHA_BASE36_UP == expected
        assert len(ALPHA_BASE36_UP) == 36

    def test_alpha_base62_constant(self):
        """Test ALPHA_BASE62 constant."""
        expected = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
        assert ALPHA_BASE62 == expected
        assert len(ALPHA_BASE62) == 62

    def test_alphabet_constants_no_duplicates(self):
        """Test that all alphabet constants have no duplicate characters."""
        alphabets = [
            ALPHA_DIGITS,
            ALPHA_HEX_LOWER,
            ALPHA_HEX_UPPER,
            ALPHA_BASE36_LOW,
            ALPHA_BASE36_UP,
            ALPHA_BASE62,
        ]

        for alphabet in alphabets:
            assert len(set(alphabet)) == len(alphabet), f"Duplicates found in {alphabet}"

    def test_alphabet_constants_correct_content(self):
        """Test that alphabet constants contain expected characters."""
        # Test digits
        for char in ALPHA_DIGITS:
            assert char.isdigit()

        # Test hex lower has digits + a-f
        hex_chars = set("0123456789abcdef")
        assert set(ALPHA_HEX_LOWER) == hex_chars

        # Test hex upper has digits + A-F
        hex_upper_chars = set("0123456789ABCDEF")
        assert set(ALPHA_HEX_UPPER) == hex_upper_chars

        # Test base36 lower has digits + a-z
        base36_chars = set("0123456789abcdefghijklmnopqrstuvwxyz")
        assert set(ALPHA_BASE36_LOW) == base36_chars

        # Test base36 upper has digits + A-Z
        base36_upper_chars = set("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")
        assert set(ALPHA_BASE36_UP) == base36_upper_chars

        # Test base62 has digits + A-Z + a-z
        base62_chars = set("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
        assert set(ALPHA_BASE62) == base62_chars


class TestAlphabetSpecConstants:
    """Test cases for predefined AlphabetSpec constants."""

    def test_spec_digits_constant(self):
        """Test SPEC_DIGITS constant."""
        assert isinstance(SPEC_DIGITS, AlphabetSpec)
        assert SPEC_DIGITS.charset == ALPHA_DIGITS
        # No radix property - just check charset

    def test_spec_hex_lower_constant(self):
        """Test SPEC_HEX_LOWER constant."""
        assert isinstance(SPEC_HEX_LOWER, AlphabetSpec)
        assert SPEC_HEX_LOWER.charset == ALPHA_HEX_LOWER
        # No radix property - just check charset

    def test_spec_hex_upper_constant(self):
        """Test SPEC_HEX_UPPER constant."""
        assert isinstance(SPEC_HEX_UPPER, AlphabetSpec)
        assert SPEC_HEX_UPPER.charset == ALPHA_HEX_UPPER
        # No radix property - just check charset

    def test_spec_base36_low_constant(self):
        """Test SPEC_BASE36_LOW constant."""
        assert isinstance(SPEC_BASE36_LOW, AlphabetSpec)
        assert SPEC_BASE36_LOW.charset == ALPHA_BASE36_LOW
        # No radix property - just check charset

    def test_spec_base36_up_constant(self):
        """Test SPEC_BASE36_UP constant."""
        assert isinstance(SPEC_BASE36_UP, AlphabetSpec)
        assert SPEC_BASE36_UP.charset == ALPHA_BASE36_UP
        # No radix property - just check charset

    def test_spec_base62_constant(self):
        """Test SPEC_BASE62 constant."""
        assert isinstance(SPEC_BASE62, AlphabetSpec)
        assert SPEC_BASE62.charset == ALPHA_BASE62
        # No radix property - just check charset

    def test_all_spec_constants_valid(self):
        """Test that all predefined spec constants are valid."""
        specs = [
            SPEC_DIGITS,
            SPEC_HEX_LOWER,
            SPEC_HEX_UPPER,
            SPEC_BASE36_LOW,
            SPEC_BASE36_UP,
            SPEC_BASE62,
        ]

        for spec in specs:
            assert isinstance(spec, AlphabetSpec)
            assert len(spec.charset) >= 2  # Should have at least 2 characters
            assert len(spec.charset) <= 62  # Should not exceed 62 characters

    def test_alphabet_subset_relationships(self):
        """Test subset relationships between alphabets."""
        # Digits should be subset of all others
        digits_set = set(ALPHA_DIGITS)

        assert digits_set.issubset(set(ALPHA_HEX_LOWER))
        assert digits_set.issubset(set(ALPHA_HEX_UPPER))
        assert digits_set.issubset(set(ALPHA_BASE36_LOW))
        assert digits_set.issubset(set(ALPHA_BASE36_UP))
        assert digits_set.issubset(set(ALPHA_BASE62))

        # Hex should be subset of base36 and base62
        hex_lower_set = set(ALPHA_HEX_LOWER)
        assert hex_lower_set.issubset(set(ALPHA_BASE36_LOW))
        assert hex_lower_set.issubset(set(ALPHA_BASE62))

        # Base36 should be subset of base62
        base36_lower_set = set(ALPHA_BASE36_LOW)
        assert base36_lower_set.issubset(set(ALPHA_BASE62))

    def test_case_sensitivity_in_alphabets(self):
        """Test case sensitivity between upper and lower case alphabets."""
        # Hex upper and lower should be different
        assert set(ALPHA_HEX_LOWER) != set(ALPHA_HEX_UPPER)

        # Base36 upper and lower should be different
        assert set(ALPHA_BASE36_LOW) != set(ALPHA_BASE36_UP)

        # But digits should be in both
        digits_set = set(ALPHA_DIGITS)
        assert digits_set.issubset(set(ALPHA_HEX_LOWER))
        assert digits_set.issubset(set(ALPHA_HEX_UPPER))