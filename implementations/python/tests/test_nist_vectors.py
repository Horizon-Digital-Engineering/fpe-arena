"""
Unit tests for NIST FF3 test vector compliance.
"""

import pytest
import json
import sys
import os

# Add parent directory to path for imports
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))

from src.ff3 import (
    digits,
    hex_lower,
    base36_lower,
    base62,
    from_spec,
    AlphabetSpec,
    ALPHA_BASE36_LOW,
)


class TestNISTVectors:
    """Test cases for official NIST FF3 test vectors."""

    def setup_method(self):
        """Set up test fixtures."""
        # Try environment variable first (for CI/CD)
        test_file = os.environ.get('FF3_TEST_VECTORS_PATH')

        if not test_file:
            # Fallback to relative path for local development
            test_file = os.path.join(
                os.path.dirname(__file__),
                "../../../shared/test-vectors/nist_ff3_official_vectors.json"
            )

        if not os.path.exists(test_file):
            pytest.skip(f"NIST test vectors not found at: {test_file}")

        with open(test_file, 'r') as f:
            data = json.load(f)

        self.vectors = data['vectors']

    def get_cipher_for_radix(self, key, tweak, radix):
        """Get appropriate cipher for given radix."""
        if radix == 10:
            return digits(key, tweak)
        elif radix == 16:
            return hex_lower(key, tweak)
        elif radix == 26:
            # Use first 26 chars of base36 alphabet (0-9a-p)
            spec = AlphabetSpec(ALPHA_BASE36_LOW[:26])
            return from_spec(key, tweak, spec)
        elif radix == 36:
            return base36_lower(key, tweak)
        elif radix == 62:
            return base62(key, tweak)
        else:
            raise ValueError(f"Unsupported radix: {radix}")

    def test_all_nist_vectors_encrypt(self):
        """Test encryption against all NIST test vectors."""
        for vector in self.vectors:
            key = bytes.fromhex(vector['key'])
            tweak = bytes.fromhex(vector['tweak'])

            cipher = self.get_cipher_for_radix(key, tweak, vector['radix'])

            # Test encryption
            actual = cipher.encrypt(vector['plaintext'])
            expected = vector['ciphertext']

            assert actual == expected, (
                f"Encryption failed for sample {vector['sample']} "
                f"(radix {vector['radix']}): "
                f"expected {expected}, got {actual}"
            )

    def test_all_nist_vectors_decrypt(self):
        """Test decryption against all NIST test vectors."""
        for vector in self.vectors:
            key = bytes.fromhex(vector['key'])
            tweak = bytes.fromhex(vector['tweak'])

            cipher = self.get_cipher_for_radix(key, tweak, vector['radix'])

            # Test decryption
            actual = cipher.decrypt(vector['ciphertext'])
            expected = vector['plaintext']

            assert actual == expected, (
                f"Decryption failed for sample {vector['sample']} "
                f"(radix {vector['radix']}): "
                f"expected {expected}, got {actual}"
            )

    def test_all_nist_vectors_roundtrip(self):
        """Test roundtrip (encrypt then decrypt) for all NIST test vectors."""
        for vector in self.vectors:
            key = bytes.fromhex(vector['key'])
            tweak = bytes.fromhex(vector['tweak'])

            cipher = self.get_cipher_for_radix(key, tweak, vector['radix'])

            # Test roundtrip
            plaintext = vector['plaintext']
            encrypted = cipher.encrypt(plaintext)
            decrypted = cipher.decrypt(encrypted)

            assert decrypted == plaintext, (
                f"Roundtrip failed for sample {vector['sample']} "
                f"(radix {vector['radix']}): "
                f"original {plaintext}, got {decrypted}"
            )

    def test_nist_vector_count(self):
        """Test that we have the expected number of NIST vectors."""
        assert len(self.vectors) == 15, f"Expected 15 NIST vectors, got {len(self.vectors)}"

    def test_nist_vector_radix_coverage(self):
        """Test that NIST vectors cover expected radix values."""
        radix_values = {vector['radix'] for vector in self.vectors}
        expected_radix = {10, 26}  # Only these are in the official NIST vectors

        assert radix_values == expected_radix, (
            f"Expected radix values {expected_radix}, got {radix_values}"
        )

    def test_nist_vector_key_sizes(self):
        """Test that NIST vectors cover different key sizes."""
        key_sizes = {len(bytes.fromhex(vector['key'])) * 8 for vector in self.vectors}

        # Should have AES-128, AES-192, and AES-256
        assert 128 in key_sizes, "Missing AES-128 test vectors"
        assert 192 in key_sizes, "Missing AES-192 test vectors"
        assert 256 in key_sizes, "Missing AES-256 test vectors"

    def test_nist_vector_format_preservation(self):
        """Test that all NIST vectors preserve format (length)."""
        for vector in self.vectors:
            plaintext = vector['plaintext']
            ciphertext = vector['ciphertext']

            assert len(plaintext) == len(ciphertext), (
                f"Format not preserved for sample {vector['sample']}: "
                f"plaintext length {len(plaintext)}, ciphertext length {len(ciphertext)}"
            )

    def test_specific_known_vectors(self):
        """Test specific known NIST vectors with exact values."""
        # Test the first vector (commonly used as reference)
        first_vector = self.vectors[0]

        # This should be the digits radix 10 test
        assert first_vector['radix'] == 10
        assert first_vector['plaintext'] == "890121234567890000"
        assert first_vector['ciphertext'] == "750918814058654607"

        # Test encryption with our implementation
        key = bytes.fromhex(first_vector['key'])
        tweak = bytes.fromhex(first_vector['tweak'])
        cipher = digits(key, tweak)

        result = cipher.encrypt(first_vector['plaintext'])
        assert result == first_vector['ciphertext']