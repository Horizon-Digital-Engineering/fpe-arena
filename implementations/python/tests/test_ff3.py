"""
Basic FF3 tests for Python implementation.
"""

import json
import os
import sys
from typing import List, Dict, Any
# import pytest  # Only needed for pytest runner

# Import our FF3 implementation
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))

# Import from the new package structure
from src.ff3 import (
    digits,
    hex,
    base36,
    base62,
    from_spec,
    AlphabetSpec,
    ALPHA_BASE36_LOW,
)


def load_nist_test_vectors() -> List[Dict[str, Any]]:
    """Load official NIST FF3 test vectors."""
    # Try environment variable first (for CI/CD)
    test_file = os.environ.get('FF3_TEST_VECTORS_PATH')

    if not test_file:
        # Fallback to relative path for local development
        test_file = os.path.join(
            os.path.dirname(__file__),
            "../../../shared/test-vectors/nist_ff3_official_vectors.json"
        )

    with open(test_file, 'r') as f:
        data = json.load(f)

    return data['vectors']


def test_basic_functionality():
    """Test basic encrypt/decrypt functionality."""
    key = bytes.fromhex("EF4359D8D580AA4F7F036D6F04FC6A94")
    tweak = bytes.fromhex("D8E7920AFA330A73")
    
    cipher = digits(key, tweak)
    
    plaintext = "1234567890"
    ciphertext = cipher.encrypt(plaintext)
    decrypted = cipher.decrypt(ciphertext)
    
    assert decrypted == plaintext
    assert ciphertext != plaintext
    print(f"Basic test: {plaintext} -> {ciphertext} -> {decrypted}")
    
    # Test against first NIST vector
    nist_plaintext = "890121234567890000"
    nist_expected = "750918814058654607"
    nist_result = cipher.encrypt(nist_plaintext)
    print(f"NIST test: {nist_plaintext} -> {nist_result} (expected: {nist_expected})")


def test_nist_vectors():
    """Test against official NIST FF3 test vectors."""
    vectors = load_nist_test_vectors()
    
    print(f"Testing {len(vectors)} NIST FF3 vectors...")
    
    for i, vector in enumerate(vectors):
        # Decode hex key and tweak
        key = bytes.fromhex(vector['key'])
        tweak = bytes.fromhex(vector['tweak'])
        
        # Create cipher based on radix
        if vector['radix'] == 10:
            cipher = digits(key, tweak)
        elif vector['radix'] == 16:
            cipher = hex(key, tweak)
        elif vector['radix'] == 26:
            # Use first 26 chars of base36 alphabet (0-9a-p)
            spec = AlphabetSpec(ALPHA_BASE36_LOW[:26])
            cipher = from_spec(key, tweak, spec)
        elif vector['radix'] == 36:
            cipher = base36(key, tweak)
        elif vector['radix'] == 62:
            cipher = base62(key, tweak)
        else:
            print(f"Skipping unsupported radix {vector['radix']} in test vectors")
            continue
        
        # Test encryption
        encrypted = cipher.encrypt(vector['plaintext'])
        
        print(f"Sample {vector['sample']} ({vector['algorithm']}):")
        print(f"  Key: {vector['key']}")
        print(f"  Tweak: {vector['tweak']}")
        print(f"  Plaintext: {vector['plaintext']}")
        print(f"  Expected: {vector['ciphertext']}")
        print(f"  Got:      {encrypted}")
        
        if encrypted == vector['ciphertext']:
            print(f"  ✅ PERFECT MATCH!")
        else:
            print(f"  ❌ MISMATCH")
            
        # Assert exact match with NIST vectors
        assert encrypted == vector['ciphertext'], f"Sample {vector['sample']} failed"
        
        # Test decryption (round-trip)
        decrypted = cipher.decrypt(encrypted)
        assert decrypted == vector['plaintext'], f"Round-trip failed for sample {vector['sample']}"
        print(f"  ✅ Round-trip successful")
        print()


def test_format_preservation():
    """Test that format is preserved exactly."""
    key = bytes.fromhex("EF4359D8D580AA4F7F036D6F04FC6A94")
    tweak = bytes.fromhex("D8E7920AFA330A73")
    
    # Test base62 case preservation
    cipher = base62(key, tweak)
    test_cases = ["Ab9Z", "Hello123", "TestCase"]
    
    for plaintext in test_cases:
        ciphertext = cipher.encrypt(plaintext)
        decrypted = cipher.decrypt(ciphertext)
        
        assert decrypted == plaintext
        print(f"Format preservation: {plaintext} -> {ciphertext} -> {decrypted}")


if __name__ == "__main__":
    test_basic_functionality()
    test_format_preservation()
    test_nist_vectors()
    print("All tests passed! ✅")