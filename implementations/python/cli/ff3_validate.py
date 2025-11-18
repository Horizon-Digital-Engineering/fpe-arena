#!/usr/bin/env python3
"""FF3 NIST Validation Tool - Validates against official test vectors"""

import argparse
import json
import sys
import os
import binascii
from pathlib import Path

# Add parent directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))

from src.ff3 import (
    digits,
    base36_lower,
    base62,
    from_spec,
    AlphabetSpec,
)


def find_vectors_file(custom_path):
    """Find the test vectors file, trying multiple locations"""
    if custom_path:
        if os.path.exists(custom_path):
            return custom_path
        print(f"Error: Vectors file not found: {custom_path}", file=sys.stderr)
        sys.exit(2)

    # Try multiple paths
    paths = [
        os.environ.get('FF3_TEST_VECTORS_PATH'),
        "../../shared/test-vectors/nist_ff3_official_vectors.json",
        "../../../shared/test-vectors/nist_ff3_official_vectors.json",
        "./nist_ff3_official_vectors.json",
    ]

    for path in paths:
        if path and os.path.exists(path):
            return path

    print("Error: Could not find NIST test vectors file", file=sys.stderr)
    print("Try: ff3-validate --vectors /path/to/vectors.json", file=sys.stderr)
    sys.exit(2)


def main():
    parser = argparse.ArgumentParser(
        description="FF3 NIST Test Vector Validation Tool",
        add_help=False
    )
    parser.add_argument("--vectors", type=str, help="Path to test vectors JSON file")
    parser.add_argument("--verbose", action="store_true", help="Show detailed test output")
    parser.add_argument("--quiet", action="store_true", help="Only show failures and summary")
    parser.add_argument("-h", "--help", action="store_true", help="Show help message")

    args = parser.parse_args()

    if args.help:
        print("FF3 NIST Test Vector Validation Tool")
        print()
        print("Usage: ff3-validate [OPTIONS]")
        print()
        print("Options:")
        print("  --vectors PATH    Path to test vectors JSON file")
        print("  --verbose         Show detailed test output")
        print("  --quiet           Only show failures and summary")
        print("  -h, --help        Show this help message")
        print()
        sys.exit(0)

    if not args.quiet:
        print("FF3 NIST Validation Tool")
        print("=" * 40)

    vectors_path = find_vectors_file(args.vectors)

    if not args.quiet:
        print(f"Vector file: {vectors_path}")
        print()

    try:
        with open(vectors_path, 'r') as f:
            data = json.load(f)
            vectors = data.get("vectors", [])
    except FileNotFoundError:
        print(f"Error: Test vector file not found: {vectors_path}", file=sys.stderr)
        sys.exit(2)
    except json.JSONDecodeError as e:
        print(f"Error: Invalid JSON in test vector file: {e}", file=sys.stderr)
        sys.exit(1)

    if not args.quiet:
        print(f"Testing {len(vectors)} NIST FF3 vectors...")
        print()

    passed = 0
    total = 0

    for vector in vectors:
        total += 1
        try:
            key = binascii.unhexlify(vector["key"])
            tweak = binascii.unhexlify(vector["tweak"])
            radix = vector["radix"]
            plaintext = vector["plaintext"]
            expected = vector["ciphertext"]

            # Get appropriate cipher based on radix
            if radix == 10:
                cipher = digits(key, tweak)
            elif radix == 26:
                # Use a custom radix-26 alphabet (matching other implementations)
                spec = AlphabetSpec("0123456789abcdefghijklmnop")
                cipher = from_spec(key, tweak, spec)
            elif radix == 36:
                cipher = base36_lower(key, tweak)
            elif radix == 62:
                cipher = base62(key, tweak)
            else:
                raise ValueError(f"Unsupported radix: {radix}")

            # Test encryption
            actual = cipher.encrypt(plaintext)
            encrypt_passed = actual == expected

            # Test roundtrip
            decrypted = cipher.decrypt(actual)
            roundtrip_passed = decrypted == plaintext

            test_passed = encrypt_passed and roundtrip_passed

            if test_passed:
                passed += 1
                if args.verbose:
                    print(f"Sample {vector.get('sample', total)}: PASS")
            else:
                if not args.quiet:
                    print(f"Sample {vector.get('sample', total)}: FAIL")
                    if not encrypt_passed:
                        print(f"  Expected:  {expected}")
                        print(f"  Got:       {actual}")
                    if not roundtrip_passed:
                        print(f"  Round-trip failed")
                    print()

        except Exception as e:
            if not args.quiet:
                print(f"Sample {vector.get('sample', total)}: ERROR - {e}")

    if not args.quiet:
        print("=" * 40)
        print(f"Results: {passed}/{total} passed ({passed/total*100:.1f}%)")
        print()

    if passed == total:
        if not args.quiet:
            print("ALL NIST TEST VECTORS PASSED!")
            print()
            print("WARNING: FF3 was WITHDRAWN by NIST due to security vulnerabilities.")
            print("This implementation is for EDUCATIONAL and RESEARCH purposes only.")
            print("DO NOT use in production systems.")
            print()
        sys.exit(0)
    else:
        if not args.quiet:
            print("VALIDATION FAILED")
        sys.exit(1)


if __name__ == "__main__":
    main()
