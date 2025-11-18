#!/usr/bin/env python3
"""Simple test to check NIST vector output"""

import sys
import os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))

from src.ff3 import digits
import binascii

def test_nist_sample1():
    print("🔍 Python FF3 NIST Sample 1...")

    # NIST Sample 1 parameters
    key = binascii.unhexlify("EF4359D8D580AA4F7F036D6F04FC6A94")
    tweak = binascii.unhexlify("D8E7920AFA330A73")
    plaintext = "890121234567890000"

    print(f"Key: {key.hex().upper()}")
    print(f"Tweak: {tweak.hex().upper()}")
    print(f"Plaintext: {plaintext}")

    cipher = digits(key, tweak)
    result = cipher.encrypt(plaintext)

    print(f"Result: {result}")
    print(f"Expected: 750918814058654607")

    if result == "750918814058654607":
        print("✅ PERFECT MATCH!")
    else:
        print("❌ MISMATCH!")

if __name__ == "__main__":
    test_nist_sample1()