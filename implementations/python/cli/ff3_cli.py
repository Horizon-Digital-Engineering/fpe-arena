#!/usr/bin/env python3
"""FF3 CLI - Command-line tool for FF3 encryption/decryption"""

import argparse
import sys
import binascii
from pathlib import Path

# Add parent directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))

from src.ff3 import (
    digits,
    hex_lower,
    hex_upper,
    base36_lower,
    base36_upper,
    base62,
    from_spec,
    AlphabetSpec,
)


def parse_hex(hex_str):
    """Parse hex string to bytes"""
    try:
        return binascii.unhexlify(hex_str)
    except binascii.Error as e:
        raise ValueError(f"Invalid hex string: {e}")


def create_cipher(key, tweak, alphabet_type, custom_charset):
    """Create cipher based on alphabet type"""
    if custom_charset:
        spec = AlphabetSpec(custom_charset)
        return from_spec(key, tweak, spec)

    alphabet_lower = alphabet_type.lower()

    if alphabet_lower == "digits":
        return digits(key, tweak)
    elif alphabet_lower in ("hex", "hex-lower"):
        return hex_lower(key, tweak)
    elif alphabet_lower == "hex-upper":
        return hex_upper(key, tweak)
    elif alphabet_lower in ("base36", "base36-lower"):
        return base36_lower(key, tweak)
    elif alphabet_lower == "base36-upper":
        return base36_upper(key, tweak)
    elif alphabet_lower == "base62":
        return base62(key, tweak)
    else:
        raise ValueError(f"Unknown alphabet type: {alphabet_type}")


def show_usage():
    """Display usage information"""
    print("FF3 CLI - Format Preserving Encryption")
    print()
    print("Usage: ff3-cli [OPTIONS]")
    print()
    print("Options:")
    print("  -e, --encrypt TEXT      Encrypt the given text")
    print("  -d, --decrypt TEXT      Decrypt the given text")
    print("  -k, --key HEX           AES key in hex format (32/48/64 hex chars)")
    print("  -t, --tweak HEX         Tweak in hex format (16 hex chars)")
    print("  -a, --alphabet TYPE     Alphabet type:")
    print("                            digits (default)")
    print("                            hex-lower")
    print("                            hex-upper")
    print("                            base36-lower")
    print("                            base36-upper")
    print("                            base62")
    print("  -c, --custom CHARSET    Custom alphabet charset")
    print("  -h, --help              Show this help message")
    print()
    print("Examples:")
    print('  ff3-cli -e "1234567890" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73')
    print('  ff3-cli -d "7501889140" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73')
    print()


def main():
    parser = argparse.ArgumentParser(
        description="FF3 Format Preserving Encryption CLI",
        add_help=False
    )

    parser.add_argument("-e", "--encrypt", type=str, help="Encrypt the given text")
    parser.add_argument("-d", "--decrypt", type=str, help="Decrypt the given text")
    parser.add_argument("-k", "--key", type=str, help="AES key in hex format")
    parser.add_argument("-t", "--tweak", type=str, help="Tweak in hex format")
    parser.add_argument("-a", "--alphabet", type=str, default="digits", help="Alphabet type")
    parser.add_argument("-c", "--custom", type=str, help="Custom alphabet charset")
    parser.add_argument("-h", "--help", action="store_true", help="Show help message")

    args = parser.parse_args()

    if args.help:
        show_usage()
        sys.exit(0)

    # Validate required arguments
    if not args.encrypt and not args.decrypt:
        print("Error: Either --encrypt or --decrypt must be specified", file=sys.stderr)
        show_usage()
        sys.exit(1)

    if not args.key or not args.tweak:
        print("Error: Key (-k) and tweak (-t) are required", file=sys.stderr)
        show_usage()
        sys.exit(1)

    try:
        # Parse key and tweak
        key = parse_hex(args.key)
        tweak = parse_hex(args.tweak)

        # Validate key length
        if len(key) not in (16, 24, 32):
            print(f"Error: Key must be 16, 24, or 32 bytes (32, 48, or 64 hex chars), got {len(key)} bytes", file=sys.stderr)
            sys.exit(1)

        # Validate tweak length
        if len(tweak) != 8:
            print(f"Error: Tweak must be 8 bytes (16 hex chars), got {len(tweak)} bytes", file=sys.stderr)
            sys.exit(1)

        # Create cipher
        cipher = create_cipher(key, tweak, args.alphabet, args.custom)

        # Encrypt or decrypt
        if args.encrypt:
            result = cipher.encrypt(args.encrypt)
            print(result)
        elif args.decrypt:
            result = cipher.decrypt(args.decrypt)
            print(result)

    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
