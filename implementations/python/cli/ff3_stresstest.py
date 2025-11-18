#!/usr/bin/env python3
"""FF3 Stress Test Tool - Randomized testing for stability."""

import argparse
import random
import secrets
import sys
import time
from pathlib import Path

# Add parent directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))

from src.ff3 import (
    digits,
    hex_lower,
    base36_lower,
    base62,
    ALPHA_DIGITS,
    ALPHA_HEX_LOWER,
    ALPHA_BASE36_LOW,
    ALPHA_BASE62,
)

DEFAULT_ITERATIONS = 1000
DEFAULT_MIN_LEN = 6
DEFAULT_MAX_LEN = 20
DEFAULT_ALPHABETS = ["digits", "hex-lower", "base36-lower", "base62"]

ALPHABET_MAP = {
    "digits": (ALPHA_DIGITS, digits),
    "hex-lower": (ALPHA_HEX_LOWER, hex_lower),
    "base36-lower": (ALPHA_BASE36_LOW, base36_lower),
    "base62": (ALPHA_BASE62, base62),
}


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="FF3 Stress Test Tool",
        formatter_class=argparse.RawTextHelpFormatter,
    )
    parser.add_argument(
        "iterations",
        nargs="?",
        type=int,
        default=DEFAULT_ITERATIONS,
        help="Number of iterations per alphabet (default: 1000)",
    )
    parser.add_argument(
        "--alphabets",
        type=str,
        default="",
        help="Comma-separated list (default: digits,hex-lower,base36-lower,base62)",
    )
    parser.add_argument(
        "--min-length",
        type=int,
        default=DEFAULT_MIN_LEN,
        help="Minimum plaintext length (default: 6)",
    )
    parser.add_argument(
        "--max-length",
        type=int,
        default=DEFAULT_MAX_LEN,
        help="Maximum plaintext length (default: 20)",
    )
    parser.add_argument(
        "--quick",
        action="store_true",
        help="Run 100 iterations (fast test)",
    )
    parser.add_argument(
        "--seed",
        type=int,
        default=None,
        help="Random seed for reproducibility",
    )
    return parser.parse_args()


def validate_options(ns: argparse.Namespace) -> tuple[int, list[str], int, int, int]:
    iterations = ns.iterations
    if ns.quick:
        iterations = 100
    if iterations <= 0:
        raise ValueError("iterations must be greater than 0")

    min_len = ns.min_length
    max_len = ns.max_length
    if min_len < 2:
        raise ValueError("--min-length must be at least 2")
    if max_len < min_len:
        raise ValueError("--max-length must be greater than or equal to --min-length")

    if ns.alphabets.strip() == "":
        alphabets = DEFAULT_ALPHABETS
    else:
        alphabets = [name.strip() for name in ns.alphabets.split(",") if name.strip()]
        if not alphabets:
            raise ValueError("--alphabets list cannot be empty")

    unknown = [name for name in alphabets if name not in ALPHABET_MAP]
    if unknown:
        raise ValueError(f"unknown alphabet(s): {', '.join(unknown)}")

    seed = ns.seed
    return iterations, alphabets, min_len, max_len, seed


def generate_plaintext(charset: str, length: int, rng: random.Random) -> str:
    return "".join(rng.choice(charset) for _ in range(length))


def print_failure(key: bytes, tweak: bytes, plaintext: str, ciphertext: str, decrypted: str | None, detail: str | None) -> None:
    print("  Round-trip failed:")
    print(f"    Key: {key.hex()}")
    print(f"    Tweak: {tweak.hex()}")
    print(f"    Plaintext: \"{plaintext}\"")
    if ciphertext:
        print(f"    Ciphertext: \"{ciphertext}\"")
    if decrypted:
        print(f"    Decrypted: \"{decrypted}\"")
    if detail:
        print(f"    Detail: {detail}")


def stress_test_alphabet(
    name: str,
    charset: str,
    factory,
    iterations: int,
    min_len: int,
    max_len: int,
    rng: random.Random,
    byte_source,
) -> tuple[int, int]:
    print(f"Testing {name}...")
    print(f"  Alphabet: {charset} (radix {len(charset)})")

    passed = 0
    failed = 0
    interval = max(1, iterations // 10)

    for i in range(iterations):
        key = byte_source(16)
        tweak = byte_source(8)
        length = rng.randint(min_len, max_len)
        plaintext = generate_plaintext(charset, length, rng)

        try:
            cipher = factory(key, tweak)
        except Exception as exc:  # pylint: disable=broad-except
            failed += 1
            print_failure(key, tweak, plaintext, "", None, f"cipher initialization failed: {exc}")
            continue

        try:
            ciphertext = cipher.encrypt(plaintext)
            decrypted = cipher.decrypt(ciphertext)
        except Exception as exc:  # pylint: disable=broad-except
            failed += 1
            print_failure(key, tweak, plaintext, "", None, f"encryption error: {exc}")
            continue

        if decrypted != plaintext:
            failed += 1
            print_failure(key, tweak, plaintext, ciphertext, decrypted, "round-trip mismatch")
        else:
            passed += 1

        if (i + 1) % interval == 0 or (i + 1) == iterations:
            percent = (i + 1) * 100 // iterations
            print(f"  Progress: {i + 1}/{iterations} ({percent}%)")

    print(f"  Passed: {passed}/{passed + failed}")
    print(f"  Failed: {failed}/{passed + failed}\n")
    return passed, failed


def main() -> int:
    try:
        args = parse_args()
        iterations, alphabets, min_len, max_len, seed = validate_options(args)
    except ValueError as exc:
        print(f"Error: {exc}", file=sys.stderr)
        return 1

    rng = random.Random(seed) if seed is not None else random.Random()
    if seed is not None:
        def byte_source(size: int) -> bytes:
            return bytes(rng.randrange(0, 256) for _ in range(size))
    else:
        def byte_source(size: int) -> bytes:
            return secrets.token_bytes(size)

    print("FF3 Stress Test v1.0")
    print("====================\n")
    print("Warning: FF3 was withdrawn by NIST; run for education and research only.\n")
    print("Test configuration")
    print(f"  Iterations per alphabet: {iterations}")
    print("  Random key/tweak generation: enabled")
    print(f"  String length range: {min_len}-{max_len} characters")
    print(f"  Alphabets: {', '.join(alphabets)}\n")

    total_tests = 0
    total_failures = 0
    start = time.time()

    for name in alphabets:
        charset, factory = ALPHABET_MAP[name]
        passed, failed = stress_test_alphabet(name, charset, factory, iterations, min_len, max_len, rng, byte_source)
        total_tests += passed + failed
        total_failures += failed

    duration_ms = int((time.time() - start) * 1000)

    print("Summary")
    print(f"  Total tests: {total_tests}")
    print(f"  Failures: {total_failures}")
    if duration_ms > 0:
        throughput = (total_tests * 1000) / duration_ms
        print(f"  Duration: {duration_ms} ms")
        print(f"  Throughput: {throughput:.2f} tests/sec")
    if total_failures == 0:
        print("  Result: all stress tests passed")
        return 0
    print("  Result: failures detected")
    return 1


if __name__ == "__main__":
    sys.exit(main())
