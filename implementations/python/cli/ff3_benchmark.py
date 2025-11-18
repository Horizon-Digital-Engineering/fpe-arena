#!/usr/bin/env python3
"""FF3 Performance Benchmark Tool with comprehensive CLI support"""

import argparse
import time
import sys
import json
import random
from pathlib import Path
from typing import List, Dict, Any

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


def get_alphabet_chars(alphabet: str) -> str:
    """Get character set for alphabet"""
    if alphabet == "digits":
        return ALPHA_DIGITS
    elif alphabet == "hex":
        return ALPHA_HEX_LOWER
    elif alphabet == "base36":
        return ALPHA_BASE36_LOW
    elif alphabet == "base62":
        return ALPHA_BASE62
    else:
        return ALPHA_DIGITS  # default


def build_cipher(alphabet: str, key: bytes, tweak: bytes):
    """Build cipher for alphabet"""
    if alphabet == "digits":
        return digits(key, tweak)
    elif alphabet == "hex":
        return hex_lower(key, tweak)
    elif alphabet == "base36":
        return base36_lower(key, tweak)
    elif alphabet == "base62":
        return base62(key, tweak)
    else:
        return digits(key, tweak)  # default


def generate_inputs(alphabet_chars: str, length: int, count: int, seed: int) -> List[str]:
    """Generate deterministic test inputs with seed"""
    rng = random.Random(seed)
    inputs = []
    for _ in range(count):
        input_str = ''.join(rng.choice(alphabet_chars) for _ in range(length))
        inputs.append(input_str)
    return inputs


def run_benchmark(cipher, inputs: List[str], case_type: str, iterations: int, verbose: bool) -> Dict[str, Any]:
    """Run benchmark for a specific case"""
    times = []
    checksum = 0

    # Precompute ciphertexts for decrypt benchmarks
    ciphertexts = [cipher.encrypt(pt) for pt in inputs]

    for i in range(iterations):
        input_idx = i % len(inputs)
        plaintext = inputs[input_idx]
        ciphertext = ciphertexts[input_idx]

        start = time.perf_counter_ns()

        if case_type == "enc":
            result = cipher.encrypt(plaintext)
        elif case_type == "dec":
            result = cipher.decrypt(ciphertext)
        elif case_type == "roundtrip":
            ct = cipher.encrypt(plaintext)
            result = cipher.decrypt(ct)
            if result != plaintext:
                raise ValueError(f"Round-trip verification failed: {plaintext} != {result}")
        else:
            raise ValueError(f"Unknown case type: {case_type}")

        end = time.perf_counter_ns()
        times.append(end - start)

        # XOR checksum to prevent dead code elimination
        for c in result:
            checksum ^= ord(c)

        if verbose and (i + 1) % 10000 == 0:
            print(f"  Progress: {i + 1}/{iterations}", file=sys.stderr)

    total_ns = sum(times)
    avg_ns = total_ns // len(times)
    ops_per_sec = int(1e9 / avg_ns) if avg_ns > 0 else 0

    return {
        "elapsed_ns": total_ns,
        "ns_per_op": avg_ns,
        "ops_per_sec": ops_per_sec,
        "checksum": f"{checksum:08x}"
    }


def main():
    parser = argparse.ArgumentParser(
        description="FF3 Performance Benchmark Tool",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
⚠️  FF3 was withdrawn by NIST due to security vulnerabilities.
   This tool is for educational and research purposes only.
        """
    )

    parser.add_argument("--config", type=str, help="Load configuration from JSON file")
    parser.add_argument("--alphabet", type=str, default="digits",
                       help="Alphabet to use (digits, hex, base36, base62)")
    parser.add_argument("--radix", type=int, default=10, help="Radix (2-62)")
    parser.add_argument("--lengths", type=str, default="9,12,16",
                       help="Comma-separated input lengths (e.g., '9,12,16')")
    parser.add_argument("--cases", type=str, default="enc,dec,roundtrip",
                       help="Comma-separated cases (enc,dec,roundtrip)")
    parser.add_argument("--iterations", type=int, default=100000,
                       help="Number of iterations (default: 100000)")
    parser.add_argument("--warmup", type=int, default=10000,
                       help="Warmup iterations (default: 10000)")
    parser.add_argument("--key", type=str, default="EF4359D8D580AA4F7F036D6F04FC6A94",
                       help="Encryption key (hex string)")
    parser.add_argument("--tweak", type=str, default="D8E7920AFA330A73",
                       help="Tweak value (hex string)")
    parser.add_argument("--seed", type=int, default=42,
                       help="Random seed for input generation")
    parser.add_argument("--verbose", action="store_true",
                       help="Show progress during benchmarks")
    parser.add_argument("--json-out", type=str,
                       help="Write JSON results to file")
    parser.add_argument("--quick", action="store_true",
                       help="Quick benchmark (fewer iterations)")
    parser.add_argument("-v", "--version", action="version", version="FF3 Benchmark Tool v0.1.0")

    args = parser.parse_args()

    # Load config file if provided
    if args.config:
        try:
            with open(args.config, 'r') as f:
                config = json.load(f)
                # Override defaults with config file values
                for key, value in config.items():
                    if hasattr(args, key) and value is not None:
                        # Convert lengths and cases from lists to comma-separated strings
                        if key == "lengths" and isinstance(value, list):
                            value = ','.join(map(str, value))
                        elif key == "cases" and isinstance(value, list):
                            value = ','.join(value)
                        setattr(args, key, value)
        except Exception as e:
            print(f"ERR: Failed to load config file: {e}", file=sys.stderr)
            sys.exit(2)

    # Handle --quick flag
    if args.quick:
        args.iterations = 10000
        args.warmup = 1000
        args.lengths = "9,12"
        args.cases = "enc,roundtrip"

    # Parse comma-separated values
    lengths = [int(x.strip()) for x in args.lengths.split(',')]
    cases = [x.strip() for x in args.cases.split(',')]

    # Convert hex strings to bytes
    key = bytes.fromhex(args.key)
    tweak = bytes.fromhex(args.tweak)

    if args.verbose:
        print("\n🚀 FF3 Performance Benchmark Tool v0.1.0", file=sys.stderr)
        print("=" * 44, file=sys.stderr)
        print("\n⚠️  FF3 was WITHDRAWN by NIST due to security vulnerabilities.", file=sys.stderr)
        print("    This benchmark is for EDUCATIONAL and RESEARCH purposes only.\n", file=sys.stderr)
        print("🔧 Configuration:", file=sys.stderr)
        print(f"   Alphabet: {args.alphabet}", file=sys.stderr)
        print(f"   Radix: {args.radix}", file=sys.stderr)
        print(f"   Key: {args.key[:16]}...", file=sys.stderr)
        print(f"   Tweak: {args.tweak}", file=sys.stderr)
        print(f"   Lengths: {lengths}", file=sys.stderr)
        print(f"   Cases: {cases}", file=sys.stderr)
        print(f"   Iterations: {args.iterations}", file=sys.stderr)
        print(f"   Warmup: {args.warmup}", file=sys.stderr)
        print(f"   Seed: {args.seed}", file=sys.stderr)
        print("\nStarting performance benchmarks...\n", file=sys.stderr)

    # Build cipher
    cipher = build_cipher(args.alphabet, key, tweak)
    alphabet_chars = get_alphabet_chars(args.alphabet)
    radix = len(alphabet_chars)

    benchmarks = []

    # Run benchmarks for each length
    for length in lengths:
        # Generate ring buffer of 64 varied inputs
        inputs = generate_inputs(alphabet_chars, length, 64, args.seed)

        # Warmup phase
        if args.warmup > 0:
            if args.verbose:
                print(f"\n📊 Warmup for length {length}...", file=sys.stderr)
            run_benchmark(cipher, inputs, "enc", args.warmup, False)

        # Benchmark each case
        for case_type in cases:
            if args.verbose:
                print(f"\n📊 Benchmarking {case_type}_length{length}_radix{radix}...", file=sys.stderr)

            result = run_benchmark(cipher, inputs, case_type, args.iterations, args.verbose)

            benchmarks.append({
                "name": f"{case_type}_len{length}_radix{radix}",
                "n": args.iterations,
                "elapsed_ns": result["elapsed_ns"],
                "ns_per_op": result["ns_per_op"],
                "ops_per_sec": result["ops_per_sec"],
                "checksum": result["checksum"],
                "checksum_int": int(result["checksum"], 16)
            })

            if args.verbose:
                print(f"   {result['ns_per_op']} ns/op ({result['ops_per_sec']:,} ops/sec)", file=sys.stderr)
                print(f"   Checksum: {result['checksum']}", file=sys.stderr)

    # Build spec-compliant JSON output
    import platform
    import os

    # Calculate total duration
    total_duration = sum(b["elapsed_ns"] for b in benchmarks) / 1e9

    # Calculate combined checksum (XOR all individual checksums would be here if we tracked them)
    combined_checksum = 0
    for b in benchmarks:
        combined_checksum ^= b.get("checksum_int", 0)

    output = {
        "metadata": {
            "version": "1.0",
            "timestamp": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
            "language": "python",
            "runtime": f"Python {sys.version.split()[0]}",
            "platform": {
                "os": platform.system(),
                "arch": platform.machine(),
                "cpu": platform.processor() or "unknown",
                "cores": os.cpu_count() or 0
            }
        },
        "configuration": {
            "seed": args.seed,
            "warmup_iterations": args.warmup
        },
        "benchmarks": [],
        "summary": {
            "total_tests": len(benchmarks),
            "total_duration_sec": total_duration,
            "checksum": f"{combined_checksum:08x}"
        }
    }

    # Build benchmarks array with proper structure
    for b in benchmarks:
        # Extract length from name (format: "enc_len9_radix10")
        name_parts = b["name"].split("_")
        test_case = name_parts[0]
        length_str = name_parts[1]  # "len9"
        length = int(length_str.replace("len", ""))

        output["benchmarks"].append({
            "name": b["name"],
            "test_case": test_case,
            "parameters": {
                "alphabet": args.alphabet,
                "radix": radix,
                "length": length,
                "key_bits": len(key) * 8,
                "key_fingerprint": args.key[:8].upper(),
                "tweak": args.tweak.upper()
            },
            "iterations": b["n"],
            "elapsed_ns": b["elapsed_ns"],
            "ns_per_op": b["ns_per_op"],
            "ops_per_sec": b["ops_per_sec"],
            "checksum": b.get("checksum", "00000000")
        })

    json_output = json.dumps(output, indent=2)

    if args.json_out:
        with open(args.json_out, 'w') as f:
            f.write(json_output)
        if args.verbose:
            print(f"\n✅ Results written to {args.json_out}", file=sys.stderr)
    else:
        print(json_output)

    if args.verbose:
        print("\n⚠️  FF3 Format Preserving Encryption Library v0.1.0", file=sys.stderr)
        print("\n    FF3 was WITHDRAWN by NIST due to security vulnerabilities.", file=sys.stderr)
        print("    This implementation is for EDUCATIONAL and RESEARCH purposes only.", file=sys.stderr)
        print("\n    DO NOT use in production systems.\n", file=sys.stderr)


if __name__ == "__main__":
    main()