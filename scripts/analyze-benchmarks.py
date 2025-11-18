#!/usr/bin/env python3
"""
Analyze and compare FF3 benchmark results across all language implementations.

Reads JSON benchmark files from artifacts/ directory and generates:
- Performance comparison table
- Statistics summary
- Language rankings by operation speed
"""

import json
import sys
from pathlib import Path
from collections import defaultdict
from typing import Dict, List, Tuple


def load_benchmark_file(filepath: Path) -> Dict:
    """Load and parse a benchmark JSON file."""
    try:
        with open(filepath, 'r') as f:
            return json.load(f)
    except Exception as e:
        print(f"Warning: Failed to load {filepath}: {e}", file=sys.stderr)
        return None


def extract_benchmarks(data: Dict) -> List[Dict]:
    """Extract benchmark results from JSON data."""
    if not data:
        return []
    return data.get('benchmarks', [])


def group_by_operation(benchmarks: List[Dict]) -> Dict[str, List[Tuple[str, int]]]:
    """Group benchmarks by operation type (enc, dec, roundtrip) and length."""
    grouped = defaultdict(lambda: defaultdict(list))

    for bench in benchmarks:
        test_case = bench.get('test_case', 'unknown')
        length = bench['parameters'].get('length', 0)
        ops_per_sec = bench.get('ops_per_sec', 0)
        lang = bench.get('name', '').split('_')[0] if '_' in bench.get('name', '') else 'unknown'

        key = f"{test_case}_len{length}"
        grouped[key][lang].append(ops_per_sec)

    # Average multiple runs per language
    averaged = {}
    for key, lang_data in grouped.items():
        averaged[key] = [(lang, sum(ops)/len(ops)) for lang, ops in lang_data.items()]

    return averaged


def format_ops_per_sec(ops: int) -> str:
    """Format operations per second with appropriate units."""
    if ops >= 1_000_000:
        return f"{ops/1_000_000:.2f}M"
    elif ops >= 1_000:
        return f"{ops/1_000:.1f}K"
    else:
        return f"{ops}"


def print_comparison_table(artifacts_dir: Path):
    """Print a comparison table of all benchmark results."""

    # Load all benchmark files
    all_results = {}
    for bench_file in sorted(artifacts_dir.glob('*-bench.json')):
        lang = bench_file.stem.replace('-bench', '')
        data = load_benchmark_file(bench_file)
        if data:
            all_results[lang] = data

    if not all_results:
        print("No benchmark files found!", file=sys.stderr)
        return

    # Collect all benchmarks by language
    lang_benchmarks = {}
    for lang, data in all_results.items():
        benchmarks = extract_benchmarks(data)
        if benchmarks:
            lang_benchmarks[lang] = benchmarks

    # Group by operation and length
    all_ops = defaultdict(dict)
    for lang, benchmarks in lang_benchmarks.items():
        for bench in benchmarks:
            test_case = bench.get('test_case', 'unknown')
            length = bench['parameters'].get('length', 0)
            ops_per_sec = bench.get('ops_per_sec', 0)
            key = f"{test_case}_len{length}"

            if lang not in all_ops[key]:
                all_ops[key][lang] = []
            all_ops[key][lang].append(ops_per_sec)

    # Average multiple runs
    for key in all_ops:
        for lang in all_ops[key]:
            all_ops[key][lang] = sum(all_ops[key][lang]) / len(all_ops[key][lang])

    # Print header
    print("\n" + "="*80)
    print("FF3 Multi-Language Benchmark Comparison")
    print("="*80)
    print(f"\nTotal implementations: {len(lang_benchmarks)}")
    print(f"Languages: {', '.join(sorted(lang_benchmarks.keys()))}")
    print()

    # Print results by operation type
    operations = sorted(set(key.split('_len')[0] for key in all_ops.keys()))

    for op in operations:
        print(f"\n{'='*80}")
        print(f"Operation: {op.upper()}")
        print(f"{'='*80}")

        # Get all keys for this operation
        op_keys = sorted([k for k in all_ops.keys() if k.startswith(op)])

        if not op_keys:
            continue

        # Print table header
        langs = sorted(set(lang for key in op_keys for lang in all_ops[key].keys()))

        # Column widths
        col_width = 12
        print(f"\n{'Length':<10}", end='')
        for lang in langs:
            print(f"{lang.capitalize():<{col_width}}", end='')
        print("  Winner")
        print("-" * (10 + col_width * len(langs) + 20))

        # Print rows
        for key in op_keys:
            length = key.split('_len')[1]
            print(f"len={length:<5}", end='')

            results = all_ops[key]
            max_ops = max(results.values()) if results else 0

            for lang in langs:
                ops = results.get(lang, 0)
                formatted = format_ops_per_sec(int(ops))
                if ops == max_ops and ops > 0:
                    print(f"{formatted:<{col_width}}", end='')
                else:
                    print(f"{formatted:<{col_width}}", end='')

            # Winner
            if results:
                winner_lang = max(results.items(), key=lambda x: x[1])[0]
                print(f"  🏆 {winner_lang.capitalize()}")
            else:
                print()

    # Overall statistics
    print(f"\n{'='*80}")
    print("Overall Performance Summary")
    print(f"{'='*80}\n")

    lang_avg_ops = {}
    for lang, benchmarks in lang_benchmarks.items():
        total_ops = sum(b.get('ops_per_sec', 0) for b in benchmarks)
        count = len(benchmarks)
        lang_avg_ops[lang] = total_ops / count if count > 0 else 0

    # Sort by performance
    ranked = sorted(lang_avg_ops.items(), key=lambda x: x[1], reverse=True)

    print(f"{'Rank':<6} {'Language':<15} {'Avg Ops/Sec':<15} {'Performance'}")
    print("-" * 70)

    for i, (lang, avg_ops) in enumerate(ranked, 1):
        medal = "🥇" if i == 1 else "🥈" if i == 2 else "🥉" if i == 3 else f"{i:2d}."
        bar_length = int((avg_ops / ranked[0][1]) * 40) if ranked[0][1] > 0 else 0
        bar = "█" * bar_length
        print(f"{medal:<6} {lang.capitalize():<15} {format_ops_per_sec(int(avg_ops)):<15} {bar}")

    print()


def main():
    """Main entry point."""
    # Find artifacts directory
    script_dir = Path(__file__).parent
    project_dir = script_dir.parent
    artifacts_dir = project_dir / 'artifacts'

    if not artifacts_dir.exists():
        print(f"Error: Artifacts directory not found at {artifacts_dir}", file=sys.stderr)
        sys.exit(1)

    print_comparison_table(artifacts_dir)


if __name__ == '__main__':
    main()
