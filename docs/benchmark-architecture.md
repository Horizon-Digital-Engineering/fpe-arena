# FF3 Benchmark CLI Architecture

**Version:** 1.0
**Status:** Reference
**Last Updated:** 2024-12-01

## Overview

Standard architecture for FF3 benchmark CLI tools producing consistent, comparable results across implementations.

### Quick Summary

**What it does:**
- Measures encrypt/decrypt/roundtrip performance
- Proper warmup to eliminate JIT/cache effects
- Ring buffer of varied inputs to prevent optimizations
- Outputs JSON for cross-language comparison

**How to use:**
```bash
# Quick test
ff3-benchmark --quick --verbose

# Full test with output file
ff3-benchmark --lengths 9,12,16,24,32 --iterations 200000 --json-out results.json

# Predefined test suite
ff3-benchmark --config standard-suite.json --json-out results.json
```

## Design Goals

1. **Simple** - Single file, minimal dependencies
2. **Consistent** - Same flags and output format across all implementations
3. **Scientific** - Proper warmup, varied inputs, checksums to prevent optimizations
4. **Comparable** - JSON output with platform metadata
5. **Practical** - CLI flags for quick tests, JSON config for complex suites

## Input Methods

### 1. Command Line Flags (Primary)

Standard CLI flags all implementations should support:

```bash
ff3-benchmark \
  --config <file>             # read test configuration from JSON (alternative to flags)
  --alphabet <name>           # digits|hex|base36|base62|custom (default: digits)
  --radix <n>                 # 2..62, used with alphabet=custom (default: 10)
  --lengths <csv>             # comma-separated lengths (default: 9,12,16)
  --cases <csv>               # enc,dec,roundtrip (default: all three)
  --iterations <n>            # measured iterations per case (default: 100000)
  --warmup <n>                # warmup iterations (default: 10000)
  --key <hex>                 # hex-encoded key (default: standard test key)
  --tweak <hex>               # hex-encoded tweak (default: standard test tweak)
  --quick                     # reduce iterations 10x for fast testing
  --json-out <file>           # write JSON results to file (silent mode)
  --verbose                   # show simple progress: "Running X... done (Y ns/op)"
```

### 2. JSON Config File (For Complex Suites)

Simple format matching CLI flags:

```json
{
  "alphabet": "digits",
  "radix": 10,
  "lengths": [9, 12, 16, 24, 32],
  "cases": ["enc", "dec", "roundtrip"],
  "iterations": 200000,
  "warmup": 20000,
  "key": "EF4359D8D580AA4F7F036D6F04FC6A94",
  "tweak": "D8E7920AFA330A73"
}
```

Or multiple test configurations:

```json
{
  "tests": [
    {
      "name": "digits_suite",
      "alphabet": "digits",
      "lengths": [9, 12, 16],
      "cases": ["enc", "dec", "roundtrip"],
      "iterations": 100000
    },
    {
      "name": "base62_suite",
      "alphabet": "base62",
      "lengths": [16, 24],
      "cases": ["enc", "roundtrip"],
      "iterations": 50000,
      "key": "EF4359D8D580AA4F7F036D6F04FC6A942B7E151628AED2A6ABF7158809CF4F3C"
    }
  ]
}
```

Usage:
```bash
ff3-benchmark --config standard-suite.json --json-out results.json
```

**Note:** Config file values override CLI flags. CLI flags provide defaults for missing config values.

## Test Methodology

### Warmup Phase

**Purpose:** Eliminate JIT compilation, cache effects, and CPU frequency scaling

**Implementation:**
1. Perform warmup iterations (default: 10,000 or 10% of measured iterations)
2. Use actual test data, not dummy values
3. Discard all warmup timing results
4. Optional: Detect when performance stabilizes

### Input Variation

**Problem:** Repeated encryption of same input can be optimized away by compilers or cached

**Solution:** Use ring buffer of varied inputs
- Generate 64-256 random inputs matching the test configuration
- Rotate through inputs during benchmark: `input = inputs[i % inputs.length]`
- Use crypto-random generation for variety
- Pre-compute ciphertexts for decrypt benchmarks

### Dead Code Elimination Prevention

**Problem:** Compiler may optimize away unused results

**Solution:** Accumulate checksum of all outputs
```
checksum = 0
for i in iterations:
    result = cipher.encrypt(input)
    checksum ^= hash(result)  // Simple hash, e.g., XOR of bytes
```

Include checksum in output to prove work was done.

### Timing Precision

**Requirements:**
- Use high-resolution monotonic clock (not wall clock)
- Measure in nanoseconds internally
- Report ns/op and ops/sec
- Time only the measured phase, not warmup
- Exclude setup/teardown from timing

### Test Cases

Three standard operations to measure:

1. **enc** - Encrypt plaintext → ciphertext
2. **dec** - Decrypt ciphertext → plaintext
3. **roundtrip** - Encrypt then decrypt (measures full cycle)

## Output Format

### JSON Output Schema

```json
{
  "metadata": {
    "version": "1.0",
    "timestamp": "2025-09-29T21:30:00Z",
    "language": "go",
    "runtime": "go1.23.1",
    "platform": {
      "os": "darwin",
      "arch": "arm64",
      "cpu": "Apple M2",
      "cores": 8
    }
  },
  "configuration": {
    "seed": 12345,
    "warmup_iterations": 10000
  },
  "benchmarks": [
    {
      "name": "enc_len9_radix10",
      "test_case": "enc",
      "parameters": {
        "alphabet": "digits",
        "radix": 10,
        "length": 9,
        "key_bits": 128,
        "key_fingerprint": "ef4359d8",
        "tweak": "d8e7920afa330a73"
      },
      "iterations": 100000,
      "elapsed_ns": 6234567890,
      "ns_per_op": 62345.67,
      "ops_per_sec": 16040.23,
      "checksum": "a3f5c2e1"
    }
  ],
  "summary": {
    "total_tests": 9,
    "total_duration_sec": 56.234,
    "checksum": "combined_checksum_here"
  }
}
```

### Console Output Modes

#### Default (no flags):
Outputs JSON to stdout
```bash
ff3-benchmark
# {"metadata": {...}, "benchmarks": [...]}
```

#### With --json-out:
Silent mode - JSON written to file, nothing to screen
```bash
ff3-benchmark --json-out results.json
# (no output)
```

#### With --verbose:
Simple progress to stderr, JSON to stdout at end
```bash
ff3-benchmark --verbose
```
```
Running enc_len9_radix10... done (62,346 ns/op, 16,040 ops/sec)
Running dec_len9_radix10... done (61,234 ns/op, 16,330 ops/sec)
Running roundtrip_len9_radix10... done (123,456 ns/op, 8,100 ops/sec)
Running enc_len12_radix10... done (66,578 ns/op, 15,020 ops/sec)
Running dec_len12_radix10... done (66,793 ns/op, 14,972 ops/sec)
Running roundtrip_len12_radix10... done (133,586 ns/op, 7,486 ops/sec)
...

{"metadata": {...}, "benchmarks": [...]}
```

**No fancy progress bars or dashboards** - keeps implementation simple (~5 lines of code).

## Implementation Guidelines

### Implementation Considerations

Use language-appropriate:
- High-resolution monotonic clock for timing
- Cryptographically secure random for input generation
- Standard CLI argument parsing library
- Iteration counts based on language performance characteristics

### Common Pitfalls to Avoid

1. **No Warmup** - Cold starts skew results
2. **Single Input** - Compiler optimizations or caching
3. **Wall Clock Time** - Subject to system load
4. **Including Setup** - Measure only the operation
5. **Ignoring Results** - Compiler may eliminate "dead" code
6. **Too Few Iterations** - High variance in results
7. **No Variance Reporting** - Can't assess reliability

## Comparison and Aggregation

To fairly compare implementations:

1. **Normalize by hardware** - Use same machine or scale by reference benchmark
2. **Use consistent inputs** - Same key, tweak, lengths, radix
3. **Same iteration counts** - Adjust for performance characteristics
4. **Report confidence intervals** - Run multiple times, show variance
5. **Control for system load** - Run on idle system

## Example Usage

### Quick Test
```bash
# Fast sanity check (10x fewer iterations)
ff3-benchmark --quick
```

### Standard Test (JSON to stdout)
```bash
ff3-benchmark
```

### With Progress
```bash
ff3-benchmark --verbose
```

### Full Suite to File
```bash
ff3-benchmark \
  --lengths 9,12,16,24,32 \
  --cases enc,dec,roundtrip \
  --iterations 200000 \
  --warmup 20000 \
  --json-out results.json
```

### Custom Scenario
```bash
ff3-benchmark \
  --alphabet base62 \
  --lengths 16 \
  --key EF4359D8D580AA4F7F036D6F04FC6A94 \
  --tweak D8E7920AFA330A73 \
  --iterations 100000 \
  --verbose
```

### JSON Config File
```bash
# Run predefined test suite
ff3-benchmark --config test-suite.json --json-out results.json
```

## Implementation Checklist

Use this checklist when implementing a new language benchmark to ensure full compliance with the standard.

### Phase 1: Input Parsing 

**Required CLI Flags** (all must be supported):
```
- --config <file>          # JSON config file (CRITICAL - enables cross-language testing)
- --alphabet <name>        # digits|hex|base36|base62|custom
- --radix <n>              # 2..62 for custom alphabets
- --lengths <csv>          # Comma-separated lengths (e.g., "9,12,16")
- --cases <csv>            # enc,dec,roundtrip (comma-separated)
- --iterations <n>         # Measured iterations per case
- --warmup <n>             # Warmup iterations per case
- --key <hex>              # Hex-encoded key
- --tweak <hex>            # Hex-encoded tweak
- --seed <n>               # Random seed for reproducibility
- --quick                  # Reduce iterations 10x for fast testing
- --json-out <file>        # Write JSON results to file (silent mode)
- --verbose                # Show progress messages to stderr
- -h, --help               # Show help and exit
```

**JSON Config Support** (must parse both formats):
```json
- Simple config: { "alphabet": "digits", "lengths": [9,12], ... }
- Multi-suite config: { "tests": [{ "name": "...", "alphabet": "...", ... }] }
- Config file overrides CLI flags
- CLI flags provide defaults for missing config values
```

---

### Phase 2: Benchmark Setup 

**Ring Buffer Generation** (prevents compiler optimization):
```
- Generate 64 varied inputs per test length
- Use seeded random number generator (for reproducibility)
- Match alphabet character set
- Store in array/vector for rotation: inputs[i % 64]
```

**Precomputed Ciphertexts** (for decrypt benchmarks):
```
- For "dec" case: pre-encrypt all ring buffer inputs
- Store ciphertexts separately
- Use during decrypt benchmark: ciphertexts[i % 64]
```

---

### Phase 3: Warmup Phase 

**Warmup Loop** (eliminates JIT/cache effects):
```
- Run warmup iterations (default: 10,000)
- Use actual test data (not dummy values)
- Discard all warmup timing results
- Perform same operations as measured phase
- Loop through ring buffer: inputs[i % 64]
```

---

### Phase 4: Measured Phase 

**Timing and Measurement**:
```
- Use high-resolution monotonic clock (nanosecond precision)
- Start timer AFTER warmup
- Exclude setup/teardown from timing
- Record start time
- Run measured iterations loop
- Record end time
- Calculate elapsed_ns = end - start
```

**Operation Loop**:
```
- Loop through iterations (default: 100,000)
- Rotate through ring buffer: inputs[i % 64]
- Perform operation based on test case:
  - "enc": encrypt(plaintext)
  - "dec": decrypt(precomputed_ciphertext)
  - "roundtrip": decrypt(encrypt(plaintext))
```

**Anti-Optimization Checksum** (prevents dead code elimination):
```
- Initialize checksum = 0
- For each iteration:
  - XOR checksum with hash of result
  - Simple hash: fold result chars with XOR or polynomial
- Include checksum in output (proves work was done)
```

---

### Phase 5: JSON Output 

**Top-Level Structure** (MUST match exactly):
```json
{
  "metadata": { ... },
  "configuration": { ... },
  "benchmarks": [ ... ],
  "summary": { ... }
}
```

**metadata Object**:
```
- version: "1.0"
- timestamp: ISO 8601 format (e.g., "2025-09-29T21:30:00Z")
- language: "go"|"rust"|"python"|etc.
- runtime: version string (e.g., "go1.23.1", "Python 3.11.5")
- platform:
  - os: operating system name
  - arch: CPU architecture (e.g., "x86_64", "arm64")
  - cpu: CPU model string (or "unknown")
  - cores: number of CPU cores
```

**configuration Object**:
```
- seed: random seed used
- warmup_iterations: warmup count
```

**benchmarks Array** (one object per test):
```
- name: "{case}_len{length}_radix{radix}" (e.g., "enc_len9_radix10")
- test_case: "enc"|"dec"|"roundtrip"
- parameters:
  - alphabet: alphabet name
  - radix: radix value
  - length: string length
  - key_bits: key size in bits
  - key_fingerprint: first 8 hex chars of key (uppercase)
  - tweak: tweak value in hex (uppercase)
- iterations: measured iteration count
- elapsed_ns: total elapsed nanoseconds
- ns_per_op: nanoseconds per operation (elapsed_ns / iterations)
- ops_per_sec: operations per second (1e9 / ns_per_op)
- checksum: hex string (8 chars, zero-padded)
```

**summary Object**:
```
- total_tests: count of benchmark items
- total_duration_sec: total elapsed seconds (all tests)
- checksum: XOR of all individual checksums (hex, 8 chars)
```

---

### Phase 6: Output Modes 

**Default Mode** (no flags):
```
- Output JSON to stdout
- No other output
```

**Silent Mode** (`--json-out <file>`):
```
- Write JSON to file
- No stdout output
- No stderr output (unless error)
```

**Verbose Mode** (`--verbose`):
```
- Progress messages to stderr (NOT stdout)
- Format: "Running {test_name}..." or similar
- JSON output still goes to stdout (or --json-out file)
```

---

### Validation Testing 

**Test Your Implementation**:

1. **CLI Flag Test**:
```bash
# Test all flags work
./benchmark --alphabet digits --lengths 9,12 --cases enc,dec --iterations 1000 --warmup 100
```

2. **JSON Config Test**:
```bash
# Create test config
cat > test.json << 'EOF'
{
  "alphabet": "digits",
  "lengths": [9, 12],
  "cases": ["enc", "roundtrip"],
  "iterations": 1000,
  "warmup": 100
}
EOF

# Test config loading
./benchmark --config test.json --json-out results.json
```

3. **JSON Output Validation**:
```bash
# Verify output structure with jq
cat results.json | jq '.metadata.version'  # Should be "1.0"
cat results.json | jq '.benchmarks[0].test_case'  # Should be "enc" or "dec" or "roundtrip"
cat results.json | jq '.summary.total_tests'  # Should match benchmark count
```

4. **Cross-Language Compatibility**:
```bash
# Use same config across languages
./your-benchmark --config ../../shared/standard-config.json --json-out your-lang-results.json
./other-benchmark --config ../../shared/standard-config.json --json-out other-lang-results.json

# Compare output structure (should have identical keys)
diff <(jq -S 'keys' your-lang-results.json) <(jq -S 'keys' other-lang-results.json)
```

---

## Quick Reference: Minimum Working Example

Here's the absolute minimum structure for a compliant benchmark:

```
1. Parse CLI args (especially --config, --json-out, --verbose)
2. Load JSON config if --config provided (overrides defaults)
3. FOR each length in lengths:
     FOR each case in cases:
       - Generate 64 varied inputs (ring buffer)
       - Precompute ciphertexts if case == "dec"
       - Warmup loop (use actual operations)
       - Start timer
       - Measured loop (with checksum XOR)
       - Stop timer
       - Calculate ns_per_op, ops_per_sec
       - Store result
4. Build JSON output with metadata/configuration/benchmarks/summary
5. Output JSON (to stdout or --json-out file)
```

**Critical Success Factors:**
-  MUST support `--config` flag (enables cross-language testing)
-  MUST produce exact JSON structure (enables result aggregation)
-  MUST use ring buffer + checksum (prevents invalid optimization)
-  MUST use warmup phase (eliminates JIT/cache skew)

---

## Future Enhancements

### Potential Additions

1. **Statistical Analysis**
   - Min/max/median/percentiles
   - Standard deviation
   - Confidence intervals
   - Outlier detection

2. **Memory Profiling**
   - Peak memory usage
   - Allocations per operation
   - Garbage collection impact

3. **Parallelism Testing**
   - Multi-threaded performance
   - Scaling with core count
   - Lock contention measurement

4. **Comparative Mode**
   - Run multiple implementations
   - Generate side-by-side comparison
   - Relative performance percentages

5. **Historical Tracking**
   - Store results in database
   - Track performance over time
   - Regression alerts

6. **Input Length Sweeps**
   - Automatically test range of lengths
   - Generate performance curves
   - Identify complexity characteristics

## References

- [NIST SP 800-38G](https://csrc.nist.gov/publications/detail/sp/800-38g/final) - FF3 Specification
- [Go Benchmark Guidelines](https://pkg.go.dev/testing#hdr-Benchmarks)
- [Rust Criterion.rs](https://github.com/bheisler/criterion.rs) - Statistical benchmarking
- [JMH](https://github.com/openjdk/jmh) - Java Microbenchmark Harness

## Versioning

- **Version 1.0** (2025-09-29) - Initial architecture document

---

**Note:** This is a living document. Implementations should strive to follow these guidelines but may adapt based on language-specific constraints or capabilities.
