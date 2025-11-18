# Docker Usage Guide

## Resource Requirements

### Build Time Expectations

**Total build time**: ~20-40 minutes (depending on hardware)

Breakdown by phase:
- **Base image + toolchains**: ~10-15 minutes
  - Rust, Go, .NET, Java, C++: ~5 minutes
  - Haskell (GHCup), Swift (swiftly): ~3-5 minutes
  - Erlang, Ada/SPARK (Alire): ~2-3 minutes
- **Compile all 11 implementations**: ~8-12 minutes
  - Rust, Go, .NET, Java: ~2-3 minutes each
  - C++, Haskell, Swift: ~1-2 minutes each
  - Ada (GNAT), Erlang, Python, JavaScript: <1 minute each
- **Run test suites**: ~2-5 minutes

**Hardware recommendations**:
- **Minimum**: 4 CPU cores, 8GB RAM, 15GB disk
- **Recommended**: 8+ CPU cores, 16GB RAM, 20GB disk
- **SSD strongly recommended** for faster compilation

### Disk Space Usage

- **Docker image size**: ~8-10 GB (all toolchains + compiled binaries)
- **Build cache**: ~3-5 GB (intermediate layers)
- **Total disk space needed**: ~15-20 GB

Breakdown:
- Base Ubuntu + system packages: ~500 MB
- Language toolchains: ~6-7 GB
  - .NET SDK: ~600 MB
  - Java JDK: ~400 MB
  - Haskell GHC: ~1.5 GB
  - Swift: ~1.2 GB
  - Rust: ~800 MB
  - Others: ~2 GB combined
- Compiled implementations: ~500 MB - 1 GB
- Test vectors + dependencies: ~200 MB

### Memory Requirements

During build:
- **Peak memory usage**: 4-6 GB
- **Concurrent compilation** may spike to 8 GB with 8+ cores
- **Recommended**: 8-16 GB RAM for smooth build experience

During benchmarks:
- **Typical usage**: 500 MB - 1 GB
- **All 11 implementations running**: <2 GB

## Building the Image

Build the multi-language FF3 development environment:

```bash
docker build -t ff3-arena .
```

**Expected output**:
```
[+] Building 1234.5s (45/45) FINISHED
 => [internal] load build definition from Dockerfile
 => [stage-0 1/38] FROM docker.io/library/ubuntu:24.04
 ...
 => exporting to image
 => => writing image sha256:abc123...
 => => naming to docker.io/library/ff3-arena
```

This will:
- Install all 11 language toolchains (Rust, Go, .NET, Java, C++, Haskell, JavaScript, Python, Swift, Erlang, Ada/SPARK)
- Build and test all implementations
- Set up benchmark runner scripts

**Optimization tips**:
- Use `--build-arg BUILDKIT_INLINE_CACHE=1` for better layer caching
- Run `docker builder prune` periodically to clean up build cache
- Use BuildKit for parallel stage builds: `DOCKER_BUILDKIT=1 docker build`

## Running Benchmarks

### Quick Test (5 fastest implementations)

```bash
docker run --rm ff3-arena run-quick-benchmarks.sh
```

### All Implementations

```bash
docker run --rm ff3-arena run-all-benchmarks.sh
```

### Save Results to Host

```bash
# Create output directory on host
mkdir -p ./benchmark-results

# Run benchmarks and save results
docker run --rm -v $(pwd)/benchmark-results:/home/dev/workspace/artifacts ff3-arena run-all-benchmarks.sh
```

Results will be saved to `./benchmark-results/*.json` on your host machine.

### Custom Configuration

```bash
# Use custom config file
docker run --rm \
  -v $(pwd)/my-config.json:/home/dev/workspace/my-config.json \
  -v $(pwd)/results:/home/dev/workspace/artifacts \
  ff3-arena run-all-benchmarks.sh --config my-config.json
```

### Interactive Shell

```bash
docker run -it ff3-arena
```

Inside the container, you can run:
```bash
# Quick test
run-quick-benchmarks.sh

# Full benchmark suite
run-all-benchmarks.sh

# Custom options
run-all-benchmarks.sh --help
run-all-benchmarks.sh --config shared/benchmarks/benchmark-config.json --output /tmp/results

# View results
cat artifacts/rust-bench.json
ls -lh artifacts/
```

## Output Format

All benchmarks output JSON to `artifacts/<language>-bench.json`:

```json
{
  "metadata": {
    "version": "1.0",
    "timestamp": "2025-10-02T...",
    "language": "rust",
    "runtime": "rustc 1.75.0",
    "platform": { "os": "linux", "arch": "x86_64", "cpu": "...", "cores": 8 }
  },
  "configuration": {
    "seed": 42,
    "warmup_iterations": 1000
  },
  "benchmarks": [ /* detailed results */ ],
  "summary": {
    "total_tests": 9,
    "total_duration_sec": 2.5,
    "checksum": "..."
  }
}
```

## Performance Comparison

After running benchmarks, you can compare performance across languages:

```bash
# Extract ops/sec for all languages
for f in artifacts/*-bench.json; do
  lang=$(basename $f -bench.json)
  ops=$(grep -o '"ops_per_sec": [0-9]*' $f | head -1 | grep -o '[0-9]*')
  echo "$lang: $ops ops/sec"
done | sort -t: -k2 -rn
```

## Environment Details

The container includes:
- **Rust** 1.75+
- **Go** 1.22.5
- **.NET** 8.0
- **Java** 25 (OpenJDK)
- **C++** (GCC 11)
- **Haskell** (GHC via GHCup)
- **JavaScript** (Node.js LTS via nvm)
- **Python** 3.10
- **Swift** (latest via swiftly)
- **Erlang** 24+
- **Ada/SPARK** (GNAT via Alire)

All implementations are pre-built and tested!
