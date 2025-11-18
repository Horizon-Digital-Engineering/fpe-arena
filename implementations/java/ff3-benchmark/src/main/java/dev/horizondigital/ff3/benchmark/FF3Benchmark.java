package dev.horizondigital.ff3.benchmark;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import dev.horizondigital.ff3.FF3API;
import dev.horizondigital.ff3.FF3Alphabets;

import java.io.File;
import java.nio.file.Files;
import java.time.Instant;
import java.util.*;

/**
 * FF3 Performance Benchmark Tool
 *
 * Spec-compliant benchmark with JSON output and config file support.
 * Per docs/benchmark-architecture.md
 */
@SuppressWarnings({"java:S106", "java:S1192"}) // System.out/err and string literals are appropriate
public class FF3Benchmark {

    private static final ObjectMapper JSON = new ObjectMapper()
            .enable(SerializationFeature.INDENT_OUTPUT);

    public static void main(String[] args) {
        try {
            BenchmarkConfig config = parseArguments(args);
            List<BenchmarkResult> results = runBenchmarks(config);
            BenchmarkReport report = createReport(results, config);
            outputReport(report, config);
        } catch (Exception e) {
            System.err.println("Error: " + e.getMessage());
            if (Boolean.getBoolean("verbose")) {
                e.printStackTrace();
            }
            System.exit(1);
        }
    }

    static class BenchmarkConfig {
        String alphabet = "digits";
        int radix = 10;
        List<Integer> lengths = Arrays.asList(9, 12, 16);
        List<String> cases = Arrays.asList("enc", "dec", "roundtrip");
        int iterations = 100000;
        int warmup = 10000;
        String keyHex = "EF4359D8D580AA4F7F036D6F04FC6A94";
        String tweakHex = "D8E7920AFA330A73";
        String jsonOut = null;
        boolean verbose = false;
        int seed = 42;
    }

    static class BenchmarkResult {
        public String name;

        @JsonProperty("test_case")
        public String testCase;

        public Map<String, Object> parameters;
        public int iterations;

        @JsonProperty("elapsed_ns")
        public long elapsedNs;

        @JsonProperty("ns_per_op")
        public double nsPerOp;

        @JsonProperty("ops_per_sec")
        public double opsPerSec;

        public String checksum;
    }

    static class BenchmarkReport {
        public Map<String, Object> metadata;
        public Map<String, Object> configuration;
        public List<BenchmarkResult> benchmarks;
        public Map<String, Object> summary;
    }

    private static BenchmarkConfig parseArguments(String[] args) throws Exception {
        BenchmarkConfig config = new BenchmarkConfig();

        for (int i = 0; i < args.length; i++) {
            String arg = args[i];
            switch (arg) {
                case "--config":
                    if (i + 1 < args.length) {
                        loadConfigFile(config, args[++i]);
                    }
                    break;
                case "--alphabet":
                    if (i + 1 < args.length) config.alphabet = args[++i];
                    break;
                case "--radix":
                    if (i + 1 < args.length) config.radix = Integer.parseInt(args[++i]);
                    break;
                case "--lengths":
                    if (i + 1 < args.length) {
                        config.lengths = parseCsv(args[++i]);
                    }
                    break;
                case "--cases":
                    if (i + 1 < args.length) {
                        config.cases = Arrays.asList(args[++i].split(","));
                    }
                    break;
                case "--iterations":
                    if (i + 1 < args.length) config.iterations = Integer.parseInt(args[++i]);
                    break;
                case "--warmup":
                    if (i + 1 < args.length) config.warmup = Integer.parseInt(args[++i]);
                    break;
                case "--key":
                    if (i + 1 < args.length) config.keyHex = args[++i];
                    break;
                case "--tweak":
                    if (i + 1 < args.length) config.tweakHex = args[++i];
                    break;
                case "--quick":
                    config.iterations /= 10;
                    config.warmup /= 10;
                    break;
                case "--json-out":
                    if (i + 1 < args.length) config.jsonOut = args[++i];
                    break;
                case "--verbose":
                    config.verbose = true;
                    break;
                case "--seed":
                    if (i + 1 < args.length) config.seed = Integer.parseInt(args[++i]);
                    break;
                case "-h":
                case "--help":
                    printHelp();
                    System.exit(0);
                    break;
                default:
                    if (arg.startsWith("-")) {
                        throw new IllegalArgumentException("Unknown option: " + arg);
                    }
            }
        }

        return config;
    }

    @SuppressWarnings("unchecked")
    private static void loadConfigFile(BenchmarkConfig config, String path) throws Exception {
        String json = Files.readString(new File(path).toPath());
        Map<String, Object> data = JSON.readValue(json, Map.class);

        if (data.containsKey("alphabet")) config.alphabet = (String) data.get("alphabet");
        if (data.containsKey("radix")) config.radix = (Integer) data.get("radix");
        if (data.containsKey("lengths")) config.lengths = (List<Integer>) data.get("lengths");
        if (data.containsKey("cases")) config.cases = (List<String>) data.get("cases");
        if (data.containsKey("iterations")) config.iterations = (Integer) data.get("iterations");
        if (data.containsKey("warmup")) config.warmup = (Integer) data.get("warmup");
        if (data.containsKey("key")) config.keyHex = (String) data.get("key");
        if (data.containsKey("tweak")) config.tweakHex = (String) data.get("tweak");
        if (data.containsKey("seed")) config.seed = (Integer) data.get("seed");
    }

    private static List<Integer> parseCsv(String csv) {
        List<Integer> result = new ArrayList<>();
        for (String part : csv.split(",")) {
            result.add(Integer.parseInt(part.trim()));
        }
        return result;
    }

    private static List<BenchmarkResult> runBenchmarks(BenchmarkConfig config) throws Exception {
        byte[] key = FF3API.hexToBytes(config.keyHex);
        byte[] tweak = FF3API.hexToBytes(config.tweakHex);

        String alphabet = getAlphabet(config.alphabet);
        FF3API cipher = new FF3API(key, tweak, alphabet);

        List<BenchmarkResult> results = new ArrayList<>();
        Random rng = new Random(config.seed);

        for (int length : config.lengths) {
            for (String testCase : config.cases) {
                if (config.verbose) {
                    System.err.printf("Running %s_len%d_%s...", testCase, length, config.alphabet);
                }

                BenchmarkResult result = runSingleBenchmark(
                    cipher, testCase, length, alphabet, config, rng
                );
                results.add(result);

                if (config.verbose) {
                    System.err.printf(" done (%.0f ns/op, %.0f ops/sec)%n",
                        result.nsPerOp, result.opsPerSec);
                }
            }
        }

        return results;
    }

    private static BenchmarkResult runSingleBenchmark(
            FF3API cipher, String testCase, int length, String alphabet,
            BenchmarkConfig config, Random rng) throws Exception {

        // Generate test inputs
        List<String> inputs = generateInputs(64, length, alphabet, rng);

        // Pre-compute ciphertexts for decrypt benchmarks (like Go does)
        List<String> precomputedCTs = new ArrayList<>();
        if (testCase.equals("dec")) {
            for (String pt : inputs) {
                precomputedCTs.add(cipher.encrypt(pt));
            }
        }

        // Warmup
        for (int i = 0; i < config.warmup; i++) {
            String input = inputs.get(i % inputs.size());
            switch (testCase) {
                case "enc":
                    cipher.encrypt(input);
                    break;
                case "dec":
                    String ct = precomputedCTs.get(i % precomputedCTs.size());
                    cipher.decrypt(ct);
                    break;
                case "roundtrip":
                    String ct2 = cipher.encrypt(input);
                    cipher.decrypt(ct2);
                    break;
            }
        }

        // Measured phase
        long startNs = System.nanoTime();
        int checksum = 0;

        for (int i = 0; i < config.iterations; i++) {
            String input = inputs.get(i % inputs.size());
            String output;

            switch (testCase) {
                case "enc":
                    output = cipher.encrypt(input);
                    checksum ^= output.hashCode();
                    break;
                case "dec":
                    String ct = precomputedCTs.get(i % precomputedCTs.size());
                    output = cipher.decrypt(ct);
                    checksum ^= output.hashCode();
                    break;
                case "roundtrip":
                    String ct2 = cipher.encrypt(input);
                    output = cipher.decrypt(ct2);
                    checksum ^= output.hashCode();
                    break;
            }
        }

        long elapsedNs = System.nanoTime() - startNs;

        BenchmarkResult result = new BenchmarkResult();
        result.name = String.format("%s_len%d_radix%d", testCase, length, alphabet.length());
        result.testCase = testCase;
        result.parameters = new HashMap<>();
        result.parameters.put("alphabet", config.alphabet);
        result.parameters.put("radix", alphabet.length());
        result.parameters.put("length", length);
        result.parameters.put("key_bits", config.keyHex.length() / 2 * 8);
        result.parameters.put("key_fingerprint", config.keyHex.substring(0, Math.min(8, config.keyHex.length())));
        result.parameters.put("tweak", config.tweakHex);
        result.iterations = config.iterations;
        result.elapsedNs = elapsedNs;
        result.nsPerOp = (double) elapsedNs / config.iterations;
        result.opsPerSec = 1_000_000_000.0 / result.nsPerOp;
        result.checksum = String.format("%08x", checksum);

        return result;
    }

    private static List<String> generateInputs(int count, int length, String alphabet, Random rng) {
        List<String> inputs = new ArrayList<>();
        for (int i = 0; i < count; i++) {
            StringBuilder sb = new StringBuilder();
            for (int j = 0; j < length; j++) {
                sb.append(alphabet.charAt(rng.nextInt(alphabet.length())));
            }
            inputs.add(sb.toString());
        }
        return inputs;
    }

    private static BenchmarkReport createReport(List<BenchmarkResult> results, BenchmarkConfig config) {
        BenchmarkReport report = new BenchmarkReport();

        // Metadata
        report.metadata = new HashMap<>();
        report.metadata.put("version", "1.0");
        report.metadata.put("timestamp", Instant.now().toString());
        report.metadata.put("language", "java");
        report.metadata.put("runtime", System.getProperty("java.version"));

        Map<String, Object> platform = new HashMap<>();
        platform.put("os", System.getProperty("os.name"));
        platform.put("arch", System.getProperty("os.arch"));
        platform.put("cpu", System.getenv().getOrDefault("PROCESSOR_IDENTIFIER", "unknown"));
        platform.put("cores", Runtime.getRuntime().availableProcessors());
        report.metadata.put("platform", platform);

        // Configuration
        report.configuration = new HashMap<>();
        report.configuration.put("seed", config.seed);
        report.configuration.put("warmup_iterations", config.warmup);

        // Benchmarks
        report.benchmarks = results;

        // Summary
        report.summary = new HashMap<>();
        report.summary.put("total_tests", results.size());
        long totalDurationNs = results.stream().mapToLong(r -> r.elapsedNs).sum();
        report.summary.put("total_duration_sec", totalDurationNs / 1_000_000_000.0);

        int combinedChecksum = 0;
        for (BenchmarkResult r : results) {
            combinedChecksum ^= Integer.parseUnsignedInt(r.checksum, 16);
        }
        report.summary.put("checksum", String.format("%08x", combinedChecksum));

        return report;
    }

    private static void outputReport(BenchmarkReport report, BenchmarkConfig config) throws Exception {
        String json = JSON.writeValueAsString(report);

        if (config.jsonOut != null) {
            Files.writeString(new File(config.jsonOut).toPath(), json);
        } else {
            System.out.println(json);
        }
    }

    private static String getAlphabet(String name) {
        return switch (name) {
            case "digits" -> FF3Alphabets.DIGITS;
            case "hex" -> FF3Alphabets.HEX_LOWER;
            case "hex-lower" -> FF3Alphabets.HEX_LOWER;
            case "hex-upper" -> FF3Alphabets.HEX_UPPER;
            case "base36" -> FF3Alphabets.BASE36_LOWER;
            case "base36-lower" -> FF3Alphabets.BASE36_LOWER;
            case "base36-upper" -> FF3Alphabets.BASE36_UPPER;
            case "base62" -> FF3Alphabets.BASE62;
            default -> throw new IllegalArgumentException("Unknown alphabet: " + name);
        };
    }

    private static void printHelp() {
        System.out.println("FF3 Performance Benchmark Tool");
        System.out.println();
        System.out.println("Usage: ff3-benchmark [OPTIONS]");
        System.out.println();
        System.out.println("Options:");
        System.out.println("  --config <file>        Read configuration from JSON file");
        System.out.println("  --alphabet <name>      Alphabet: digits|hex|base36|base62 (default: digits)");
        System.out.println("  --radix <n>            Radix 2..62 for custom alphabets (default: 10)");
        System.out.println("  --lengths <csv>        Comma-separated lengths (default: 9,12,16)");
        System.out.println("  --cases <csv>          Test cases: enc,dec,roundtrip (default: all)");
        System.out.println("  --iterations <n>       Measured iterations (default: 100000)");
        System.out.println("  --warmup <n>           Warmup iterations (default: 10000)");
        System.out.println("  --key <hex>            Hex-encoded key (default: standard)");
        System.out.println("  --tweak <hex>          Hex-encoded tweak (default: standard)");
        System.out.println("  --quick                Reduce iterations 10x for fast testing");
        System.out.println("  --json-out <file>      Write JSON results to file (silent mode)");
        System.out.println("  --verbose              Show progress messages");
        System.out.println("  --seed <n>             Random seed for reproducibility (default: 42)");
        System.out.println("  -h, --help             Show this help message");
        System.out.println();
        System.out.println("Examples:");
        System.out.println("  ff3-benchmark --quick --verbose");
        System.out.println("  ff3-benchmark --lengths 9,12,16 --json-out results.json");
        System.out.println("  ff3-benchmark --config benchmark.json --json-out results.json");
    }
}
