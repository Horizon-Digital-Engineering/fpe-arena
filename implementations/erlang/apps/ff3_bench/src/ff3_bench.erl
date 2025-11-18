%% FF3 Performance Benchmark Tool Main Module

-module(ff3_bench).
-export([main/1]).

main(Args) ->
    %% Parse command-line arguments
    Config = parse_args(Args),

    %% Check for help or version
    case maps:get(help, Config, false) of
        true ->
            print_help(),
            halt(0);
        false -> ok
    end,

    case maps:get(version, Config, false) of
        true ->
            io:format("FF3 Benchmark Tool v1.0.0~n"),
            halt(0);
        false -> ok
    end,

    %% Load config from file if specified
    FinalConfig = case maps:get(config_file, Config, undefined) of
        undefined -> Config;
        ConfigFile -> load_json_config(ConfigFile, Config)
    end,

    %% Extract configuration values
    Alphabet = maps:get(alphabet, FinalConfig, "digits"),
    Lengths = maps:get(lengths, FinalConfig, [9, 12, 16]),
    Cases = maps:get(cases, FinalConfig, ["enc", "dec", "roundtrip"]),
    Iterations = maps:get(iterations, FinalConfig, 100000),
    Warmup = maps:get(warmup, FinalConfig, 10000),
    KeyHex = maps:get(key, FinalConfig, "EF4359D8D580AA4F7F036D6F04FC6A94"),
    TweakHex = maps:get(tweak, FinalConfig, "D8E7920AFA330A73"),
    Quick = maps:get(quick, FinalConfig, false),
    Verbose = maps:get(verbose, FinalConfig, false),
    JsonOut = maps:get(json_out, FinalConfig, undefined),
    Seed = maps:get(seed, FinalConfig, 42),

    %% Apply quick mode
    FinalIterations = case Quick of
        true -> Iterations div 10;
        false -> Iterations
    end,
    FinalWarmup = case Quick of
        true -> Warmup div 10;
        false -> Warmup
    end,

    %% Convert hex strings to binary
    Key = ff3_api:hex_to_bytes(KeyHex),
    Tweak = ff3_api:hex_to_bytes(TweakHex),

    %% Get alphabet object
    AlphabetObj = get_alphabet(Alphabet),
    Radix = length(AlphabetObj),

    %% Initialize random seed
    rand:seed(default, Seed),

    %% Run benchmarks
    StartTime = erlang:system_time(nanosecond),
    Benchmarks = run_benchmarks(AlphabetObj, Radix, Alphabet, Lengths, Cases,
                                Key, Tweak, FinalIterations, FinalWarmup, Verbose),
    EndTime = erlang:system_time(nanosecond),
    TotalDuration = (EndTime - StartTime) / 1000000000.0,

    %% Output results
    output_results(Benchmarks, TotalDuration, Alphabet, Radix, KeyHex, TweakHex,
                  FinalIterations, FinalWarmup, Seed, JsonOut, Verbose).

benchmark_alphabet(Name, Alphabet, TestString, Key, Tweak, Iterations) ->
    io:format("📊 Benchmarking ~s...~n", [Name]),

    %% Warm up
    ff3_api:encrypt(TestString, Key, Tweak, Alphabet),
    ff3_api:decrypt(TestString, Key, Tweak, Alphabet),

    %% Benchmark encryption
    EncryptTime = time_operation(fun() ->
        ff3_api:encrypt(TestString, Key, Tweak, Alphabet)
    end, Iterations),

    %% Benchmark decryption
    Ciphertext = ff3_api:encrypt(TestString, Key, Tweak, Alphabet),
    DecryptTime = time_operation(fun() ->
        ff3_api:decrypt(Ciphertext, Key, Tweak, Alphabet)
    end, Iterations),

    %% Benchmark round-trip
    RoundTripTime = time_operation(fun() ->
        C = ff3_api:encrypt(TestString, Key, Tweak, Alphabet),
        ff3_api:decrypt(C, Key, Tweak, Alphabet)
    end, Iterations),

    %% Calculate statistics
    EncryptNsPerOp = (EncryptTime * 1000) div Iterations,  % Convert to nanoseconds per op
    DecryptNsPerOp = (DecryptTime * 1000) div Iterations,
    RoundTripNsPerOp = (RoundTripTime * 1000) div Iterations,

    EncryptOpsPerSec = case EncryptNsPerOp of
        0 -> infinity;
        _ -> 1000000 div (EncryptNsPerOp div 1000)  % Convert back to ops/sec
    end,

    DecryptOpsPerSec = case DecryptNsPerOp of
        0 -> infinity;
        _ -> 1000000 div (DecryptNsPerOp div 1000)
    end,

    RoundTripOpsPerSec = case RoundTripNsPerOp of
        0 -> infinity;
        _ -> 1000000 div (RoundTripNsPerOp div 1000)
    end,

    io:format("   Encrypt:     ~p ns/op (~p ops/sec)~n", [EncryptNsPerOp, EncryptOpsPerSec]),
    io:format("   Decrypt:     ~p ns/op (~p ops/sec)~n", [DecryptNsPerOp, DecryptOpsPerSec]),
    io:format("   Round-trip:  ~p ns/op (~p ops/sec)~n~n", [RoundTripNsPerOp, RoundTripOpsPerSec]).

time_operation(Fun, Iterations) ->
    StartTime = erlang:monotonic_time(microsecond),
    run_iterations(Fun, Iterations),
    EndTime = erlang:monotonic_time(microsecond),
    EndTime - StartTime.

run_iterations(_Fun, 0) -> ok;
run_iterations(Fun, N) ->
    Fun(),
    run_iterations(Fun, N - 1).

%% Parse command-line arguments
parse_args(Args) ->
    parse_args(Args, #{}).

parse_args([], Config) -> Config;
parse_args(["-h" | _], Config) -> Config#{help => true};
parse_args(["--help" | _], Config) -> Config#{help => true};
parse_args(["-v" | _], Config) -> Config#{version => true};
parse_args(["--version" | _], Config) -> Config#{version => true};

parse_args(["--config", File | Rest], Config) ->
    parse_args(Rest, Config#{config_file => File});
parse_args(["--alphabet", Alpha | Rest], Config) ->
    parse_args(Rest, Config#{alphabet => Alpha});
parse_args(["--radix", RadixStr | Rest], Config) ->
    parse_args(Rest, Config#{radix => list_to_integer(RadixStr)});
parse_args(["--lengths", LengthsStr | Rest], Config) ->
    Lengths = [list_to_integer(L) || L <- string:tokens(LengthsStr, ",")],
    parse_args(Rest, Config#{lengths => Lengths});
parse_args(["--cases", CasesStr | Rest], Config) ->
    Cases = string:tokens(CasesStr, ","),
    parse_args(Rest, Config#{cases => Cases});
parse_args(["--iterations", N | Rest], Config) ->
    parse_args(Rest, Config#{iterations => list_to_integer(N)});
parse_args(["--warmup", N | Rest], Config) ->
    parse_args(Rest, Config#{warmup => list_to_integer(N)});
parse_args(["--key", KeyHex | Rest], Config) ->
    parse_args(Rest, Config#{key => KeyHex});
parse_args(["--tweak", TweakHex | Rest], Config) ->
    parse_args(Rest, Config#{tweak => TweakHex});
parse_args(["--quick" | Rest], Config) ->
    parse_args(Rest, Config#{quick => true});
parse_args(["--json-out", File | Rest], Config) ->
    parse_args(Rest, Config#{json_out => File});
parse_args(["--verbose" | Rest], Config) ->
    parse_args(Rest, Config#{verbose => true});
parse_args(["--seed", SeedStr | Rest], Config) ->
    parse_args(Rest, Config#{seed => list_to_integer(SeedStr)});
parse_args([_ | Rest], Config) ->
    parse_args(Rest, Config).

%% Load JSON config file
load_json_config(File, DefaultConfig) ->
    case file:read_file(File) of
        {ok, Content} ->
            %% Parse JSON (using simple pattern matching for our specific format)
            %% This is a simplified parser for the specific config format we need
            JsonStr = binary_to_list(Content),
            parse_simple_json(JsonStr, DefaultConfig);
        {error, _} ->
            io:format(standard_error, "Error: Cannot read config file: ~s~n", [File]),
            halt(1)
    end.

%% Simple JSON parser for our specific config format
parse_simple_json(JsonStr, Config) ->
    %% Extract values using regex patterns
    Config1 = case re:run(JsonStr, "\"alphabet\"\\s*:\\s*\"([^\"]+)\"", [{capture, [1], list}]) of
        {match, [Alpha]} -> Config#{alphabet => Alpha};
        _ -> Config
    end,
    Config2 = case re:run(JsonStr, "\"key\"\\s*:\\s*\"([^\"]+)\"", [{capture, [1], list}]) of
        {match, [Key]} -> Config1#{key => Key};
        _ -> Config1
    end,
    Config3 = case re:run(JsonStr, "\"tweak\"\\s*:\\s*\"([^\"]+)\"", [{capture, [1], list}]) of
        {match, [Tweak]} -> Config2#{tweak => Tweak};
        _ -> Config2
    end,
    Config4 = case re:run(JsonStr, "\"iterations\"\\s*:\\s*([0-9]+)", [{capture, [1], list}]) of
        {match, [Iter]} -> Config3#{iterations => list_to_integer(Iter)};
        _ -> Config3
    end,
    Config5 = case re:run(JsonStr, "\"warmup\"\\s*:\\s*([0-9]+)", [{capture, [1], list}]) of
        {match, [Warm]} -> Config4#{warmup => list_to_integer(Warm)};
        _ -> Config4
    end,
    Config6 = case re:run(JsonStr, "\"seed\"\\s*:\\s*([0-9]+)", [{capture, [1], list}]) of
        {match, [Seed]} -> Config5#{seed => list_to_integer(Seed)};
        _ -> Config5
    end,
    %% Parse lengths array
    Config7 = case re:run(JsonStr, "\"lengths\"\\s*:\\s*\\[([^\\]]+)\\]", [{capture, [1], list}]) of
        {match, [LengthsStr]} ->
            Lengths = [list_to_integer(string:strip(L)) ||
                      L <- string:tokens(LengthsStr, ",")],
            Config6#{lengths => Lengths};
        _ -> Config6
    end,
    %% Parse cases array
    case re:run(JsonStr, "\"cases\"\\s*:\\s*\\[([^\\]]+)\\]", [{capture, [1], list}]) of
        {match, [CasesStr]} ->
            %% Parse array of quoted strings properly
            {ok, Tokens, _} = erl_scan:string("[" ++ CasesStr ++ "]."),
            {ok, CasesList} = erl_parse:parse_term(Tokens),
            Config7#{cases => CasesList};
        _ -> Config7
    end.

%% Print help message
print_help() ->
    io:format("FF3 Performance Benchmark Tool v1.0.0~n~n"),
    io:format("Usage: ff3-bench [options]~n~n"),
    io:format("Options:~n"),
    io:format("  --config FILE         Load configuration from JSON file~n"),
    io:format("  --alphabet NAME       Alphabet to use (digits, hex, base36, base62)~n"),
    io:format("  --radix N             Radix (2-62)~n"),
    io:format("  --lengths CSV         Comma-separated input lengths (e.g., \"9,12,16\")~n"),
    io:format("  --cases CSV           Comma-separated cases (enc,dec,roundtrip)~n"),
    io:format("  --iterations N        Number of iterations (default: 100000)~n"),
    io:format("  --warmup N            Warmup iterations (default: 10000)~n"),
    io:format("  --key HEX             Encryption key (hex string)~n"),
    io:format("  --tweak HEX           Tweak value (hex string)~n"),
    io:format("  --seed N              Random seed for input generation~n"),
    io:format("  --verbose             Show progress during benchmarks~n"),
    io:format("  --json-out FILE       Write JSON results to file~n"),
    io:format("  --quick               Quick benchmark (fewer iterations)~n"),
    io:format("  -h, --help            Show this help message~n"),
    io:format("  -v, --version         Show version~n~n"),
    io:format("⚠️  FF3 was withdrawn by NIST due to security vulnerabilities.~n"),
    io:format("   This tool is for educational and research purposes only.~n").

%% Get alphabet object based on name
get_alphabet("digits") -> ff3_alphabets:digits_alphabet();
get_alphabet("hex") -> ff3_alphabets:hex_lower_alphabet();
get_alphabet("hex_lower") -> ff3_alphabets:hex_lower_alphabet();
get_alphabet("hex_upper") -> ff3_alphabets:hex_upper_alphabet();
get_alphabet("base36") -> ff3_alphabets:base36_lower_alphabet();
get_alphabet("base36_lower") -> ff3_alphabets:base36_lower_alphabet();
get_alphabet("base36_upper") -> ff3_alphabets:base36_upper_alphabet();
get_alphabet("base62") -> ff3_alphabets:base62_alphabet();
get_alphabet(_) -> ff3_alphabets:digits_alphabet().

%% Generate input strings
generate_inputs(Alphabet, Length, Count, _Seed) ->
    [generate_input(Alphabet, Length, I) || I <- lists:seq(0, Count-1)].

generate_input(Alphabet, Length, Index) ->
    AlphabetList = Alphabet,
    AlphabetLen = length(AlphabetList),
    generate_input_chars(AlphabetList, AlphabetLen, Length, Index, []).

generate_input_chars(_, _, 0, _, Acc) ->
    lists:reverse(Acc);
generate_input_chars(Alphabet, AlphabetLen, Length, Index, Acc) ->
    CharIdx = (Index * 31 + Length * 17) rem AlphabetLen,
    Char = lists:nth(CharIdx + 1, Alphabet),
    generate_input_chars(Alphabet, AlphabetLen, Length - 1, Index + 1, [Char | Acc]).

%% Run benchmarks
run_benchmarks(Alphabet, Radix, AlphabetName, Lengths, Cases, Key, Tweak,
              Iterations, Warmup, Verbose) ->
    lists:flatten([
        run_length_benchmarks(Alphabet, Radix, AlphabetName, Length, Cases,
                            Key, Tweak, Iterations, Warmup, Verbose)
        || Length <- Lengths
    ]).

run_length_benchmarks(Alphabet, Radix, AlphabetName, Length, Cases, Key, Tweak,
                     Iterations, Warmup, Verbose) ->
    %% Generate inputs
    Inputs = generate_inputs(Alphabet, Length, 64, 42),

    %% Warmup
    case Verbose of
        true -> io:format("📊 Warmup for length ~p...~n", [Length]);
        false -> ok
    end,
    run_warmup(Inputs, Key, Tweak, Alphabet, Warmup),

    %% Run benchmarks for each case
    [run_case_benchmark(Case, Alphabet, Radix, AlphabetName, Length, Inputs,
                       Key, Tweak, Iterations, Verbose)
     || Case <- Cases].

run_warmup(_, _, _, _, 0) -> ok;
run_warmup([Input | Rest], Key, Tweak, Alphabet, N) ->
    ff3_api:encrypt(Input, Key, Tweak, Alphabet),
    run_warmup(Rest ++ [Input], Key, Tweak, Alphabet, N - 1).

run_case_benchmark(Case, Alphabet, Radix, AlphabetName, Length, Inputs,
                  Key, Tweak, Iterations, Verbose) ->
    Name = io_lib:format("~s_len~p_radix~p", [Case, Length, Radix]),

    case Verbose of
        true -> io:format("📊 Benchmarking ~s...~n", [Name]);
        false -> ok
    end,

    %% Precompute ciphertexts for decrypt
    Ciphertexts = [ff3_api:encrypt(Input, Key, Tweak, Alphabet) || Input <- Inputs],

    %% Run benchmark
    {ElapsedNs, Checksum} = case Case of
        "enc" -> benchmark_encrypt(Inputs, Key, Tweak, Alphabet, Iterations);
        "dec" -> benchmark_decrypt(Ciphertexts, Key, Tweak, Alphabet, Iterations);
        "roundtrip" -> benchmark_roundtrip(Inputs, Key, Tweak, Alphabet, Iterations);
        _ -> {0, 0}
    end,

    NsPerOp = ElapsedNs div Iterations,
    OpsPerSec = case NsPerOp of
        0 -> 0;
        _ -> 1000000000 div NsPerOp
    end,

    case Verbose of
        true -> io:format("   ~p ns/op (~p ops/sec)~n", [NsPerOp, OpsPerSec]);
        false -> ok
    end,

    #{name => lists:flatten(Name),
      test_case => Case,
      alphabet => AlphabetName,
      radix => Radix,
      length => Length,
      iterations => Iterations,
      elapsed_ns => ElapsedNs,
      ns_per_op => NsPerOp,
      ops_per_sec => OpsPerSec,
      checksum => integer_to_list(Checksum, 16)}.

benchmark_encrypt(Inputs, Key, Tweak, Alphabet, Iterations) ->
    StartTime = erlang:monotonic_time(nanosecond),
    Checksum = benchmark_encrypt_loop(Inputs, Key, Tweak, Alphabet, Iterations, 0, 0),
    EndTime = erlang:monotonic_time(nanosecond),
    {EndTime - StartTime, Checksum}.

benchmark_encrypt_loop(_, _, _, _, 0, _, Checksum) -> Checksum;
benchmark_encrypt_loop(Inputs, Key, Tweak, Alphabet, N, Idx, Checksum) ->
    Input = lists:nth((Idx rem length(Inputs)) + 1, Inputs),
    Result = ff3_api:encrypt(Input, Key, Tweak, Alphabet),
    NewChecksum = lists:foldl(fun(C, Acc) -> Acc bxor C end, Checksum, Result),
    benchmark_encrypt_loop(Inputs, Key, Tweak, Alphabet, N - 1, Idx + 1, NewChecksum).

benchmark_decrypt(Ciphertexts, Key, Tweak, Alphabet, Iterations) ->
    StartTime = erlang:monotonic_time(nanosecond),
    Checksum = benchmark_decrypt_loop(Ciphertexts, Key, Tweak, Alphabet, Iterations, 0, 0),
    EndTime = erlang:monotonic_time(nanosecond),
    {EndTime - StartTime, Checksum}.

benchmark_decrypt_loop(_, _, _, _, 0, _, Checksum) -> Checksum;
benchmark_decrypt_loop(Ciphertexts, Key, Tweak, Alphabet, N, Idx, Checksum) ->
    Ciphertext = lists:nth((Idx rem length(Ciphertexts)) + 1, Ciphertexts),
    Result = ff3_api:decrypt(Ciphertext, Key, Tweak, Alphabet),
    NewChecksum = lists:foldl(fun(C, Acc) -> Acc bxor C end, Checksum, Result),
    benchmark_decrypt_loop(Ciphertexts, Key, Tweak, Alphabet, N - 1, Idx + 1, NewChecksum).

benchmark_roundtrip(Inputs, Key, Tweak, Alphabet, Iterations) ->
    StartTime = erlang:monotonic_time(nanosecond),
    Checksum = benchmark_roundtrip_loop(Inputs, Key, Tweak, Alphabet, Iterations, 0, 0),
    EndTime = erlang:monotonic_time(nanosecond),
    {EndTime - StartTime, Checksum}.

benchmark_roundtrip_loop(_, _, _, _, 0, _, Checksum) -> Checksum;
benchmark_roundtrip_loop(Inputs, Key, Tweak, Alphabet, N, Idx, Checksum) ->
    Input = lists:nth((Idx rem length(Inputs)) + 1, Inputs),
    Ciphertext = ff3_api:encrypt(Input, Key, Tweak, Alphabet),
    Result = ff3_api:decrypt(Ciphertext, Key, Tweak, Alphabet),
    NewChecksum = lists:foldl(fun(C, Acc) -> Acc bxor C end, Checksum, Result),
    benchmark_roundtrip_loop(Inputs, Key, Tweak, Alphabet, N - 1, Idx + 1, NewChecksum).

%% Output results
output_results(Benchmarks, TotalDuration, Alphabet, Radix, KeyHex, TweakHex,
              Iterations, Warmup, Seed, JsonOut, Verbose) ->
    %% Calculate combined checksum
    CombinedChecksum = lists:foldl(
        fun(#{checksum := CS}, Acc) ->
            list_to_integer(CS, 16) bxor Acc
        end, 0, Benchmarks),

    %% Create output structure
    Output = #{
        metadata => #{
            version => "1.0",
            timestamp => format_timestamp(),
            language => "erlang",
            runtime => "Erlang/OTP " ++ erlang:system_info(otp_release),
            platform => #{
                os => format_os_type(erlang:system_info(os_type)),
                arch => erlang:system_info(machine),
                cpu => "unknown",
                cores => erlang:system_info(logical_processors_available)
            }
        },
        configuration => #{
            seed => Seed,
            warmup_iterations => Warmup
        },
        benchmarks => [format_benchmark(B, KeyHex, TweakHex) || B <- Benchmarks],
        summary => #{
            total_tests => length(Benchmarks),
            total_duration_sec => TotalDuration,
            checksum => integer_to_list(CombinedChecksum, 16)
        }
    },

    %% Output JSON
    JsonStr = encode_json_pretty(Output, 0),

    case JsonOut of
        undefined ->
            io:format("~s~n", [JsonStr]);
        File ->
            file:write_file(File, JsonStr),
            case Verbose of
                true -> io:format("✅ Results written to ~s~n", [File]);
                false -> ok
            end
    end,

    case Verbose of
        true ->
            io:format("~n⚠️  FF3 Format Preserving Encryption Library v1.0.0~n~n"),
            io:format("    FF3 was WITHDRAWN by NIST due to security vulnerabilities.~n"),
            io:format("    This implementation is for EDUCATIONAL and RESEARCH purposes only.~n~n"),
            io:format("    DO NOT use in production systems.~n");
        false -> ok
    end.

format_benchmark(#{name := Name, test_case := Case, alphabet := Alpha,
                  radix := Radix, length := Length, iterations := Iter,
                  elapsed_ns := Elapsed, ns_per_op := NsPerOp,
                  ops_per_sec := OpsPerSec, checksum := CS},
                KeyHex, TweakHex) ->
    #{
        name => Name,
        test_case => Case,
        parameters => #{
            alphabet => Alpha,
            radix => Radix,
            length => Length,
            key_bits => length(KeyHex) * 4,
            key_fingerprint => string:substr(KeyHex, 1, 8),
            tweak => TweakHex
        },
        iterations => Iter,
        elapsed_ns => Elapsed,
        ns_per_op => NsPerOp,
        ops_per_sec => OpsPerSec,
        checksum => CS
    }.

format_timestamp() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:universal_time(),
    lists:flatten(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                  [Year, Month, Day, Hour, Minute, Second])).

format_os_type({Family, Name}) ->
    atom_to_list(Family) ++ "/" ++ atom_to_list(Name);
format_os_type(OS) when is_atom(OS) ->
    atom_to_list(OS).

%% Simple JSON encoder for our specific structure
encode_json(Map) when is_map(Map) ->
    "{" ++ string:join([encode_json_pair(K, V) || {K, V} <- maps:to_list(Map)], ",") ++ "}";
encode_json(List) when is_list(List) ->
    case io_lib:printable_list(List) of
        true -> "\"" ++ List ++ "\"";
        false -> "[" ++ string:join([encode_json(E) || E <- List], ",") ++ "]"
    end;
encode_json(Atom) when is_atom(Atom) ->
    "\"" ++ atom_to_list(Atom) ++ "\"";
encode_json(Int) when is_integer(Int) ->
    integer_to_list(Int);
encode_json(Float) when is_float(Float) ->
    io_lib:format("~.6f", [Float]);
encode_json(Binary) when is_binary(Binary) ->
    "\"" ++ binary_to_list(Binary) ++ "\"".

encode_json_pair(Key, Value) ->
    "\"" ++ atom_to_list(Key) ++ "\":" ++ encode_json(Value).

%% Pretty-printed JSON encoder
encode_json_pretty(Map, Indent) when is_map(Map) ->
    IndentStr = lists:duplicate(Indent * 2, $ ),
    NextIndentStr = lists:duplicate((Indent + 1) * 2, $ ),
    Pairs = [NextIndentStr ++ encode_json_pair_pretty(K, V, Indent + 1)
             || {K, V} <- maps:to_list(Map)],
    "{\n" ++ string:join(Pairs, ",\n") ++ "\n" ++ IndentStr ++ "}";
encode_json_pretty(List, Indent) when is_list(List) ->
    case io_lib:printable_list(List) of
        true ->
            "\"" ++ List ++ "\"";
        false ->
            IndentStr = lists:duplicate(Indent * 2, $ ),
            NextIndentStr = lists:duplicate((Indent + 1) * 2, $ ),
            Elements = [NextIndentStr ++ encode_json_pretty(E, Indent + 1) || E <- List],
            "[\n" ++ string:join(Elements, ",\n") ++ "\n" ++ IndentStr ++ "]"
    end;
encode_json_pretty(Atom, _) when is_atom(Atom) ->
    "\"" ++ atom_to_list(Atom) ++ "\"";
encode_json_pretty(Int, _) when is_integer(Int) ->
    integer_to_list(Int);
encode_json_pretty(Float, _) when is_float(Float) ->
    lists:flatten(io_lib:format("~.6f", [Float]));
encode_json_pretty(Binary, _) when is_binary(Binary) ->
    "\"" ++ binary_to_list(Binary) ++ "\"".

encode_json_pair_pretty(Key, Value, Indent) ->
    "\"" ++ atom_to_list(Key) ++ "\": " ++ encode_json_pretty(Value, Indent).