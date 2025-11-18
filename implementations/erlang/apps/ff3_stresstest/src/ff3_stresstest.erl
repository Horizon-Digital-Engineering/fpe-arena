%% FF3 Stress Test Tool - Erlang Implementation

-module(ff3_stresstest).
-export([main/1]).

-define(DEFAULT_ITERATIONS, 1000).
-define(DEFAULT_MIN_LEN, 6).
-define(DEFAULT_MAX_LEN, 20).
-define(DEFAULT_ALPHABETS, ["digits", "hex-lower", "base36-lower", "base62"]).

-record(opts, {
    iterations = ?DEFAULT_ITERATIONS :: pos_integer(),
    alphabets = ?DEFAULT_ALPHABETS :: [string()],
    min_len = ?DEFAULT_MIN_LEN :: pos_integer(),
    max_len = ?DEFAULT_MAX_LEN :: pos_integer(),
    quick = false :: boolean(),
    seed = undefined :: undefined | non_neg_integer()
}).

main(Args) ->
    case parse_args(Args, #opts{}) of
        {error, Reason} ->
            io:format("Error: ~s~n", [Reason]),
            erlang:halt(1);
        {ok, Opts0} ->
            Opts = maybe_apply_quick(Opts0),
            case validate_opts(Opts) of
                ok -> run(Opts);
                {error, Reason2} ->
                    io:format("Error: ~s~n", [Reason2]),
                    erlang:halt(1)
            end
    end.

maybe_apply_quick(#opts{quick = true} = Opts) -> Opts#opts{iterations = 100};
maybe_apply_quick(Opts) -> Opts.

validate_opts(#opts{iterations = Iter}) when Iter =< 0 ->
    {error, "iterations must be greater than 0"};
validate_opts(#opts{min_len = Min}) when Min < 2 ->
    {error, "--min-length must be at least 2"};
validate_opts(#opts{min_len = Min, max_len = Max}) when Max < Min ->
    {error, "--max-length must be greater than or equal to --min-length"};
validate_opts(_) -> ok.

parse_args([], Opts) -> {ok, Opts};
parse_args(["--help" | _], _Opts) ->
    print_usage(),
    erlang:halt(0);
parse_args(["-h" | Rest], Opts) -> parse_args(["--help" | Rest], Opts);
parse_args(["--alphabets", Value | Rest], Opts) ->
    Alphs = [string:trim(A) || A <- string:tokens(Value, ","), A =/= ""],
    case Alphs of
        [] -> {error, "--alphabets list cannot be empty"};
        _ -> parse_args(Rest, Opts#opts{alphabets = Alphs})
    end;
parse_args(["--min-length", Value | Rest], Opts) ->
    case string:to_integer(Value) of
        {error, _} -> {error, "invalid value for --min-length"};
        {Min, _} -> parse_args(Rest, Opts#opts{min_len = Min})
    end;
parse_args(["--max-length", Value | Rest], Opts) ->
    case string:to_integer(Value) of
        {error, _} -> {error, "invalid value for --max-length"};
        {Max, _} -> parse_args(Rest, Opts#opts{max_len = Max})
    end;
parse_args(["--quick" | Rest], Opts) ->
    parse_args(Rest, Opts#opts{quick = true});
parse_args(["--seed", Value | Rest], Opts) ->
    case string:to_integer(Value) of
        {error, _} -> {error, "invalid value for --seed"};
        {Seed, _} when Seed >= 0 -> parse_args(Rest, Opts#opts{seed = Seed});
        _ -> {error, "--seed must be non-negative"}
    end;
parse_args([Value | Rest], Opts) ->
    case string:to_integer(Value) of
        {error, _} ->
            {error, lists:flatten(io_lib:format("unknown option: ~s", [Value]))};
        {Iterations, _} ->
            parse_args(Rest, Opts#opts{iterations = Iterations})
    end.

run(#opts{seed = Seed} = Opts) ->
    case Seed of
        undefined -> rand:seed(exsss, now_seed());
        S -> rand:seed(exsss, {S, S, S})
    end,

    io:format("FF3 Stress Test v1.0~n"),
    io:format("====================~n~n"),
    io:format("Warning: FF3 was withdrawn by NIST; run for education and research only.~n~n"),
    io:format("Test configuration~n"),
    io:format("  Iterations per alphabet: ~p~n", [Opts#opts.iterations]),
    io:format("  Random key/tweak generation: enabled~n"),
    io:format("  String length range: ~p-~p characters~n", [Opts#opts.min_len, Opts#opts.max_len]),
    io:format("  Alphabets: ~s~n~n", [string:join(Opts#opts.alphabets, ", ")]),

    AlphabetInfos = [alphabet_info(Name) || Name <- Opts#opts.alphabets],
    Start = erlang:monotonic_time(millisecond),

    {TotalTests, TotalFailures} = lists:foldl(
        fun(Info, {AccTests, AccFailures}) ->
            {Tests, Failures} = stress_alphabet(Info, Opts),
            {AccTests + Tests, AccFailures + Failures}
        end,
        {0, 0},
        AlphabetInfos
    ),

    Duration = erlang:monotonic_time(millisecond) - Start,

    io:format("Summary~n"),
    io:format("  Total tests: ~p~n", [TotalTests]),
    io:format("  Failures: ~p~n", [TotalFailures]),
    io:format("  Duration: ~p ms~n", [Duration]),
    case Duration > 0 of
        true ->
            Throughput = (TotalTests * 1000.0) / Duration,
            io:format("  Throughput: ~.2f tests/sec~n", [Throughput]);
        false -> ok
    end,
    case TotalFailures of
        0 ->
            io:format("  Result: all stress tests passed~n"),
            erlang:halt(0);
        _ ->
            io:format("  Result: failures detected~n"),
            erlang:halt(1)
    end.

alphabet_info("digits") -> {"digits", ff3_alphabets:digits_alphabet(), fun ff3_api:create_digits_cipher/2};
alphabet_info("hex-lower") -> {"hex-lower", ff3_alphabets:hex_lower_alphabet(), fun ff3_api:create_hex_lower_cipher/2};
alphabet_info("base36-lower") -> {"base36-lower", ff3_alphabets:base36_lower_alphabet(), fun ff3_api:create_base36_lower_cipher/2};
alphabet_info("base62") -> {"base62", ff3_alphabets:base62_alphabet(), fun ff3_api:create_base62_cipher/2};
alphabet_info(Name) -> throw({unknown_alphabet, Name}).

stress_alphabet({Name, Alphabet, Factory}, Opts) ->
    io:format("Testing ~s...~n", [Name]),
    io:format("  Alphabet: ~s (radix ~p)~n", [Alphabet, length(Alphabet)]),

    Iterations = Opts#opts.iterations,
    Interval = max(1, Iterations div 10),

    {Passed, Failed} = stress_loop(Iterations, Interval, 0, 0, Alphabet, Factory, Opts),

    io:format("  Passed: ~p/~p~n", [Passed, Iterations]),
    io:format("  Failed: ~p/~p~n~n", [Failed, Iterations]),
    {Iterations, Failed}.

stress_loop(0, _Interval, Passed, Failed, _Alphabet, _Factory, _Opts) -> {Passed, Failed};
stress_loop(Remaining, Interval, Passed, Failed, Alphabet, Factory, Opts) ->
    Counter = Opts#opts.iterations - Remaining + 1,
    maybe_print_progress(Counter, Interval, Opts#opts.iterations),

    Key = random_bytes(16),
    Tweak = random_bytes(8),
    Len = Opts#opts.min_len + rand:uniform(Opts#opts.max_len - Opts#opts.min_len + 1) - 1,
    Plain = generate_plaintext(Alphabet, Len),

    case run_iteration(Factory, Plain, Key, Tweak) of
        ok -> stress_loop(Remaining - 1, Interval, Passed + 1, Failed, Alphabet, Factory, Opts);
        {error, Detail} ->
            print_failure(Key, Tweak, Plain, Detail),
            stress_loop(Remaining - 1, Interval, Passed, Failed + 1, Alphabet, Factory, Opts)
    end.

maybe_print_progress(_, _, 0) -> ok;
maybe_print_progress(Counter, Interval, Total) when Counter rem Interval =:= 0; Counter =:= Total ->
    Percent = (Counter * 100) div Total,
    io:format("  Progress: ~p/~p (~p%)~n", [Counter, Total, Percent]);
maybe_print_progress(_, _, _) -> ok.

run_iteration(Factory, Plain, Key, Tweak) ->
    case Factory(Key, Tweak) of
        {ok, Cipher} ->
            case ff3_api:encrypt_string(Cipher, Plain) of
                {ok, Ciphertext} ->
                    case ff3_api:decrypt_string(Cipher, Ciphertext) of
                        {ok, Plain} -> ok;
                        {ok, Decrypted} -> {error, {ciphertext, Ciphertext, Decrypted}};
                        {error, Reason} -> {error, Reason}
                    end;
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} -> {error, Reason}
    end.

print_failure(Key, Tweak, Plain, {ciphertext, Ciphertext, Decrypted}) ->
    io:format("  Round-trip failed:\n"),
    io:format("    Key: ~s\n", [hex(Key)]),
    io:format("    Tweak: ~s\n", [hex(Tweak)]),
    io:format("    Plaintext: \"~s\"\n", [Plain]),
    io:format("    Ciphertext: \"~s\"\n", [Ciphertext]),
    io:format("    Decrypted: \"~s\"\n", [Decrypted]);
print_failure(Key, Tweak, Plain, Detail) ->
    io:format("  Round-trip failed:\n"),
    io:format("    Key: ~s\n", [hex(Key)]),
    io:format("    Tweak: ~s\n", [hex(Tweak)]),
    io:format("    Plaintext: \"~s\"\n", [Plain]),
    io:format("    Detail: ~p\n", [Detail]).

random_bytes(N) -> crypto:strong_rand_bytes(N).

hex(Bytes) -> string:to_lower(binary_to_list(binary:encode_hex(Bytes))).

generate_plaintext(Alphabet, Len) ->
    [lists:nth(rand:uniform(length(Alphabet)), Alphabet) || _ <- lists:seq(1, Len)].

now_seed() ->
    {Mega, Sec, Micro} = os:timestamp(),
    {Mega rem 30000 + 1, Sec rem 30000 + 1, Micro rem 30000 + 1}.

max(A, B) when A >= B -> A;
max(_, B) -> B.

print_usage() ->
    io:format("FF3 Stress Test Tool~n~n"),
    io:format("Usage: ff3_stresstest [OPTIONS] [ITERATIONS]~n~n"),
    io:format("Options:~n"),
    io:format("  --alphabets LIST      Comma-separated list (default: digits,hex-lower,base36-lower,base62)~n"),
    io:format("  --min-length N        Minimum plaintext length (default: 6)~n"),
    io:format("  --max-length N        Maximum plaintext length (default: 20)~n"),
    io:format("  --quick               Run 100 iterations (fast test)~n"),
    io:format("  --seed N              Random seed for reproducibility~n").
