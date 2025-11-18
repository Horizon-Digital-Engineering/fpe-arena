%% FF3 NIST Test Vector Validator Main Module

-module(ff3_validate).
-export([main/1]).

-record(opts, {
    vectors_path = undefined :: undefined | string(),
    verbose = false :: boolean(),
    quiet = false :: boolean(),
    help = false :: boolean()
}).

-record(nist_vector, {
    sample :: integer(),
    algorithm :: string(),
    key :: string(),
    radix :: integer(),
    alphabet :: string(),
    plaintext :: string(),
    tweak :: string(),
    ciphertext :: string()
}).

main(Args) ->
    try
        Opts = parse_args(Args, #opts{}),

        case Opts#opts.help of
            true ->
                show_usage(),
                halt(0);
            false ->
                run_validation(Opts)
        end
    catch
        error:{badarg, Msg} ->
            io:format(standard_error, "Error: ~s~n", [Msg]),
            show_usage(),
            halt(1);
        Error:Reason ->
            io:format(standard_error, "Error: ~p:~p~n", [Error, Reason]),
            halt(2)
    end.

show_usage() ->
    io:format("FF3 NIST Test Vector Validation Tool~n~n"),
    io:format("Usage: ff3-validate [OPTIONS]~n~n"),
    io:format("Options:~n"),
    io:format("  --vectors PATH    Path to test vectors JSON file~n"),
    io:format("  --verbose         Show detailed test output~n"),
    io:format("  --quiet           Only show failures and summary~n"),
    io:format("  -h, --help        Show this help message~n~n").

parse_args([], Opts) -> Opts;
parse_args(["-h"|_], Opts) -> Opts#opts{help = true};
parse_args(["--help"|_], Opts) -> Opts#opts{help = true};
parse_args(["--vectors", Path|Rest], Opts) ->
    parse_args(Rest, Opts#opts{vectors_path = Path});
parse_args(["--verbose"|Rest], Opts) ->
    parse_args(Rest, Opts#opts{verbose = true});
parse_args(["--quiet"|Rest], Opts) ->
    parse_args(Rest, Opts#opts{quiet = true});
parse_args([Arg|_], _) ->
    error({badarg, io_lib:format("Unknown option: ~s", [Arg])}).

find_vectors_file(undefined) ->
    Paths = ["../../shared/test-vectors/nist_ff3_official_vectors.json",
             "../../../shared/test-vectors/nist_ff3_official_vectors.json",
             "./nist_ff3_official_vectors.json"],
    find_first_existing(Paths);
find_vectors_file(Path) ->
    case filelib:is_file(Path) of
        true -> {ok, Path};
        false -> {error, "Vectors file not found: " ++ Path}
    end.

find_first_existing([]) -> {error, "Could not find NIST test vectors file"};
find_first_existing([Path|Rest]) ->
    case filelib:is_file(Path) of
        true -> {ok, Path};
        false -> find_first_existing(Rest)
    end.

run_validation(Opts) ->
    Quiet = Opts#opts.quiet,

    case not Quiet of
        true ->
            io:format("FF3 NIST Test Vector Validation Tool~n"),
            io:format("========================================~n~n");
        false -> ok
    end,

    case find_vectors_file(Opts#opts.vectors_path) of
        {error, Msg} ->
            io:format(standard_error, "Error: ~s~n", [Msg]),
            case Opts#opts.vectors_path of
                undefined ->
                    io:format(standard_error, "Try: ff3-validate --vectors /path/to/vectors.json~n", []);
                _ -> ok
            end,
            halt(2);
        {ok, VectorsPath} ->
            case not Quiet of
                true -> io:format("Vector file: ~s~n~n", [VectorsPath]);
                false -> ok
            end,

            Vectors = load_nist_vectors(VectorsPath),

            case not Quiet of
                true -> io:format("Testing ~p NIST FF3 vectors...~n~n", [length(Vectors)]);
                false -> ok
            end,

            {Passed, Total} = run_tests(Vectors, Opts, 0, 0),

            case not Quiet of
                true ->
                    io:format("~n========================================~n"),
                    io:format("Results: ~p/~p passed~n~n", [Passed, Total]);
                false -> ok
            end,

            case Passed =:= Total of
                true ->
                    case not Quiet of
                        true ->
                            io:format("ALL NIST TEST VECTORS PASSED!~n~n"),
                            io:format("WARNING: FF3 was WITHDRAWN by NIST due to security vulnerabilities.~n"),
                            io:format("This implementation is for EDUCATIONAL and RESEARCH purposes only.~n"),
                            io:format("DO NOT use in production systems.~n~n");
                        false -> ok
                    end,
                    halt(0);
                false ->
                    case not Quiet of
                        true -> io:format("VALIDATION FAILED~n~n");
                        false -> ok
                    end,
                    halt(1)
            end
    end.

run_tests([], _Opts, Passed, Total) -> {Passed, Total};
run_tests([Vector | Rest], Opts, Passed, Total) ->
    case test_single_vector(Vector, Opts) of
        pass -> run_tests(Rest, Opts, Passed + 1, Total + 1);
        fail -> run_tests(Rest, Opts, Passed, Total + 1)
    end.

test_single_vector(Vector, Opts) ->
    #nist_vector{
        sample = Sample,
        key = KeyHex,
        radix = Radix,
        plaintext = Plaintext,
        tweak = TweakHex,
        ciphertext = ExpectedCiphertext
    } = Vector,

    try
        Key = ff3_api:hex_to_bytes(KeyHex),
        Tweak = ff3_api:hex_to_bytes(TweakHex),

        Alphabet = case Radix of
            10 -> ff3_alphabets:digits_alphabet();
            26 -> ff3_alphabets:custom_radix26_alphabet();
            _ -> error({unsupported_radix, Radix})
        end,

        Result = ff3_api:encrypt(Plaintext, Key, Tweak, Alphabet),
        Decrypted = ff3_api:decrypt(Result, Key, Tweak, Alphabet),

        EncryptPassed = (Result =:= ExpectedCiphertext),
        RoundtripPassed = (Decrypted =:= Plaintext),
        TestPassed = EncryptPassed andalso RoundtripPassed,

        case TestPassed of
            true ->
                case Opts#opts.verbose of
                    true -> io:format("Sample ~p: PASS~n", [Sample]);
                    false -> ok
                end,
                pass;
            false ->
                case Opts#opts.quiet of
                    false ->
                        io:format("Sample ~p: FAIL~n", [Sample]),
                        case EncryptPassed of
                            false ->
                                io:format("  Expected: ~s~n", [ExpectedCiphertext]),
                                io:format("  Got:      ~s~n", [Result]);
                            true -> ok
                        end,
                        case RoundtripPassed of
                            false -> io:format("  Round-trip failed~n", []);
                            true -> ok
                        end;
                    true -> ok
                end,
                fail
        end
    catch
        _:Error ->
            case Opts#opts.quiet of
                false -> io:format("Sample ~p: ERROR - ~p~n", [Sample, Error]);
                true -> ok
            end,
            fail
    end.

load_nist_vectors(FilePath) ->
    {ok, Binary} = file:read_file(FilePath),
    Json = jsx:decode(Binary, [return_maps]),
    #{<<"vectors">> := VectorsList} = Json,
    [parse_vector(V) || V <- VectorsList].

parse_vector(VectorMap) ->
    #nist_vector{
        sample = maps:get(<<"sample">>, VectorMap),
        algorithm = binary_to_list(maps:get(<<"algorithm">>, VectorMap)),
        key = binary_to_list(maps:get(<<"key">>, VectorMap)),
        radix = maps:get(<<"radix">>, VectorMap),
        alphabet = binary_to_list(maps:get(<<"alphabet">>, VectorMap, <<"">>)),
        plaintext = binary_to_list(maps:get(<<"plaintext">>, VectorMap)),
        tweak = binary_to_list(maps:get(<<"tweak">>, VectorMap)),
        ciphertext = binary_to_list(maps:get(<<"ciphertext">>, VectorMap))
    }.
