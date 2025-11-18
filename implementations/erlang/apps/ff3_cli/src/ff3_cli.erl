%% FF3 CLI Main Module

-module(ff3_cli).
-export([main/1]).

main(Args) ->
    case parse_args(Args, #{}) of
        {ok, Opts} ->
            run(Opts);
        {error, help} ->
            show_usage(),
            erlang:halt(0);
        {error, Reason} ->
            io:format("Error: ~s~n", [Reason]),
            erlang:halt(1)
    end.

show_usage() ->
    io:format("FF3 CLI - Format Preserving Encryption~n~n"),
    io:format("Usage: ff3-cli [OPTIONS]~n~n"),
    io:format("Options:~n"),
    io:format("  -e, --encrypt TEXT      Encrypt the given text~n"),
    io:format("  -d, --decrypt TEXT      Decrypt the given text~n"),
    io:format("  -k, --key HEX           AES key in hex format (32/48/64 hex chars)~n"),
    io:format("  -t, --tweak HEX         Tweak in hex format (16 hex chars)~n"),
    io:format("  -a, --alphabet TYPE     Alphabet type:~n"),
    io:format("                            digits (default)~n"),
    io:format("                            hex-lower~n"),
    io:format("                            hex-upper~n"),
    io:format("                            base36-lower~n"),
    io:format("                            base36-upper~n"),
    io:format("                            base62~n"),
    io:format("  -c, --custom CHARSET    Custom alphabet charset~n"),
    io:format("  -h, --help              Show this help message~n~n"),
    io:format("Examples:~n"),
    io:format("  ff3-cli -e \"1234567890\" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73~n"),
    io:format("  ff3-cli -d \"7501889140\" -k EF4359D8D580AA4F7F036D6F04FC6A94 -t D8E7920AFA330A73~n~n").

parse_args([], Opts) ->
    validate_opts(Opts);
parse_args(["-h"|_], _Opts) ->
    {error, help};
parse_args(["--help"|_], _Opts) ->
    {error, help};
parse_args(["-e", Text|Rest], Opts) ->
    parse_args(Rest, Opts#{encrypt => Text});
parse_args(["--encrypt", Text|Rest], Opts) ->
    parse_args(Rest, Opts#{encrypt => Text});
parse_args(["-d", Text|Rest], Opts) ->
    parse_args(Rest, Opts#{decrypt => Text});
parse_args(["--decrypt", Text|Rest], Opts) ->
    parse_args(Rest, Opts#{decrypt => Text});
parse_args(["-k", KeyHex|Rest], Opts) ->
    parse_args(Rest, Opts#{key => KeyHex});
parse_args(["--key", KeyHex|Rest], Opts) ->
    parse_args(Rest, Opts#{key => KeyHex});
parse_args(["-t", TweakHex|Rest], Opts) ->
    parse_args(Rest, Opts#{tweak => TweakHex});
parse_args(["--tweak", TweakHex|Rest], Opts) ->
    parse_args(Rest, Opts#{tweak => TweakHex});
parse_args(["-a", Type|Rest], Opts) ->
    parse_args(Rest, Opts#{alphabet => Type});
parse_args(["--alphabet", Type|Rest], Opts) ->
    parse_args(Rest, Opts#{alphabet => Type});
parse_args(["-c", Charset|Rest], Opts) ->
    parse_args(Rest, Opts#{custom => Charset});
parse_args(["--custom", Charset|Rest], Opts) ->
    parse_args(Rest, Opts#{custom => Charset});
parse_args([Unknown|_], _Opts) ->
    {error, io_lib:format("Unknown option: ~s", [Unknown])}.

validate_opts(Opts) ->
    case {maps:is_key(encrypt, Opts), maps:is_key(decrypt, Opts)} of
        {false, false} ->
            {error, "Either --encrypt or --decrypt must be specified"};
        _ ->
            case {maps:is_key(key, Opts), maps:is_key(tweak, Opts)} of
                {false, _} ->
                    {error, "Key (-k) is required"};
                {_, false} ->
                    {error, "Tweak (-t) is required"};
                _ ->
                    {ok, Opts}
            end
    end.

run(Opts) ->
    try
        KeyHex = maps:get(key, Opts),
        TweakHex = maps:get(tweak, Opts),
        AlphabetType = maps:get(alphabet, Opts, "digits"),
        CustomCharset = maps:get(custom, Opts, undefined),

        Key = ff3_api:hex_to_bytes(KeyHex),
        Tweak = ff3_api:hex_to_bytes(TweakHex),

        %% Validate key length
        KeyLen = byte_size(Key),
        case KeyLen of
            16 -> ok;
            24 -> ok;
            32 -> ok;
            _ -> throw({invalid_key_length, KeyLen})
        end,

        %% Validate tweak length
        case byte_size(Tweak) of
            8 -> ok;
            _ -> throw({invalid_tweak_length, byte_size(Tweak)})
        end,

        Alphabet = create_alphabet(AlphabetType, CustomCharset),

        case {maps:get(encrypt, Opts, undefined), maps:get(decrypt, Opts, undefined)} of
            {undefined, Ciphertext} ->
                Plaintext = ff3_api:decrypt(Ciphertext, Key, Tweak, Alphabet),
                io:format("~s~n", [Plaintext]);
            {Plaintext, _} ->
                Ciphertext = ff3_api:encrypt(Plaintext, Key, Tweak, Alphabet),
                io:format("~s~n", [Ciphertext])
        end
    catch
        _:Error ->
            io:format("Error: ~p~n", [Error]),
            erlang:halt(1)
    end.

create_alphabet(_Type, CustomCharset) when CustomCharset =/= undefined ->
    CustomCharset;
create_alphabet("digits", _) ->
    ff3_alphabets:digits_alphabet();
create_alphabet("hex-lower", _) ->
    ff3_alphabets:hex_lower_alphabet();
create_alphabet("hex-upper", _) ->
    ff3_alphabets:hex_upper_alphabet();
create_alphabet("base36-lower", _) ->
    ff3_alphabets:base36_lower_alphabet();
create_alphabet("base36-upper", _) ->
    ff3_alphabets:base36_upper_alphabet();
create_alphabet("base62", _) ->
    ff3_alphabets:base62_alphabet();
create_alphabet(Other, _) ->
    throw({unknown_alphabet, Other}).
