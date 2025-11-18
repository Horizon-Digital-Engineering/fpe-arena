%% FF3 Alphabets Module - Character Set Definitions
%% Defines standard alphabets for Format Preserving Encryption

-module(ff3_alphabets).
-export([
    digits_alphabet/0,
    hex_lower_alphabet/0,
    hex_upper_alphabet/0,
    base36_lower_alphabet/0,
    base36_upper_alphabet/0,
    base62_alphabet/0,
    custom_radix26_alphabet/0,
    validate_alphabet/1,
    get_alphabet_for_radix/1
]).

%% Standard alphabet constants (order is part of the spec!)

%% Digits alphabet (radix 10)
-spec digits_alphabet() -> string().
digits_alphabet() ->
    "0123456789".

%% Hex lowercase alphabet (radix 16)
-spec hex_lower_alphabet() -> string().
hex_lower_alphabet() ->
    "0123456789abcdef".

%% Hex uppercase alphabet (radix 16)
-spec hex_upper_alphabet() -> string().
hex_upper_alphabet() ->
    "0123456789ABCDEF".

%% Base36 lowercase alphabet (radix 36)
-spec base36_lower_alphabet() -> string().
base36_lower_alphabet() ->
    "0123456789abcdefghijklmnopqrstuvwxyz".

%% Base36 uppercase alphabet (radix 36)
-spec base36_upper_alphabet() -> string().
base36_upper_alphabet() ->
    "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ".

%% Base62 alphabet (radix 62)
-spec base62_alphabet() -> string().
base62_alphabet() ->
    "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz".

%% Custom radix 26 alphabet (used in NIST test vectors)
-spec custom_radix26_alphabet() -> string().
custom_radix26_alphabet() ->
    "0123456789abcdefghijklmnop".

%% Validate alphabet for FF3 requirements
-spec validate_alphabet(string()) -> ok | {error, term()}.
validate_alphabet(Alphabet) ->
    Length = length(Alphabet),
    case Length of
        L when L < 2 ->
            {error, {invalid_radix, "Alphabet must have at least 2 characters"}};
        L when L > 62 ->
            {error, {invalid_radix, "Alphabet must have at most 62 characters"}};
        _ ->
            case has_unique_characters(Alphabet) of
                true -> ok;
                false -> {error, {invalid_alphabet, "Alphabet must have unique characters"}}
            end
    end.

%% Get standard alphabet for common radix values
-spec get_alphabet_for_radix(integer()) -> {ok, string()} | {error, term()}.
get_alphabet_for_radix(10) -> {ok, digits_alphabet()};
get_alphabet_for_radix(16) -> {ok, hex_lower_alphabet()};
get_alphabet_for_radix(26) -> {ok, custom_radix26_alphabet()};
get_alphabet_for_radix(36) -> {ok, base36_lower_alphabet()};
get_alphabet_for_radix(62) -> {ok, base62_alphabet()};
get_alphabet_for_radix(Radix) ->
    {error, {unsupported_radix, Radix}}.

%% Internal helper functions

%% Check if all characters in alphabet are unique
has_unique_characters(Alphabet) ->
    length(Alphabet) =:= length(lists:usort(Alphabet)).