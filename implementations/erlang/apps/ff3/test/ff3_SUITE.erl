%% FF3 Common Test Suite
%% Comprehensive test coverage for FF3 Format Preserving Encryption

-module(ff3_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("../include/ff3.hrl").

%% Test suite exports
-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test case exports
-export([
    test_ff3_core_create_cipher/1,
    test_ff3_core_encrypt_decrypt/1,
    test_ff3_core_invalid_params/1,
    test_ff3_api_factory_functions/1,
    test_ff3_api_encrypt_decrypt/1,
    test_ff3_api_invalid_operations/1,
    test_ff3_alphabets_validation/1,
    test_ff3_alphabets_standard_sets/1,
    test_nist_vectors_integration/1,
    test_round_trip_consistency/1
]).

%% Test vector record
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

%% Common Test callbacks

all() ->
    [
        {group, ff3_core_tests},
        {group, ff3_api_tests},
        {group, ff3_alphabets_tests},
        {group, integration_tests}
    ].

groups() ->
    [
        {ff3_core_tests, [parallel], [
            test_ff3_core_create_cipher,
            test_ff3_core_encrypt_decrypt,
            test_ff3_core_invalid_params
        ]},
        {ff3_api_tests, [parallel], [
            test_ff3_api_factory_functions,
            test_ff3_api_encrypt_decrypt,
            test_ff3_api_invalid_operations
        ]},
        {ff3_alphabets_tests, [parallel], [
            test_ff3_alphabets_validation,
            test_ff3_alphabets_standard_sets
        ]},
        {integration_tests, [sequence], [
            test_nist_vectors_integration,
            test_round_trip_consistency
        ]}
    ].

init_per_suite(Config) ->
    application:start(crypto),
    Config.

end_per_suite(_Config) ->
    application:stop(crypto),
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% FF3 Core Tests

test_ff3_core_create_cipher(_Config) ->
    Key = <<16#EF, 16#43, 16#59, 16#D8, 16#D5, 16#80, 16#AA, 16#4F,
            16#7F, 16#03, 16#6D, 16#6F, 16#04, 16#FC, 16#6A, 16#94>>,
    Tweak = <<16#D8, 16#E7, 16#92, 16#0A, 16#FA, 16#33, 16#0A, 16#73>>,

    %% Test valid cipher creation
    Cipher = ff3_core:ff3_cipher(10, Key, Tweak),
    true = is_record(Cipher, ff3_cipher),

    %% Test various key sizes
    Key192 = <<Key/binary, 16#AB, 16#CD, 16#EF, 16#12, 16#34, 16#56, 16#78, 16#90>>,
    Cipher192 = ff3_core:ff3_cipher(10, Key192, Tweak),
    true = is_record(Cipher192, ff3_cipher),

    Key256 = <<Key192/binary, 16#AB, 16#CD, 16#EF, 16#12, 16#34, 16#56, 16#78, 16#90>>,
    Cipher256 = ff3_core:ff3_cipher(10, Key256, Tweak),
    true = is_record(Cipher256, ff3_cipher),

    ok.

test_ff3_core_encrypt_decrypt(_Config) ->
    Key = <<16#EF, 16#43, 16#59, 16#D8, 16#D5, 16#80, 16#AA, 16#4F,
            16#7F, 16#03, 16#6D, 16#6F, 16#04, 16#FC, 16#6A, 16#94>>,
    Tweak = <<16#D8, 16#E7, 16#92, 16#0A, 16#FA, 16#33, 16#0A, 16#73>>,
    Cipher = ff3_core:ff3_cipher(10, Key, Tweak),

    %% Test basic encryption/decryption
    Plaintext = [8,9,0,1,2,1,2,3,4,5,6,7,8,9,0,0,0,0],
    Ciphertext = ff3_core:encrypt(Cipher, Plaintext),
    Decrypted = ff3_core:decrypt(Cipher, Ciphertext),

    Plaintext = Decrypted,

    %% Test different lengths
    ShortPlain = [1,2,3,4,5,6],
    ShortCipher = ff3_core:encrypt(Cipher, ShortPlain),
    ShortDecrypted = ff3_core:decrypt(Cipher, ShortCipher),
    ShortPlain = ShortDecrypted,

    ok.

test_ff3_core_invalid_params(_Config) ->
    Key = <<16#EF, 16#43, 16#59, 16#D8, 16#D5, 16#80, 16#AA, 16#4F,
            16#7F, 16#03, 16#6D, 16#6F, 16#04, 16#FC, 16#6A, 16#94>>,
    Tweak = <<16#D8, 16#E7, 16#92, 16#0A, 16#FA, 16#33, 16#0A, 16#73>>,

    %% Test invalid radix
    try ff3_core:ff3_cipher(1, Key, Tweak) of
        _ -> ct:fail("Should fail with invalid radix")
    catch
        error:{invalid_radix, 1} -> ok
    end,

    try ff3_core:ff3_cipher(63, Key, Tweak) of
        _ -> ct:fail("Should fail with invalid radix")
    catch
        error:{invalid_radix, 63} -> ok
    end,

    %% Test invalid key size
    ShortKey = <<1, 2, 3>>,
    try ff3_core:ff3_cipher(10, ShortKey, Tweak) of
        _ -> ct:fail("Should fail with invalid key size")
    catch
        error:{invalid_key_size, 3} -> ok
    end,

    %% Test invalid tweak size
    ShortTweak = <<1, 2, 3>>,
    try ff3_core:ff3_cipher(10, Key, ShortTweak) of
        _ -> ct:fail("Should fail with invalid tweak size")
    catch
        error:{invalid_tweak_size, 3} -> ok
    end,

    ok.

%% FF3 API Tests

test_ff3_api_factory_functions(_Config) ->
    Key = <<16#EF, 16#43, 16#59, 16#D8, 16#D5, 16#80, 16#AA, 16#4F,
            16#7F, 16#03, 16#6D, 16#6F, 16#04, 16#FC, 16#6A, 16#94>>,
    Tweak = <<16#D8, 16#E7, 16#92, 16#0A, 16#FA, 16#33, 16#0A, 16#73>>,

    %% Test all factory functions
    {ok, DigitsCipher} = ff3_api:create_digits_cipher(Key, Tweak),
    10 = ff3_api:get_radix(DigitsCipher),

    {ok, HexLowerCipher} = ff3_api:create_hex_lower_cipher(Key, Tweak),
    16 = ff3_api:get_radix(HexLowerCipher),

    {ok, HexUpperCipher} = ff3_api:create_hex_upper_cipher(Key, Tweak),
    16 = ff3_api:get_radix(HexUpperCipher),

    {ok, Base36LowerCipher} = ff3_api:create_base36_lower_cipher(Key, Tweak),
    36 = ff3_api:get_radix(Base36LowerCipher),

    {ok, Base36UpperCipher} = ff3_api:create_base36_upper_cipher(Key, Tweak),
    36 = ff3_api:get_radix(Base36UpperCipher),

    {ok, Base62Cipher} = ff3_api:create_base62_cipher(Key, Tweak),
    62 = ff3_api:get_radix(Base62Cipher),

    {ok, CustomCipher} = ff3_api:create_custom_cipher(Key, Tweak, "0123456789abcdefghijklmnop"),
    26 = ff3_api:get_radix(CustomCipher),

    ok.

test_ff3_api_encrypt_decrypt(_Config) ->
    Key = <<16#EF, 16#43, 16#59, 16#D8, 16#D5, 16#80, 16#AA, 16#4F,
            16#7F, 16#03, 16#6D, 16#6F, 16#04, 16#FC, 16#6A, 16#94>>,
    Tweak = <<16#D8, 16#E7, 16#92, 16#0A, 16#FA, 16#33, 16#0A, 16#73>>,
    {ok, Cipher} = ff3_api:create_digits_cipher(Key, Tweak),

    %% Test string encryption/decryption
    Plaintext = "890121234567890000",
    {ok, Ciphertext} = ff3_api:encrypt_string(Cipher, Plaintext),
    {ok, Decrypted} = ff3_api:decrypt_string(Cipher, Ciphertext),

    Plaintext = Decrypted,

    %% Test different strings
    ShortPlain = "123456",
    {ok, ShortCipher} = ff3_api:encrypt_string(Cipher, ShortPlain),
    {ok, ShortDecrypted} = ff3_api:decrypt_string(Cipher, ShortCipher),
    ShortPlain = ShortDecrypted,

    ok.

test_ff3_api_invalid_operations(_Config) ->
    Key = <<16#EF, 16#43, 16#59, 16#D8, 16#D5, 16#80, 16#AA, 16#4F,
            16#7F, 16#03, 16#6D, 16#6F, 16#04, 16#FC, 16#6A, 16#94>>,
    Tweak = <<16#D8, 16#E7, 16#92, 16#0A, 16#FA, 16#33, 16#0A, 16#73>>,
    {ok, Cipher} = ff3_api:create_digits_cipher(Key, Tweak),

    %% Test invalid characters
    {error, _} = ff3_api:encrypt_string(Cipher, "123abc"),

    %% Test invalid alphabet
    {error, _} = ff3_api:create_custom_cipher(Key, Tweak, "a"),  % Too short
    {error, _} = ff3_api:create_custom_cipher(Key, Tweak, "aabbcc"),  % Duplicates

    ok.

%% FF3 Alphabets Tests

test_ff3_alphabets_validation(_Config) ->
    %% Test valid alphabets
    ok = ff3_alphabets:validate_alphabet("0123456789"),
    ok = ff3_alphabets:validate_alphabet("abcdefghijklmnopqrstuvwxyz"),

    %% Test invalid alphabets
    {error, {invalid_radix, _}} = ff3_alphabets:validate_alphabet("a"),  % Too short
    {error, {invalid_alphabet, _}} = ff3_alphabets:validate_alphabet("aabbcc"),  % Duplicates

    ok.

test_ff3_alphabets_standard_sets(_Config) ->
    %% Test standard alphabets
    "0123456789" = ff3_alphabets:digits_alphabet(),
    "0123456789abcdef" = ff3_alphabets:hex_lower_alphabet(),
    "0123456789ABCDEF" = ff3_alphabets:hex_upper_alphabet(),
    "0123456789abcdefghijklmnopqrstuvwxyz" = ff3_alphabets:base36_lower_alphabet(),
    "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" = ff3_alphabets:base36_upper_alphabet(),

    %% Test get_alphabet_for_radix
    {ok, "0123456789"} = ff3_alphabets:get_alphabet_for_radix(10),
    {ok, "0123456789abcdef"} = ff3_alphabets:get_alphabet_for_radix(16),
    {error, {unsupported_radix, 99}} = ff3_alphabets:get_alphabet_for_radix(99),

    ok.

%% Integration Tests

test_nist_vectors_integration(_Config) ->
    Vectors = load_nist_vectors(),
    ct:pal("Testing ~p NIST vectors", [length(Vectors)]),

    Results = [test_single_nist_vector(V) || V <- Vectors],

    Passed = length([ok || ok <- Results]),
    Total = length(Results),

    ct:pal("NIST Vector Results: ~p/~p passed", [Passed, Total]),

    %% All vectors must pass
    Total = Passed,

    ok.

test_round_trip_consistency(_Config) ->
    Key = <<16#EF, 16#43, 16#59, 16#D8, 16#D5, 16#80, 16#AA, 16#4F,
            16#7F, 16#03, 16#6D, 16#6F, 16#04, 16#FC, 16#6A, 16#94>>,
    Tweak = <<16#D8, 16#E7, 16#92, 16#0A, 16#FA, 16#33, 16#0A, 16#73>>,

    %% Test multiple ciphers and random strings
    Ciphers = [
        ff3_api:create_digits_cipher(Key, Tweak),
        ff3_api:create_hex_lower_cipher(Key, Tweak),
        ff3_api:create_base36_lower_cipher(Key, Tweak)
    ],

    TestStrings = [
        "123456789",
        "abcdef1234",
        "0123456789abcdef",
        "zyxwvutsrqponmlkjihgfedcba9876543210"
    ],

    [test_cipher_with_strings(Cipher, TestStrings) || Cipher <- Ciphers],

    ok.

%% Helper functions

test_single_nist_vector(Vector) ->
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

        {ok, Cipher} = case Radix of
            10 -> ff3_api:create_digits_cipher(Key, Tweak);
            26 -> ff3_api:create_custom_cipher(Key, Tweak, ff3_alphabets:custom_radix26_alphabet());
            _ -> {error, {unsupported_radix, Radix}}
        end,

        {ok, Result} = ff3_api:encrypt_string(Cipher, Plaintext),
        {ok, Decrypted} = ff3_api:decrypt_string(Cipher, Result),

        case {Result, Decrypted} of
            {ExpectedCiphertext, Plaintext} ->
                ct:pal("Sample ~p: PASS", [Sample]),
                ok;
            _ ->
                ct:pal("Sample ~p: FAIL - Expected: ~s, Got: ~s", [Sample, ExpectedCiphertext, Result]),
                error
        end

    catch
        Error:Reason ->
            ct:pal("Sample ~p: ERROR - ~p:~p", [Sample, Error, Reason]),
            error
    end.

test_cipher_with_strings({ok, Cipher}, TestStrings) ->
    ValidStrings = filter_valid_strings(Cipher, TestStrings),
    [test_round_trip(Cipher, S) || S <- ValidStrings];
test_cipher_with_strings(Cipher, TestStrings) ->
    ValidStrings = filter_valid_strings(Cipher, TestStrings),
    [test_round_trip(Cipher, S) || S <- ValidStrings].

filter_valid_strings(Cipher, Strings) ->
    Radix = ff3_api:get_radix(Cipher),
    {ok, Alphabet} = ff3_alphabets:get_alphabet_for_radix(Radix),
    [S || S <- Strings, string_fits_alphabet(S, Alphabet)].

string_fits_alphabet(String, Alphabet) ->
    lists:all(fun(C) -> lists:member(C, Alphabet) end, String).

test_round_trip(Cipher, String) ->
    {ok, Encrypted} = ff3_api:encrypt_string(Cipher, String),
    {ok, Decrypted} = ff3_api:decrypt_string(Cipher, Encrypted),
    String = Decrypted.

load_nist_vectors() ->
    %% Try environment variable first (for CI/CD)
    VectorPath = case os:getenv("FF3_TEST_VECTORS_PATH") of
        false ->
            %% Fallback to relative path for local development
            "../../../../shared/test-vectors/nist_ff3_official_vectors.json";
        Path -> Path
    end,

    case file:read_file(VectorPath) of
        {ok, JsonBinary} ->
            try
                %% Parse JSON
                JsonMap = jsx:decode(JsonBinary, [return_maps]),
                VectorList = maps:get(<<"vectors">>, JsonMap),

                %% Convert each JSON vector to our record format
                [json_to_nist_vector(V) || V <- VectorList]
            catch
                _:Error ->
                    ct:pal("Failed to parse NIST vectors from ~s: ~p", [VectorPath, Error]),
                    []
            end;
        {error, Reason} ->
            ct:pal("Failed to read NIST vectors from ~s: ~p", [VectorPath, Reason]),
            []
    end.

%% Convert JSON map to nist_vector record
json_to_nist_vector(JsonMap) ->
    #nist_vector{
        sample = maps:get(<<"sample">>, JsonMap),
        algorithm = binary_to_list(maps:get(<<"algorithm">>, JsonMap)),
        key = binary_to_list(maps:get(<<"key">>, JsonMap)),
        radix = maps:get(<<"radix">>, JsonMap),
        alphabet = binary_to_list(maps:get(<<"alphabet">>, JsonMap)),
        plaintext = binary_to_list(maps:get(<<"plaintext">>, JsonMap)),
        tweak = binary_to_list(maps:get(<<"tweak">>, JsonMap)),
        ciphertext = binary_to_list(maps:get(<<"ciphertext">>, JsonMap))
    }.