%% FF3 API Module - High-level string interface for FF3 Format Preserving Encryption
%% Provides convenient string-based operations with alphabet support

-module(ff3_api).
-export([
    %% High-level interface for CLI apps
    encrypt/4,
    decrypt/4,
    %% Factory functions
    create_digits_cipher/2,
    create_hex_lower_cipher/2,
    create_hex_upper_cipher/2,
    create_base36_lower_cipher/2,
    create_base36_upper_cipher/2,
    create_base62_cipher/2,
    create_custom_cipher/3,
    encrypt_string/2,
    decrypt_string/2,
    get_radix/1,
    %% Utilities
    hex_to_bytes/1,
    bytes_to_hex/1
]).

-record(ff3_string_cipher, {
    core_cipher :: term(),
    alphabet :: string(),
    radix :: integer()
}).

-type ff3_string_cipher() :: #ff3_string_cipher{}.
-type string_result() :: {ok, string()} | {error, term()}.

%% High-level interface for CLI applications

%% Encrypt string with specified alphabet
-spec encrypt(string(), binary(), binary(), string()) -> string().
encrypt(Plaintext, Key, Tweak, Alphabet) ->
    {ok, Cipher} = create_custom_cipher(Key, Tweak, Alphabet),
    {ok, Ciphertext} = encrypt_string(Cipher, Plaintext),
    Ciphertext.

%% Decrypt string with specified alphabet
-spec decrypt(string(), binary(), binary(), string()) -> string().
decrypt(Ciphertext, Key, Tweak, Alphabet) ->
    {ok, Cipher} = create_custom_cipher(Key, Tweak, Alphabet),
    {ok, Plaintext} = decrypt_string(Cipher, Ciphertext),
    Plaintext.

%% Factory functions for common alphabets

%% Create digits cipher (radix 10)
-spec create_digits_cipher(binary(), binary()) -> {ok, ff3_string_cipher()} | {error, term()}.
create_digits_cipher(Key, Tweak) ->
    try
        Cipher = ff3_core:ff3_cipher(10, Key, Tweak),
        {ok, #ff3_string_cipher{
            core_cipher = Cipher,
            alphabet = ff3_alphabets:digits_alphabet(),
            radix = 10
        }}
    catch
        Error:ErrReason:_ -> {error, {Error, ErrReason}}
    end.

%% Create hex lowercase cipher (radix 16)
-spec create_hex_lower_cipher(binary(), binary()) -> {ok, ff3_string_cipher()} | {error, term()}.
create_hex_lower_cipher(Key, Tweak) ->
    try
        Cipher = ff3_core:ff3_cipher(16, Key, Tweak),
        {ok, #ff3_string_cipher{
            core_cipher = Cipher,
            alphabet = ff3_alphabets:hex_lower_alphabet(),
            radix = 16
        }}
    catch
        Error:ErrReason:_ -> {error, {Error, ErrReason}}
    end.

%% Create hex uppercase cipher (radix 16)
-spec create_hex_upper_cipher(binary(), binary()) -> {ok, ff3_string_cipher()} | {error, term()}.
create_hex_upper_cipher(Key, Tweak) ->
    try
        Cipher = ff3_core:ff3_cipher(16, Key, Tweak),
        {ok, #ff3_string_cipher{
            core_cipher = Cipher,
            alphabet = ff3_alphabets:hex_upper_alphabet(),
            radix = 16
        }}
    catch
        Error:ErrReason:_ -> {error, {Error, ErrReason}}
    end.

%% Create base36 lowercase cipher (radix 36)
-spec create_base36_lower_cipher(binary(), binary()) -> {ok, ff3_string_cipher()} | {error, term()}.
create_base36_lower_cipher(Key, Tweak) ->
    try
        Cipher = ff3_core:ff3_cipher(36, Key, Tweak),
        {ok, #ff3_string_cipher{
            core_cipher = Cipher,
            alphabet = ff3_alphabets:base36_lower_alphabet(),
            radix = 36
        }}
    catch
        Error:ErrReason:_ -> {error, {Error, ErrReason}}
    end.

%% Create base36 uppercase cipher (radix 36)
-spec create_base36_upper_cipher(binary(), binary()) -> {ok, ff3_string_cipher()} | {error, term()}.
create_base36_upper_cipher(Key, Tweak) ->
    try
        Cipher = ff3_core:ff3_cipher(36, Key, Tweak),
        {ok, #ff3_string_cipher{
            core_cipher = Cipher,
            alphabet = ff3_alphabets:base36_upper_alphabet(),
            radix = 36
        }}
    catch
        Error:ErrReason:_ -> {error, {Error, ErrReason}}
    end.

%% Create base62 cipher (radix 62)
-spec create_base62_cipher(binary(), binary()) -> {ok, ff3_string_cipher()} | {error, term()}.
create_base62_cipher(Key, Tweak) ->
    try
        Cipher = ff3_core:ff3_cipher(62, Key, Tweak),
        {ok, #ff3_string_cipher{
            core_cipher = Cipher,
            alphabet = ff3_alphabets:base62_alphabet(),
            radix = 62
        }}
    catch
        Error:ErrReason:_ -> {error, {Error, ErrReason}}
    end.

%% Create custom alphabet cipher
-spec create_custom_cipher(binary(), binary(), string()) -> {ok, ff3_string_cipher()} | {error, term()}.
create_custom_cipher(Key, Tweak, Alphabet) ->
    try
        Radix = length(Alphabet),
        case ff3_alphabets:validate_alphabet(Alphabet) of
            ok ->
                Cipher = ff3_core:ff3_cipher(Radix, Key, Tweak),
                {ok, #ff3_string_cipher{
                    core_cipher = Cipher,
                    alphabet = Alphabet,
                    radix = Radix
                }};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        Error:ErrReason:_ -> {error, {Error, ErrReason}}
    end.

%% String encryption/decryption operations

%% Encrypt string
-spec encrypt_string(ff3_string_cipher(), string()) -> string_result().
encrypt_string(#ff3_string_cipher{core_cipher = Cipher, alphabet = Alphabet}, Plaintext) ->
    try
        Digits = string_to_array(Plaintext, Alphabet),
        EncryptedDigits = ff3_core:encrypt(Cipher, Digits),
        Ciphertext = array_to_string(EncryptedDigits, Alphabet),
        {ok, Ciphertext}
    catch
        Error:ErrReason:_ -> {error, {Error, ErrReason}}
    end.

%% Decrypt string
-spec decrypt_string(ff3_string_cipher(), string()) -> string_result().
decrypt_string(#ff3_string_cipher{core_cipher = Cipher, alphabet = Alphabet}, Ciphertext) ->
    try
        Digits = string_to_array(Ciphertext, Alphabet),
        DecryptedDigits = ff3_core:decrypt(Cipher, Digits),
        Plaintext = array_to_string(DecryptedDigits, Alphabet),
        {ok, Plaintext}
    catch
        Error:ErrReason:_ -> {error, {Error, ErrReason}}
    end.

%% Query functions

%% Get radix of cipher
-spec get_radix(ff3_string_cipher()) -> integer().
get_radix(#ff3_string_cipher{radix = Radix}) ->
    Radix.

%% Utility functions

%% Convert hex string to bytes
-spec hex_to_bytes(string()) -> binary().
hex_to_bytes(HexString) ->
    hex_to_bytes(HexString, <<>>).

hex_to_bytes([], Acc) -> Acc;
hex_to_bytes([A, B | Rest], Acc) ->
    Byte = list_to_integer([A, B], 16),
    hex_to_bytes(Rest, <<Acc/binary, Byte>>);
hex_to_bytes([_], _Acc) ->
    error({invalid_hex, "Hex string must have even length"}).

%% Convert bytes to hex string
-spec bytes_to_hex(binary()) -> string().
bytes_to_hex(Bytes) ->
    lists:flatten([io_lib:format("~2.16.0B", [X]) || <<X>> <= Bytes]).

%% Internal helper functions

%% Convert string to digit array using alphabet
string_to_array(String, Alphabet) ->
    [char_to_index(C, Alphabet) || C <- String].

%% Convert digit array to string using alphabet
array_to_string(Array, Alphabet) ->
    [lists:nth(Index + 1, Alphabet) || Index <- Array].

%% Find character index in alphabet
char_to_index(Char, Alphabet) ->
    case string:chr(Alphabet, Char) of
        0 -> error({invalid_character, Char});
        Index -> Index - 1
    end.