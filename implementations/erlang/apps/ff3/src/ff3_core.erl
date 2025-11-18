-module(ff3_core).
-export([ff3_cipher/3, encrypt/2, decrypt/2]).

-include("../include/ff3.hrl").

%% Type specifications
-spec ff3_cipher(integer(), binary(), binary()) -> #ff3_cipher{}.
-spec encrypt(#ff3_cipher{}, [integer()]) -> [integer()].
-spec decrypt(#ff3_cipher{}, [integer()]) -> [integer()].

%% Create FF3 cipher
ff3_cipher(Radix, Key, Tweak) ->
    if
        Radix < 2; Radix > 62 ->
            error({invalid_radix, Radix});
        byte_size(Key) =/= 16 andalso byte_size(Key) =/= 24 andalso byte_size(Key) =/= 32 ->
            error({invalid_key_size, byte_size(Key)});
        byte_size(Tweak) =/= 8 ->
            error({invalid_tweak_size, byte_size(Tweak)});
        true ->
            %% FF3 specification requires byte reversal of the key
            ReversedKey = list_to_binary(lists:reverse(binary_to_list(Key))),
            #ff3_cipher{radix = Radix, key = ReversedKey, tweak = Tweak}
    end.

%% Encrypt plaintext
encrypt(#ff3_cipher{radix = Radix, key = Key, tweak = Tweak}, Plaintext) ->
    N = length(Plaintext),
    U = (N + 1) div 2,  % ceil(N/2)
    _V = N - U,         % floor(N/2) - not used but kept for clarity

    {A, B} = lists:split(U, Plaintext),

    %% 8 Feistel rounds
    {FinalA, FinalB} = feistel_rounds(A, B, Radix, Key, Tweak, 0, 8),

    FinalA ++ FinalB.

%% Decrypt ciphertext
decrypt(#ff3_cipher{radix = Radix, key = Key, tweak = Tweak}, Ciphertext) ->
    N = length(Ciphertext),
    U = (N + 1) div 2,
    _V = N - U,         % floor(N/2) - not used but kept for clarity

    {A, B} = lists:split(U, Ciphertext),

    %% 8 Feistel rounds in reverse
    {FinalA, FinalB} = feistel_rounds_decrypt(A, B, Radix, Key, Tweak, 7),

    FinalA ++ FinalB.

%% Feistel rounds for encryption
feistel_rounds(A, B, _Radix, _Key, _Tweak, Round, MaxRounds) when Round >= MaxRounds ->
    {A, B};
feistel_rounds(A, B, Radix, Key, Tweak, Round, MaxRounds) ->
    case Round rem 2 of
        0 ->
            %% Even round: use B to update A
            W = calculate_w(Tweak, Round, B),
            P = calculate_p(Round, W, B, Key, Radix),
            M = pow(Radix, length(A)),

            %% FF3 uses reversed digit order: NUM_radix(REV(A))
            ReversedA = lists:reverse(A),
            ANum = array_to_bigint(ReversedA, Radix),

            %% c = (NUM_radix(REV(A)) + NUM(S)) mod radix^u
            Y = (ANum + P) rem M,

            %% C = REV(STR_radix(c))
            NewDigits = bigint_to_array(Y, Radix, length(A)),
            NewA = lists:reverse(NewDigits),
            feistel_rounds(NewA, B, Radix, Key, Tweak, Round + 1, MaxRounds);
        1 ->
            %% Odd round: use A to update B
            W = calculate_w(Tweak, Round, A),
            P = calculate_p(Round, W, A, Key, Radix),
            M = pow(Radix, length(B)),

            ReversedB = lists:reverse(B),
            BNum = array_to_bigint(ReversedB, Radix),

            Y = (BNum + P) rem M,

            NewDigits = bigint_to_array(Y, Radix, length(B)),
            NewB = lists:reverse(NewDigits),
            feistel_rounds(A, NewB, Radix, Key, Tweak, Round + 1, MaxRounds)
    end.

%% Feistel rounds for decryption (reverse order)
feistel_rounds_decrypt(A, B, _Radix, _Key, _Tweak, Round) when Round < 0 ->
    {A, B};
feistel_rounds_decrypt(A, B, Radix, Key, Tweak, Round) ->
    case Round rem 2 of
        0 ->
            %% Even round: use B to update A (reverse)
            W = calculate_w(Tweak, Round, B),
            P = calculate_p(Round, W, B, Key, Radix),
            M = pow(Radix, length(A)),

            ReversedA = lists:reverse(A),
            ANum = array_to_bigint(ReversedA, Radix),

            %% c = (NUM_radix(REV(A)) - NUM(S)) mod radix^u
            Y = ((ANum - P) rem M + M) rem M,

            NewDigits = bigint_to_array(Y, Radix, length(A)),
            NewA = lists:reverse(NewDigits),
            feistel_rounds_decrypt(NewA, B, Radix, Key, Tweak, Round - 1);
        1 ->
            %% Odd round: use A to update B (reverse)
            W = calculate_w(Tweak, Round, A),
            P = calculate_p(Round, W, A, Key, Radix),
            M = pow(Radix, length(B)),

            ReversedB = lists:reverse(B),
            BNum = array_to_bigint(ReversedB, Radix),

            Y = ((BNum - P) rem M + M) rem M,

            NewDigits = bigint_to_array(Y, Radix, length(B)),
            NewB = lists:reverse(NewDigits),
            feistel_rounds_decrypt(A, NewB, Radix, Key, Tweak, Round - 1)
    end.

%% Calculate W from tweak
calculate_w(Tweak, Round, _Half) ->
    case Round rem 2 of
        0 ->
            %% Even rounds: W = Tr (rightmost 4 bytes)
            <<_:4/binary, W:4/binary>> = Tweak,
            W;
        1 ->
            %% Odd rounds: W = Tl (leftmost 4 bytes)
            <<W:4/binary, _:4/binary>> = Tweak,
            W
    end.

%% Calculate P using AES
calculate_p(Round, W, Half, Key, Radix) ->
    %% P = REVB(AES-ECB(K, REVB(W XOR [i]_4 || NUM_radix(REV(B)))))
    <<W1, W2, W3, W4>> = W,

    %% First 4 bytes: W XOR with round number in the last byte
    Input1 = W1,
    Input2 = W2,
    Input3 = W3,
    Input4 = W4 bxor Round,

    %% Last 12 bytes: NUM_radix(REV(B))
    ReversedHalf = lists:reverse(Half),
    HalfBigInt = array_to_bigint(ReversedHalf, Radix),
    HalfBytes = bigint_to_bytes(HalfBigInt, 12),

    Input = <<Input1, Input2, Input3, Input4, HalfBytes/binary>>,

    %% Apply FF3 byte reversal: REVB before AES
    ReversedInput = reverse_bytes(Input),

    %% Encrypt with AES
    AESOutput = crypto:crypto_one_time(aes_ecb, Key, ReversedInput, true),

    %% Apply FF3 byte reversal: REVB after AES
    Output = reverse_bytes(AESOutput),

    %% Convert to integer
    bytes_to_bigint(Output).

%% Utility functions - use built-in math power for better precision
pow(_Base, 0) -> 1;
pow(Base, Exp) when Exp > 0 ->
    round(math:pow(Base, Exp)).

%% Convert digit array to big integer (left to right processing)
array_to_bigint(Digits, Radix) ->
    array_to_bigint(Digits, Radix, 0).

array_to_bigint([], _Radix, Acc) -> Acc;
array_to_bigint([H|T], Radix, Acc) ->
    array_to_bigint(T, Radix, Acc * Radix + H).

%% Convert big integer to digit array of fixed length
bigint_to_array(Num, Radix, Len) ->
    Digits = bigint_to_digits(Num, Radix, []),
    pad_array(Digits, Len).

bigint_to_digits(0, _Radix, []) -> [0];
bigint_to_digits(0, _Radix, Acc) -> Acc;
bigint_to_digits(Num, Radix, Acc) ->
    bigint_to_digits(Num div Radix, Radix, [Num rem Radix | Acc]).

%% Pad array to required length with leading zeros
pad_array(Digits, Len) ->
    case length(Digits) of
        L when L >= Len ->
            lists:nthtail(L - Len, Digits);  % Take last Len digits
        L ->
            lists:duplicate(Len - L, 0) ++ Digits  % Pad with leading zeros
    end.

bigint_to_bytes(Num, Len) ->
    Bytes = integer_to_bytes(Num),
    ByteLen = byte_size(Bytes),
    if
        ByteLen >= Len ->
            binary:part(Bytes, ByteLen - Len, Len);
        true ->
            Padding = binary:copy(<<0>>, Len - ByteLen),
            <<Padding/binary, Bytes/binary>>
    end.

integer_to_bytes(0) -> <<0>>;
integer_to_bytes(N) when N > 0 ->
    integer_to_bytes(N, <<>>).

integer_to_bytes(0, Acc) -> Acc;
integer_to_bytes(N, Acc) ->
    integer_to_bytes(N div 256, <<(N rem 256), Acc/binary>>).

bytes_to_bigint(<<>>) -> 0;
bytes_to_bigint(<<H, T/binary>>) ->
    H * pow(256, byte_size(T)) + bytes_to_bigint(T).

reverse_bytes(Bin) ->
    list_to_binary(lists:reverse(binary_to_list(Bin))).