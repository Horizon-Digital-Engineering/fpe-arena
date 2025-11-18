%% FF3 record definitions
%% Shared between implementation and tests

-record(ff3_cipher, {
    radix :: integer(),
    key :: binary(),
    tweak :: binary()
}).