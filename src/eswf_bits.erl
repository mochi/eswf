%% @author Bob Ippolito <bob@redivi.com>
%% @copyright 2006 Bob Ippolito

%% @doc Bit twiddling functions.

-module(eswf_bits).
-export([calc/2, enc/3, enc/4, to_bytes/1]).
-export([test/0]).

%% @type bit_format() = unsigned | signed | fixed

%% @spec calc(Kind::bit_format(), List) -> integer()
%% @doc Returns the number of bits required to represent each number of list.
calc(unsigned, [])   -> 0;
calc(unsigned, List) -> calc(unsigned, lists:max(List), 0);
calc(signed, List)   -> 1 + calc(unsigned, [abs(X) || X <- List]);
calc(fixed, [])      -> 1;
calc(fixed, List)    ->
    1 + calc(unsigned, trunc(65536 * lists:max([abs(X) || X <- List])), 0).

calc(unsigned, Num, 0) when is_float(Num) -> calc(unsigned, trunc(Num), 0);
calc(unsigned, 0, Pos)                    -> Pos;
calc(unsigned, Num, Pos) when Pos < 32    -> calc(unsigned, Num bsr 1, Pos + 1).

%% @spec enc(Kind::bit_format(), Bits, Num, Acc) -> list()
%% @doc Returns Num converted to Kind as a Bits-long list, prepended to Acc.
enc(_, 0, _, Acc) ->
    Acc;
enc(unsigned, Bits, Num, Acc) when is_float(Num) ->
    enc(unsigned, Bits, trunc(Num), Acc);
enc(signed, Bits, Num, Acc) when is_float(Num) ->
    enc(signed, Bits, trunc(Num), Acc);
enc(unsigned, Bits, Num, Acc) when Bits >= 8 ->
    % Unlikely to be worth implementing a 16-bit or 32-bit optimization.
    NextBits = Bits - 8,
    NextNum = Num bsr 8,
    Byte = Num band 255,
    <<B7:1, B6:1, B5:1, B4:1, B3:1, B2:1, B1:1, B0:1>> = <<Byte>>,
    enc(unsigned, NextBits, NextNum, [B7, B6, B5, B4, B3, B2, B1, B0 | Acc]);
enc(unsigned, Bits, Num, Acc) ->
    NextBits = Bits - 1,
    [1 band (Num bsr NextBits) | enc(unsigned, NextBits, Num, Acc)];
enc(signed, Bits, Num, Acc) ->
    % bsr preserves sign so we're good here for signed as well.
    enc(unsigned, Bits, Num, Acc);
enc(fixed, Bits, Num, Acc) ->
    enc(signed, Bits, trunc(Num * 65536), Acc).

%% @spec enc(Kind::bit_format(), Bits, Num) -> list()
%% @equiv env(Kind, Bits, Num, [])
enc(Kind, Bits, Num) ->
    enc(Kind, Bits, Num, []).

%% @spec to_bytes(L) -> list()
%% @doc Returns a list of bytes from the list of bits L.
to_bytes(L) ->
    to_bytes(L, []).

to_bytes([], Acc) ->
    lists:reverse(Acc);
to_bytes([B7, B6, B5, B4, B3, B2, B1, B0 | T], Acc) ->
    <<Byte>> = <<B7:1, B6:1, B5:1, B4:1, B3:1, B2:1, B1:1, B0:1>>,
    to_bytes(T, [Byte | Acc]);
to_bytes(List, Acc) ->
    % Right-pad the list with zeros so that the byte-wide clause matches.
    Zeros = lists:duplicate(8 - length(List), 0),
    to_bytes(List ++ Zeros, Acc).

%% @spec test() -> ok
%% @doc Run the tests.

test() ->
    ok = test(to_bytes),
    ok = test(enc),
    ok = test(calc),
    ok.

test(to_bytes) ->
    [] = to_bytes([]),
    [128] = to_bytes([1]),
    [254] = to_bytes([1,1,1,1,1,1,1]),
    [255] = to_bytes([1,1,1,1,1,1,1,1]),
    [255, 1] = to_bytes([1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,1]),
    ok;
test(enc) ->
    [1,1,1,1,1,1,1,0] = enc(unsigned, 8, 254),
    [0,1,1,1,1,1,1,1,0] = enc(unsigned, 9, 254),
    [1,1,1,1,1,1,1,0] = enc(signed, 8, -2),
    [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0] = enc(signed, 16, -2),
    [0,0,1,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0] = enc(fixed, 22, 10.5),
    [0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0] = enc(fixed, 22, 30.0),
    [1,0,1,0,1,1,1,0,0,1] = enc(fixed, 10, -0.005),
    ok;
test(calc) ->
    8 = calc(unsigned, [254]),
    4 = calc(unsigned, [12]),
    2 = calc(signed, [-1]),
    12 = calc(signed, [-1200]),
    21 = calc(fixed, [10.5]),
    22 = calc(fixed, [30]),
    22 = calc(fixed, [10.5, 30]),
    22 = calc(fixed, [30, 10.5]),
    10 = calc(fixed, [-0.005]),
    ok.
    
