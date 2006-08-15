%% @author Bob Ippolito <bob@redivi.com>
%% @copyright 2006 Bob Ippolito

%% @doc Bit twiddling functions.

-module(eswf_bits).
-export([calc/2, enc/3, enc/4, to_bytes/1]).

%% @type bit_format() = unsigned | signed | fixed

%% @spec calc(Kind::bit_format(), List) -> integer()
%% @doc Returns the number of bits required to represent each number of list.
calc(unsigned, [])   -> 0;
calc(unsigned, List) -> calc(unsigned, lists:max(List), 0);
calc(signed, List)   -> 1 + calc(unsigned, List);
calc(fixed, [])      -> 1;
calc(fixed, List)    ->
    1 + calc(unsigned, abs(trunc(lists:max(List) * 65536)), 0).

calc(unsigned, 0, Pos)                 -> Pos;
calc(unsigned, Num, Pos) when Pos < 32 -> calc(unsigned, Num bsr 1, Pos + 1).

%% @spec enc(Kind::bit_format(), Bits, Num, Acc) -> list()
%% @doc Returns Num converted to Kind as a Bits-long list, prepended to Acc.
enc(_, 0, _, Acc) ->
    Acc;
enc(unsigned, 1, Num, Acc) ->
    [Num band 1 | Acc];
enc(unsigned, Bits, Num, Acc) ->
    NextBits = Bits - 1,
    [1 band (Num bsr NextBits) | enc(unsigned, NextBits, Num, Acc)];
enc(signed, Bits, Num, Acc) when Num < 0 ->
    [1 | enc(unsigned, Bits - 1, Num, Acc)];
enc(signed, Bits, Num, Acc) ->
    [0 | enc(unsigned, Bits - 1, Num, Acc)];
enc(fixed, Bits, Num, Acc) ->
    enc(signed, Bits, trunc(Num * 65536), Acc).

%% @spec enc(Kind::bit_format(), Bits, Num) -> list()
%% @equiv env(Kind, Bits, Num, [])
enc(Kind, Bits, Num) ->
    enc(Kind, Bits, Num, []).

%% @spec to_bytes(L) -> list()
%% @doc Returns a list of bytes from the list of bits L.
to_bytes(L) -> to_bytes(L, []).

to_bytes([B7, B6, B5, B4, B3, B2, B1, B0 | T], Acc) ->
    Byte = (B7 bsl 7) bor (B6 bsl 6) bor
    (B5 bsl 5) bor (B4 bsl 4) bor (B3 bsl 3) bor
    (B2 bsl 2) bor (B1 bsl 1) bor B0,
    to_bytes(T, [Byte | Acc]);
to_bytes([], Acc) ->
    lists:reverse(Acc);
to_bytes(List, Acc) ->
    to_bytes(List ++ lists:duplicate(8 - length(List), 0), Acc).
