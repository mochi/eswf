%% @author Matthew Dempsky <matthew@mochimedia.com>
%% @copyright 2007 Mochi Media, Inc.

-module(eswf_actions_utils).
-author(matthew@mochimedia.com).

-export([stencilify/3]).

%% @spec stencilify(Actions::binary(), Fun, Acc) -> {Chunks, NewAcc}
%% where
%%       Fun = (String::binary(), Acc) -> {Action, NewAcc}
%%       Action = skip | {punch, Key}
%%       Chunks = [{chunk, iodata()} | {hole, Key}]
%%       Key = term() | {as2insnlen, BaseSize, [Key]}
%%
%% @doc Returns a list of chunks after folding <code>Fun</code> over
%% each string in a sequence of ActionScript instructions.  To
%% generate a valid sequence of ActionScript instructions, each
%% <code>as2insnlen</code> hole must be filled with the 16-bit
%% representation of <code>BaseSize</code> plus the size of each value
%% specified in <code>Key</code>.  Every other hole should be replaced
%% by a null terminated ASCII string.
stencilify(Actions, Fun, UserAcc) ->
    stencilify(Actions, Fun, UserAcc, []).

stencilify(<<>>, _Fun, UserAcc, Acc) ->
    {lists:flatten(lists:reverse(Acc)), UserAcc};
stencilify(<<Code, Rest/binary>>, Fun, UserAcc, Acc) when Code < 16#80 ->
    %% XXX: Could speed up some, but only affects precomputation
    stencilify(Rest, Fun, UserAcc, [{chunk, [Code]} | Acc]);
stencilify(<<Code, Length:16/little, Data:Length/binary, Rest/binary>>, Fun, UserAcc, Acc) ->
    {NewChunks, NewUserAcc} = do_cisc(Code, Data, Fun, UserAcc),
    stencilify(Rest, Fun, NewUserAcc, [NewChunks | Acc]).

do_cisc(16#88, <<Count:16/little, Rest/binary>>, Fun, Acc) ->
    %% ConstantPool
    {Chunks, NewAcc} = pool_loop(Fun, Acc, Rest, Count, []),
    Result = make_cisc_chunks(16#88, [{chunk, <<Count:16/little>>} | Chunks]),
    {Result, NewAcc};
do_cisc(16#96, PushData, Fun, Acc) ->
    %% PushData
    {Chunks, NewAcc} = push_loop(Fun, Acc, PushData, []),
    Result = make_cisc_chunks(16#96, Chunks),
    {Result, NewAcc};
do_cisc(Code, Data, _Fun, Acc) ->
    {{chunk, <<Code, (size(Data)):16/little, Data/binary>>}, Acc}.

make_cisc_chunks(Code, Chunks) ->
    ChunksSize = lists:sum([iolist_size(C) || {chunk, C} <- Chunks]),
    SizeChunk =
        case [Key || {hole, Key} <- Chunks] of
            [] ->
                {chunk, <<ChunksSize:16/little>>};
            Keys ->
                {hole, {as2insnlen, ChunksSize, Keys}}
        end,
    [{chunk, [Code]}, SizeChunk | Chunks].


pool_loop(_Fun, Acc, <<>>, 0, Chunks) ->
    {lists:reverse(Chunks), Acc};
pool_loop(Fun, Acc, Pool, N, Chunks) when N > 0 ->
    {String, Rest} = parse_string(Pool),
    {Action, NewAcc} = Fun(String, Acc),
    NewChunk =
        case Action of
            skip ->
                {chunk, [String, 0]};
            {punch, Key} ->
                {hole, Key}
        end,
    pool_loop(Fun, NewAcc, Rest, N - 1, [NewChunk | Chunks]).

push_loop(_Fun, Acc, <<>>, Chunks) ->
    {lists:reverse(Chunks), Acc};
push_loop(Fun, Acc, <<0, Rest/binary>>, Chunks) ->
    {String, Rest2} = parse_string(Rest),
    {Action, NewAcc} = Fun(String, Acc),
    NewChunk =
        case Action of
            skip ->
                {chunk, [String, 0]};
            {punch, Key} ->
                {hole, Key}
        end,
    push_loop(Fun, NewAcc, Rest2, [NewChunk, {chunk, [0]} | Chunks]);
push_loop(Fun, Acc, <<1, Float:32/float, Rest/binary>>, Chunks) ->
    push_loop(Fun, Acc, Rest, [{chunk, <<1, Float:32/float>>} | Chunks]);
push_loop(Fun, Acc, <<2, Rest/binary>>, Chunks) ->
    push_loop(Fun, Acc, Rest, [{chunk, [2]} | Chunks]);
push_loop(Fun, Acc, <<3, Rest/binary>>, Chunks) ->
    push_loop(Fun, Acc, Rest, [{chunk, [3]} | Chunks]);
push_loop(Fun, Acc, <<4, Reg, Rest/binary>>, Chunks) ->
    push_loop(Fun, Acc, Rest, [{chunk, [4, Reg]} | Chunks]);
push_loop(Fun, Acc, <<5, Bool, Rest/binary>>, Chunks) ->
    push_loop(Fun, Acc, Rest, [{chunk, [5, Bool]} | Chunks]);
push_loop(Fun, Acc, <<6, Double:64/float, Rest/binary>>, Chunks) ->
    push_loop(Fun, Acc, Rest, [{chunk, <<6, Double:64/float>>} | Chunks]);
push_loop(Fun, Acc, <<7, Int:32/little, Rest/binary>>, Chunks) ->
    push_loop(Fun, Acc, Rest, [{chunk, <<7, Int:32/little>>} | Chunks]);
push_loop(Fun, Acc, <<8, Ref, Rest/binary>>, Chunks) ->
    push_loop(Fun, Acc, Rest, [{chunk, [8, Ref]} | Chunks]);
push_loop(Fun, Acc, <<9, Ref:16/little, Rest/binary>>, Chunks) ->
    push_loop(Fun, Acc, Rest, [{chunks, <<9, Ref:16/little>>} | Chunks]).


parse_string(Binary) ->
    parse_string(Binary, 0).

parse_string(Binary, N) ->
    case Binary of
        <<String:N/binary, 0, Rest/binary>> ->
            {String, Rest};
        _ when N < size(Binary) ->
            parse_string(Binary, N + 1)
    end.
