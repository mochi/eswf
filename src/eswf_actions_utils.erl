%% @author Matthew Dempsky <matthew@mochimedia.com>
%% @copyright 2007 Mochi Media, Inc.

-module(eswf_actions_utils).
-author(matthew@mochimedia.com).

-export([stringmap/2]).

%% @spec stringmap(Fun, Actions::iodata()) -> iodata()
%% where
%%       Fun = (String::binary()) -> iodata() | skip
%%
%% @doc Returns a sequence of ActionScript (1.0 or 2.0) instructions
%% where each dictionary string <code>String</code> in
%% <code>Actions</code> has been replaced by <code>Fun(String)</code>.
%% <code>Fun(String)</code> must not contain any null bytes.
%% Alternatively, <code>Fun(String)</code> may return
%% <code>skip</code>, and <code>String</code> will remain unmodified.
%% (Returning <code>skip</code> is semantically equivalent to
%% returning <code>String</code> but slightly faster.)
%%
%% Except as mentioned above, the returned ActionScript instruction
%% sequence does not otherwise differ from <code>Actions</code>.  The
%% result of applying <code>stringmap</code> to an invalid instruction
%% sequence is undefined.
stringmap(Fun, Actions) ->
    P = make_parser(Actions),
    Edits = stringmap_loop(Fun, P, []),
    eswf_utils:edit(Actions, Edits).

find_null(Bin) ->
    find_null(Bin, 0).

find_null(Binary, Skip) -> 
    case Binary of
        <<String:Skip/binary, 0, Rest/binary>> ->
            {String, Rest};
        Binary when Skip < size(Binary) ->
            find_null(Binary, 1 + Skip)
    end.

push_loop(_Fun, <<>>, Acc) ->
    iolist_to_binary(lists:reverse(Acc));
push_loop(Fun, <<0, Rest/binary>>, Acc) ->
    {String, Rest1} = find_null(Rest),
    NewString = case Fun(String) of
                    skip ->
                        String;
                    S ->
                        S
                end,
    push_loop(Fun, Rest1, [[0, NewString, 0] | Acc]);
push_loop(Fun, <<1, Float:4/binary, Rest/binary>>, Acc) ->
    push_loop(Fun, Rest, [[1, Float] | Acc]);
push_loop(Fun, <<2, Rest/binary>>, Acc) ->
    push_loop(Fun, Rest, [2 | Acc]);
push_loop(Fun, <<3, Rest/binary>>, Acc) ->
    push_loop(Fun, Rest, [3 | Acc]);
push_loop(Fun, <<4, Reg, Rest/binary>>, Acc) ->
    push_loop(Fun, Rest, [[4, Reg] | Acc]);
push_loop(Fun, <<5, Bool, Rest/binary>>, Acc) ->
    push_loop(Fun, Rest, [[5, Bool] | Acc]);
push_loop(Fun, <<6, Double:8/binary, Rest/binary>>, Acc) ->
    push_loop(Fun, Rest, [[6, Double] | Acc]);
push_loop(Fun, <<7, Integer:4/binary, Rest/binary>>, Acc) ->
    push_loop(Fun, Rest, [[7, Integer] | Acc]);
push_loop(Fun, <<8, C8, Rest/binary>>, Acc) ->
    push_loop(Fun, Rest, [[8, C8] | Acc]);
push_loop(Fun, <<9, C16:2/binary, Rest/binary>>, Acc) ->
    push_loop(Fun, Rest, [[9, C16] | Acc]).


stringmap_loop(Fun, P0, Acc) ->
    {<<Code>>, P1} = bytes(P0, 1),
    case Code of
        0 ->
            lists:reverse(Acc);
        16#88 ->
            %% ActionConstantPool
            Pre = offset(P1),
            {_Length, P2} = get_ushort(P1),
            Post = offset(P2),
            {Count, P3} = get_ushort(P2),
            {RevEdits, NewLength, P4} = dictionary_loop(Fun, P3, 0, [], Count),
            %% @todo Verify that exactly Length bytes have been parsed
            LengthEdit = {Pre, Post, <<(2 + NewLength):16/little>>},
            NewAcc = RevEdits ++ [LengthEdit] ++ Acc,
            stringmap_loop(Fun, P4, NewAcc);
        16#96 ->
            %% ActionPush
            Pre = offset(P1),
            {Length, P2} = get_ushort(P1),
            Post = offset(P2),
            {PushData, P3} = bytes(P2, Length),
            case push_loop(Fun, PushData, []) of
                PushData ->
                    stringmap_loop(Fun, P3, Acc);
                NewPushData ->
                    NewLength = size(NewPushData),
                    NewAcc = [{Post, offset(P3), NewPushData},
                              {Pre, Post, <<NewLength:16/little>>} | Acc],
                    stringmap_loop(Fun, P3, NewAcc)
            end;
        Code when Code < 16#80 ->
            stringmap_loop(Fun, P1, Acc);
        Code ->
            {Length, P2} = get_ushort(P1),
            {_Args, P3} = bytes(P2, Length),
            stringmap_loop(Fun, P3, Acc)
    end.

dictionary_loop(_Fun, P0, Length, Acc, 0) ->
    {Acc, Length, P0};
dictionary_loop(Fun, P0, Length, Acc, N) when N > 0 ->
    Pre = offset(P0),
    {String, P1} = get_string(P0),
    Post = offset(P1),
    {NewStringLength, NewAcc} =
        case Fun(String) of
            skip ->
                {size(String), Acc};
            NewString ->
                %% @todo Verify that NewString does not contain a null byte
                Encoded = [NewString, 0],
                {iolist_size(NewString), [{Pre, Post, Encoded} | Acc]}
        end,
    NewLength = Length + NewStringLength + 1,
    dictionary_loop(Fun, P1, NewLength, NewAcc, N - 1).


%% Internal helper functions, similar in theme to eswf_abc's.

make_parser(Binary) ->
    {Binary, 0}.
bytes(P = {Binary, Offset}, NumBytes) ->
    <<_Pre:Offset/binary, Want:NumBytes/binary, _Rest/binary>> = Binary,
    {Want, seek(P, NumBytes)}.
offset({_Binary, Offset}) ->
    Offset.
seek({Binary, Offset}, NumBytes) ->
    {Binary, Offset + NumBytes}.

get_ushort(P0) ->
    {<<Value:16/little>>, P1} = bytes(P0, 2),
    {Value, P1}.

get_string({Binary, Offset}) ->
    <<_Pre:Offset/binary, Rest/binary>> = Binary,
    N = findnull(Rest, 0),
    <<_Pre:Offset/binary, Want:N/binary, 0, _Rest2/binary>> = Binary,
    {Want, {Binary, Offset + N + 1}}.

findnull(<<0, _Rest/binary>>, N) ->
    N;
findnull(<<_Ch, Rest/binary>>, N) ->
    findnull(Rest, N + 1).
