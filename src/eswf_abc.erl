%% @copyright 2007 Mochi Media, Inc.
%% @author Matthew Dempsky <matthew@mochimedia.com>

%% @doc The <code>eswf_abc</code> module provides functions for
%% manipulating ActionScript Byte Code segments (herein ABC segments).
%%
%% A full description of the ABC data format is outside of the
%% scope of this documentation.  For details, see chapter 4 of <a
%% href="http://www.adobe.com/devnet/actionscript/articles/avm2overview.pdf"
%% >ActionScript Virtual Machine 2 Overview</a>.


-module(eswf_abc).
-author(matthew@mochimedia.com).

-define(ABCMAJOR, 46).
-define(ABCMINOR, 16).

-export([stringmap/2, stencilify/3]).
-export([encode_string/1]).
-export([test/0]).

%% @type abcsegment() = binary()

%% @spec stringmap(Fun, ABCSegment::abcsegment()) -> iodata()
%% where
%%       Fun = (String::binary()) -> iodata() | skip
%%
%% @doc Returns an ABC segment where each string constant
%% <code>String</code> in <code>ABCSegment</code> has been replaced by
%% <code>Fun(String)</code>.  <code>Fun(String)</code> must return a
%% valid UTF-8 sequence.  Alternatively, <code>Fun(String)</code> may
%% return <code>skip</code>, and <code>String</code> will remain
%% unmodified in the resulting ABC segment.  (Returning
%% <code>skip</code> is semantically equivalent to returning
%% <code>String</code> but slightly faster.)
%%
%% Except as mentioned above, the returned ABC segment does not
%% otherwise differ from <code>ABCSegment</code>.  The result of
%% applying <code>stringmap</code> to an invalid ABC segment is
%% undefined.
stringmap(Fun, ABCSegment) when is_function(Fun) ->
    FoldFun = fun(String, Begin, End, Acc) ->
                      case Fun(String) of
                          skip ->
                              Acc;
                          NewString ->
                              Encoded = encode_string(NewString),
                              [{Begin, End, Encoded} | Acc]
                      end
              end,
    {_Begin, Acc, _End} = fold(ABCSegment, FoldFun, []),
    eswf_utils:edit(ABCSegment, lists:reverse(Acc)).

%% @type template() = [{hole, term()} | {chunk, iodata()}]

%% @spec stencilify(ABCSegment::abcsegment(), Fun, Acc) -> {template(), NewAcc}
%% where
%%       Fun = (String::binary()) -> {Action, NewAcc}
%%       Action = skip | {punch, term()}
%%
%% @doc Returns a <code>template()</code> based on
%% <code>ABCSegment</code>.  For each string constant
%% <code>String</code> in <code>ABCSegment</code>, <code>Fun(String,
%% Acc)</code> is evaluated, which should return a tuple with an
%% action and a new accumulator.  If the action is <code>{punch,
%% Term}</code>, then the corresponding section of the template
%% contains <code>{hole, Term}</code>, which should later be replaced
%% by a valid string constant encoding (see
%% <code>encode_string/1</code>).  Otherwise, the action must be
%% <code>skip</code>, and the string constant is left in tact.
stencilify(ABCSegment, Fun, UserAcc) when is_function(Fun) ->
    FoldFun = fun(String, Begin, End, {Acc, UAcc}) ->
                      {Action, NewUAcc} = Fun(String, UAcc),
                      NewElt = 
                          case Action of
                              skip ->
                                  {chunk, subseg(ABCSegment, Begin, End)};
                              {punch, Key} ->
                                  {hole, Key}
                          end,
                      {[NewElt | Acc], NewUAcc}
              end,
    {Begin, {Acc, NewUserAcc}, End} = fold(ABCSegment, FoldFun, {[], UserAcc}),
    BeginChunk = {chunk, subseg(ABCSegment, 0, Begin)},
    EndChunk = {chunk, subseg(ABCSegment, End, all)},
    Chunks = [BeginChunk | lists:reverse(Acc, [EndChunk])],
    {Chunks, NewUserAcc}.


fold(ABCSegment, Fun, Acc) ->
    P0 = make_parser(ABCSegment),
    {ok, ?ABCMINOR, P1} = parse(u16, P0),
    {ok, ?ABCMAJOR, P2} = parse(u16, P1),
    {ok, _Ints, P3} = parse_array(s32, P2),
    {ok, _UInts, P4} = parse_array(u32, P3),
    {ok, _Doubles, P5} = parse_array(d64, P4),
    {ok, N, P6} = parse(u30, P5),
    Begin = offset(P6),
    {Res, P7} =
        if
            N > 0 ->
                fold1(Fun, P6, N - 1, Acc);
            true ->
                {Acc, P6}
        end,
    End = offset(P7),
    {Begin, Res, End}.

fold1(_Fun, Parser, 0, Acc) ->
    {Acc, Parser};
fold1(Fun, Parser, N, Acc) when N > 0 ->
    Begin = offset(Parser),
    {ok, String, Next} = parse_string(Parser),
    End = offset(Next),
    fold1(Fun, Next, N - 1, Fun(String, Begin, End, Acc)).


%% @spec parse(Type, Parser::parser()) -> {ok, Value, Next::parser()}
%% where
%%       Type = u8 | u16 | s24 | u30 | u32 | s32 | d64
%%
%% @doc Parses an ABC primitive data value of type <code>Type</code>
%% from <code>Parser</code>.  Returns the value cast to the
%% appropriate Erlang type.
%%
%% N.B. Where <code>mxmlc</code> (from the Flex 2 SDK) and the AVM2
%% specification differ in encoding (e.g., sign extension for
%% <code>s32</code> values), we choose to be compatible with the
%% former.
parse(u8, Parser) ->
    {<<Val:8/little>>, Next} = bytes(Parser, 1),
    {ok, Val, Next};
parse(u16, Parser) ->
    {<<Val:16/little>>, Next} = bytes(Parser, 2),
    {ok, Val, Next};
parse(s24, Parser) ->
    {<<Val:24/signed-little>>, Next} = bytes(Parser, 3),
    {ok, Val, Next};
parse(u30, Binary) ->
    case parse_u32(Binary) of
        Result = {ok, Val, _Next} when Val < (1 bsl 30) ->
            Result
    end;
parse(u32, Binary) ->
    parse_u32(Binary);
parse(s32, Binary) ->
    %% According to the AVM2 spec:
    %%     In the case of s32, sign extension is applied: the seventh
    %%     bit of the last byte of the encoding is propagated to fill
    %%     out the 32 bits of the decoded value.
    %% However, mxmlc and Tamarin don't do this.
    case parse_u32(Binary) of
        {ok, Val, Next} when Val >= 16#80000000 ->
            {ok, Val - 16#100000000, Next};
        Else ->
            Else
    end;
parse(d64, Binary) ->
    case bytes(Binary, 8) of
        {<<Val:64/float-little>>, Next} ->
            {ok, Val, Next};
        {Bytes, Next} ->
            %% Erlang doesn't like Inf or NaN. :-(
            {ok, {double, Bytes}, Next}
    end;
parse(Type, _Binary) ->
    throw({badtype, Type}).

%% @spec parse_u32(Parser::parser()) -> {ok, Val::int(), Next::parser()}
%%
%% @doc Parse a variable-length encoded 32-bit unsigned integer value
%% from <code>Binary</code>.
parse_u32({Binary, Offset}) ->
    %% This could have been written as a loop, but after correctly
    %% handling the corner cases, it probably wouldn't be any better.
    <<_Skip:Offset/binary, Next/binary>> = Binary,
    {Bytes, Val} =
        case Next of
            <<0:1, V0:7, _Rest/binary>> ->
                {1, V0};
            <<1:1, V0:7, 0:1, V1:7, _Rest/binary>> ->
                {2, V0 + (V1 bsl 7)};
            <<1:1, V0:7, 1:1, V1:7, 0:1, V2:7, _Rest/binary>> ->
                {3, V0 + (V1 bsl 7) + (V2 bsl 14)};
            <<1:1, V0:7, 1:1, V1:7, 1:1, V2:7, 0:1, V3:7, _Rest/binary>> ->
                {4, V0 + (V1 bsl 7) + (V2 bsl 14) + (V3 bsl 21)};
            <<1:1, V0:7, 1:1, V1:7, 1:1, V2:7, 1:1, V3:7, 0:4, V4:4, _Rest/binary>> ->
                {5, V0 + (V1 bsl 7) + (V2 bsl 14) + (V3 bsl 21) + (V4 bsl 28)}
        end,
    {ok, Val, {Binary, Offset + Bytes}}.


%% @spec parse_array(Type, Parser::parser()) -> {ok, [Val], Next::parser()}
parse_array(Type, Parser) ->
    {ok, N, Next} = parse(u30, Parser),
    if
        N > 0 ->
            parse_array(Type, Next, N - 1, []);
        true ->
            %% Lame.
            {ok, [], Next}
    end.

parse_array(_Type, Parser, 0, Acc) ->
    {ok, lists:reverse(Acc), Parser};
parse_array(Type, Parser, N, Acc) when N > 0 ->
    {ok, Val, Next} = parse(Type, Parser),
    parse_array(Type, Next, N - 1, [Val | Acc]).


%% @spec parse_string(Parser::parser()) -> {ok, String::binary(), Next::parser()}
parse_string(Parser) ->
    %% XXX: Validate parsed string as UTF-8?
    {ok, Length, Next0} = parse(u30, Parser),
    {String, Next1} = bytes(Next0, Length),
    {ok, String, Next1}.


%% Internal helper routines for parsing.
%%
%% @todo Come up with useful public API so that parse/2 can be
%% exported.

make_parser(Binary) ->
    {Binary, 0}.

bytes({Binary, Offset}, N) ->
    <<_Skip:Offset/binary, Bytes:N/binary, _Rest/binary>> = Binary,
    {Bytes, {Binary, Offset + N}}.

subseg(Binary, Start, all) ->
    element(2, split_binary(Binary, Start));
subseg(Binary, Start, Stop) ->
    Size = Stop - Start,
    <<_Pre:Start/binary, Want:Size/binary, _Rest/binary>> = Binary,
    Want.
    

offset({_Binary, Offset}) ->
    Offset.

%% @todo Implement more encoding functions and export them.

%% @spec encode_u32(N::int()) -> [int()]
%%
%% @doc Encode <code>N</code> into a <code>u32</code> value.
encode_u32(N) when N >= 0, N < 128 ->
    [N];
encode_u32(N) when N >= 128, N < 16#100000000 ->
    [(N band 16#7f) bor 16#80 | encode_u32(N bsr 7)].

%% @spec encode_u30(N::int()) -> [int()]
%%
%% @doc Encode <code>N</code> into a <code>u30</code> value.
encode_u30(N) when N >= 0, N < 16#40000000 ->
    encode_u32(N).

%% @spec encode_string(String::iodata()) -> iodata()
%%
%% @doc Encode <code>String</code> into a <code>string_info</code>
%% value.
encode_string(String) ->
    %% XXX: Validate String as UTF-8?
    Length = iolist_size(String),
    [encode_u30(Length) | String].


%% @spec test() -> ok
%%
%% @doc Performs several unit tests and returns <code>ok</code>
%% (hopefully).
test() ->
    ok = test_parse(),
    ok = test_stringmap(),
    ok.

test_parse() ->
    V = [{u8, 0, <<0>>},
         {u8, 42, <<42>>},
         {u16, 258, <<2,1>>},
         {s24, -12345, <<199,207,255>>},
         {s32, 0, <<0>>},
         {s32, 1, <<1>>},
         {s32, 42, <<42>>},
         {s32, 1000, <<232,7>>},
         {s32, 1000000, <<192,132,61>>},
         {s32, 1000000000, <<128,148,235,220,3>>},
         {s32, -1, <<255,255,255,255,15>>},
         {s32, -2, <<254,255,255,255,15>>},
         {s32, -1000000000, <<128,236,148,163,12>>},
         {d64, 4.0e+9, <<0,0,0,0,101,205,237,65>>},
         {d64, 9007199254740993.0, <<0,0,0,0,0,0,64,67>>},

         %% I don't particularly care what these return, but they
         %% shouldn't crash.
         {d64, '_', <<0,0,0,0,0,0,240,127>>}, %% Inf
         {d64, '_', <<0,0,0,0,0,0,248,127>>}, %% NaN

         %% N.B. These test cases stand contrary to how the AVM2
         %% specification describes the encoding of s32 values, but
         %% they are how mxmlc encodes them.
         {s32, 64, <<64>>},
         {s32, 8192, <<128,64>>},
         {s32, 1048576, <<128,128,64>>}
         ],
    [begin
         P0 = make_parser(Binary),
         {ok, Got, P1} = parse(Type, P0),
         true = (Want =:= Got orelse Want =:= '_'),
         true = (offset(P1) =:= size(Binary))
     end || {Type, Want, Binary} <- V],

    ok.

test_stringmap() ->
    MakeSegment =
        fun(Strings) ->
                iolist_to_binary(
                  [<<?ABCMINOR:16/little, ?ABCMAJOR:16/little>>,
                   0, 0, 0,
                   encode_u30(length(Strings) + 1),
                   [encode_string(S) || S <- Strings],
                   0, 0, 0, 0, 0, 0, 0, 0])
        end,
    Segment = MakeSegment(["", "foo", "blorp"]),
    Expect = MakeSegment(["AZ", "AfooZ", "AblorpZ"]),
    Fun = fun(String) -> [$A, String, $Z] end,
    Expect = iolist_to_binary(stringmap(Fun, Segment)),
    ok.
