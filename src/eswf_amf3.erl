%% @copyright 2008 Mochi Media, Inc.
%% @author Matthew Dempsky <matthew@mochimedia.com>
%%
%% @doc Simple AMF3 encoder. Does not provide a surjective mapping to
%% valid AMF3 encodings. Does not try to share object references.
%%
%% @reference See the <a
%% href="http://download.macromedia.com/pub/labs/amf/amf3_spec_121207.pdf">AMF3
%% specification</a> for details on the AMF3 format.

-module(eswf_amf3).
-author("Matthew Dempsky <matthew@mochimedia.com>").

-export([encode/1]).

-record(state, {strings = gb_trees:empty()}).

%% @spec encode(Object::amfterm()) -> iodata()
%% where amfterm() = undefined | null | false | true
%%                   | int() | float() | amfstring()
%%                   | [amfterm()]
%%                   | {struct, [{amfstring(), amfterm()}]}
%%                   | {bytes, binary()}
%%       amfstring() = binary() | {string, iodata()}
%%
%% @doc Encode using AMF3.
encode(Object) ->
    {Result, _} = encode(Object, #state{}),
    Result.

encode(undefined, State) ->
    {0, State};
encode(null, State) ->
    {1, State};
encode(false, State) ->
    {2, State};
encode(true, State) ->
    {3, State};
encode(N, State) when is_integer(N), N >= 0, N < (1 bsl 29) ->
    {[4, encode_u29(N)], State};
encode(N, State) when is_integer(N), N < 0, N >= -(1 bsl 28) ->
    {[4, encode_u29(N + (1 bsl 29))], State};
encode(N, State) when is_integer(N) ->
    encode(float(N), State);
encode(Double, State) when is_float(Double) ->
    {<<5, Double/float>>, State};
encode(String, State) when is_binary(String) ->
    {Bytes, NewState} = encode_string(String, State),
    {[6, Bytes], NewState};
encode({string, String}, State) ->
    {Bytes, NewState} = encode_string(String, State),
    {[6, Bytes], NewState};
encode(Array, State) when is_list(Array) ->
    Header = [9, encode_u29(2 * length(Array) + 1), 1],
    {Body, NewState} = lists:mapfoldl(fun encode/2, State, Array),
    {[Header, Body], NewState};
encode({struct, Fields0}, State) when is_list(Fields0) ->
    Fields = lists:sort(Fields0),
    Fun = fun({Key, Value}, State0) ->
                  case encode_string(Key, State0) of
                      {1, _} ->
                          %% The AMF3 format can't handle
                          %% empty strings as keys in sparse
                          %% arrays.
                          {[], State0};
                      {KeyBytes, State1} ->
                          {ValueBytes, State2} = encode(Value, State1),
                          {[KeyBytes, ValueBytes], State2}
                  end
          end,
    {Body, NewState} = lists:mapfoldl(Fun, State, Fields),
    {[9, 1, Body, 1], NewState};
encode({bytes, ByteArray}, State) when is_binary(ByteArray) ->
    {[12, encode_u29(2 * size(ByteArray) + 1), ByteArray], State}.

encode_string(<<>>, State) ->
    {1, State};
encode_string(String, State) when is_binary(String) ->
    %% @todo Ensure <code>String</code> is valid UTF8?
    case find(String, State#state.strings) of
        {ok, N} when is_integer(N) ->
            {encode_u29(2 * N), State};
        none ->
            Encoding = [encode_u29(2 * size(String) + 1), String],
            NewState = State#state{strings = add(String, State#state.strings)},
            {Encoding, NewState}
    end;
encode_string(String, State) when is_atom(String) ->
    encode_string(atom_to_binary(String, utf8), State);
encode_string(String, State) ->
    encode_string(iolist_to_binary(String), State).

encode_u29(N) when is_integer(N), N >= 0, N < (1 bsl 29) ->
    %% Ugh, the 4-byte case has to make this a pain...
    if
        N < (1 bsl 7) ->
            N;
        N < (1 bsl 15) ->
            <<1:1, (N bsr 7):7, 0:1, N:7>>;
        N < (1 bsl 23) ->
            <<1:1, (N bsr 14):7, 1:1, (N bsr 7):7, 0:1, N:7>>;
        true ->
            <<1:1, (N bsr 22):7, 1:1, (N bsr 15):7, 1:1, (N bsr 8):7, N:8>>
    end.


find(Key, Strings) ->
    case gb_trees:lookup(Key, Strings) of
        {value, Val} ->
            Val;
        none ->
            none
    end.

add(Key, Strings) ->
    gb_trees:insert(Key, gb_trees:size(Strings), Strings).
