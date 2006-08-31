-module(eswf_amf).
-export([from_amf/1, from_amf/3]).
-export([from_amf_utf8/2, from_amf_dict/4, from_amf_array/4]).
-export([to_amf/1]).
-export([to_amf_dict/2, to_amf_array/3]).

-export([test/0]).

-define(DOUBLE, 0).
-define(BOOL, 1).
-define(UTF8, 2).
-define(OBJECT, 3).
-define(MOVIECLIP, 4).
-define(NULL, 5).
-define(UNDEFINED, 6).
-define(REFERENCE, 7).
-define(MIXEDARRAY, 8).
-define(ENDOFOBJECT, 9).
-define(ARRAY, 10).
-define(DATE, 11).
-define(LONGUTF8, 12).
-define(UNSUPPORTED, 13).
-define(RECORDSET, 14).
-define(XML, 15).
-define(TYPEDOBJECT, 16).
-define(AMF3DATA, 17).

-record(amf, {objects=nil, objectcount=0}).

get_ref({ref, RefNum}, S) ->
    gb_trees:get(RefNum, S#amf.objects).

alloc_refnum(S) ->
    RefNum = S#amf.objectcount,
    {RefNum, S#amf{objectcount=1 + RefNum}}.

make_ref(RefNum, Object, S) ->
    Tree = gb_trees:insert(RefNum, Object, S#amf.objects),
    {{ref, RefNum}, S#amf{objects=Tree}}.

from_amf(Data) ->
    <<Code, Rest/binary>> = iolist_to_binary(Data),
    from_amf(Code, Rest, #amf{objects=gb_trees:empty()}).

from_amf(Code, <<Float:64/float, Data/binary>>, S) when Code == ?DOUBLE ->
    {Float, Data, S};
from_amf(Code, <<0, Data/binary>>, S) when Code == ?BOOL ->
    {false, Data, S};
from_amf(Code, <<_, Data/binary>>, S) when Code == ?BOOL ->
    {true, Data, S};
from_amf(Code, Data, S) when Code == ?UTF8 ->
    {String, Rest} = from_amf_utf8(Data, 16),
    {String, Rest, S};
from_amf(Code, Data, S) when Code == ?OBJECT ->
    from_amf_dict({object}, Data, S);
from_amf(Code, Data, S) when Code == ?NULL ->
    {null, Data, S};
from_amf(Code, Data, S) when Code == ?UNDEFINED ->
    {undefined, Data, S};
from_amf(Code, Data, S) when Code == ?UNSUPPORTED ->
    {unsupported, Data, S};
from_amf(Code, <<RefNum:16, Data/binary>>, S) when Code == ?REFERENCE ->
    {{ref, RefNum}, Data, S};
from_amf(Code, <<Size:32, Data/binary>>, S) when Code == ?MIXEDARRAY ->
    from_amf_dict({mixed, Size}, Data, S);
from_amf(Code, Data, S) when Code == ?ENDOFOBJECT ->
    {stop, Data, S};
from_amf(Code, <<Size:32, Data/binary>>, S) when Code == ?ARRAY ->
    from_amf_array({array}, Size, Data, S);
from_amf(Code, Data, S) when Code == ?DATE ->
    <<TS:64/float, TZ:16/signed, Rest/binary>> = Data,
    {{date, TS, TZ}, Rest, S};
from_amf(Code, Data, S) when Code == ?LONGUTF8 ->
    {String, Rest} = from_amf_utf8(Data, 32),
    {String, Rest, S};
from_amf(Code, Data, S) when Code == ?XML ->
    {String, Rest} = from_amf_utf8(Data, 32),
    {{xml, String}, Rest, S};
from_amf(Code, Data, S) when Code == ?TYPEDOBJECT ->
    {Type, DictAndRest} = from_amf_utf8(Data, 16),
    from_amf_dict({typed, Type}, DictAndRest, S).
%% from_amf(Code, Data) when Code == ?AMF3DATA ->
%%    {Chunk, Rest} = eswf_amf3:from_amf3(Data),
%%    {{amf3, Chunk}, Rest}.


from_amf_array(Tag, Size, Data, S) ->
    from_amf_array(Tag, Size, Data, S, []).

from_amf_array(Tag, 0, Data, S, Acc) ->
    Array = erlang:append_element(Tag, lists:reverse(Acc)),
    {Array, Data, S};
from_amf_array(Tag, Size, <<Code, Data/binary>>, S, Acc) ->
    {Item, Rest, S1} = from_amf(Code, Data, S),
    from_amf_array(Tag, Size - 1, Rest, S1, [Item | Acc]).

from_amf_utf8(Data, Bits) ->
    <<Length:Bits, StringAndRest/binary>> = Data,
    <<String:Length/binary, Rest/binary>> = StringAndRest,
    {String, Rest}.
    
from_amf_dict(Tag, Data, S) ->
    from_amf_dict(Tag, Data, S, []).

from_amf_dict(Tag, Data, S, Acc) ->
    {Key, <<Code, ValueAndRest/binary>>} = from_amf_utf8(Data, 16),
    {Value, Rest, S1} = from_amf(Code, ValueAndRest, S),
    case {Key, Value} of 
	{<<>>, stop} ->
	    Dict = erlang:append_element(Tag, lists:reverse(Acc)),
	    {Dict, Rest, S1};
	Pair ->
	    from_amf_dict(Tag, Rest, S1, [Pair | Acc])
    end.

to_amf(Float) when is_float(Float) ->
    <<?DOUBLE, Float:64/float>>;
to_amf(Integer) when is_integer(Integer) ->
    <<?DOUBLE, Integer:64/float>>;
to_amf(Bool) when Bool == true ->
    <<?BOOL, 1>>;
to_amf(Bool) when Bool == false ->
    <<?BOOL, 0>>;
to_amf(String) when is_binary(String) ->
    Size = size(String),
    Bits = if Size > 65535 ->
		   32;
	      true ->
		   16
	   end,
    [<<?UTF8, Size:Bits>>, String];
to_amf({object, Dict}) ->
    [<<?OBJECT>> | to_amf_dict(Dict, [])];
%% to_amf({movieclip, _}) ->
%%    [?MOVIECLIP, no_idea];
to_amf(null) ->
    <<?NULL>>;
to_amf(undefined) ->
    <<?UNDEFINED>>;
to_amf(unsupported) ->
    <<?UNSUPPORTED>>;
to_amf({reference, Ref}) ->
    <<?REFERENCE, Ref:16>>;
to_amf({mixed, Size, Dict}) ->
    [<<?MIXEDARRAY, Size:32>> | to_amf_dict(Dict, [])];
to_amf({array, Size, Array}) ->
    [<<?ARRAY, Size:32>>, to_amf_array(Size, Array, [])];
to_amf({date, Timestamp, Timezone}) ->
    <<?DATE, Timestamp:64/float, Timezone:16/signed>>;
to_amf({xml, String}) ->
    Size = size(String),
    [<<?XML, Size:32>>, String];
to_amf({typed, Name, Dict}) ->
    Size = size(Name),
    [<<?TYPEDOBJECT, Size:16>>, Name | to_amf_dict(Dict, [])];
to_amf({amf3, Chunk}) ->
    [<<?AMF3DATA>>, eswf_amf3:to_amf3(Chunk)].

to_amf_dict([], Acc) ->
    lists:reverse([<<0:16, ?ENDOFOBJECT>> | Acc]);
to_amf_dict([{Key, Value} | Rest], Acc) ->
    to_amf_dict(Rest, [to_amf(Key), to_amf(Value) | Acc]).

to_amf_array(0, [], Acc) ->
    lists:reverse(Acc);
to_amf_array(Size, [Obj | Rest], Acc) ->
    to_amf_array(Size - 1, Rest, [to_amf(Obj) | Acc]). 


test() ->
    ok.
