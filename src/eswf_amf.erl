-module(eswf_amf).
-export([from_amf/1, from_amf/2]).
-export([from_amf_utf8/2, from_amf_dict/2, from_amf_array/3]).
-export([to_amf/1]).
-export([to_amf_dict/2, to_amf_array/3]).

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

from_amf(Data) ->
    <<Code, Rest/binary>> = iolist_to_binary(Data),
    from_amf(Code, Rest).

from_amf(Code, Data) when Code == ?DOUBLE ->
    <<Float:64/float, Rest/binary>> = Data,
    {Float, Rest};
from_amf(Code, Data) when Code == ?BOOL ->
    <<Char, Rest/binary>> = Data,
    V = case Char of
	    0 ->
		false;
	    _ ->
		true
	end,
    {V, Rest};
from_amf(Code, Data) when Code == ?UTF8 ->
    from_amf_utf8(Data, 16);
from_amf(Code, Data) when Code == ?OBJECT ->
    {Dict, Rest} = from_amf_dict(Data, []),
    {{object, Dict}, Rest};
%% from_amf(Code, Data) when Code == ?MOVIECLIP ->
%%    {no_idea, Rest};
from_amf(Code, Data) when Code == ?NULL ->
    {null, Data};
from_amf(Code, Data) when Code == ?UNDEFINED ->
    {undefined, Data};
from_amf(Code, Data) when Code == ?UNSUPPORTED ->
    {unsupported, Data};
from_amf(Code, Data) when Code == ?REFERENCE ->
    <<Ref:16, Rest/binary>> = Data,
    {{reference, Ref}, Rest};
from_amf(Code, Data) when Code == ?MIXEDARRAY ->
    <<Size:32, Rest/binary>> = Data,
    {Dict, Rest} = from_amf_dict(Data, []),
    {{mixed, Size, Dict}, Rest};
from_amf(Code, Data) when Code == ?ENDOFOBJECT ->
    {stop, Data};
from_amf(Code, Data) when Code == ?ARRAY ->
    <<Size:32, Rest/binary>> = Data,
    {Array, Rest} = from_amf_array(Size, Data, []),
    {{array, Size, Array}, Rest};
from_amf(Code, Data) when Code == ?DATE ->
    <<Timestamp:64/float, Timezone:16/signed, Rest/binary>> = Data,
    {{date, Timestamp, Timezone}, Rest};
from_amf(Code, Data) when Code == ?LONGUTF8 ->
    from_amf_utf8(Data, 32);
%% from_amf(Code, Data) when Code == ?RECORDSET ->
%%    {no_idea, Rest};
from_amf(Code, Data) when Code == ?XML ->
    {String, Rest} = from_amf_utf8(Data, 32),
    {{xml, String}, Rest};
from_amf(Code, Data) when Code == ?TYPEDOBJECT ->
    {Type, DictAndRest} = from_amf_utf8(Data, 16),
    {Dict, Rest} = from_amf_dict(DictAndRest, []),
    {{typed, Type, Dict}, Rest}.


from_amf_array(0, Data, Acc) ->
    {lists:reverse(Acc), Data};
from_amf_array(Size, Data, Acc) ->
    {Obj, Rest} = from_amf(Data),
    from_amf_array(Size - 1, Rest, [Obj | Acc]).

from_amf_utf8(Data, Bits) ->
    <<Length:Bits, StringAndRest/binary>> = Data,
    <<String:Length/binary, Rest/binary>> = StringAndRest,
    {String, Rest}.
    
from_amf_dict(Data, Acc) ->
    {Key, ValueAndRest} = from_amf_utf8(Data, 16),
    {Value, Rest} = from_amf(ValueAndRest),
    case {Key, Value} of 
	{<<>>, stop} ->
	    {lists:reverse(Acc), Rest};
	Pair ->
	    from_amf_dict(Rest, [Pair | Acc])
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
    [<<?TYPEDOBJECT, Size:16>>, Name | to_amf_dict(Dict, [])].

to_amf_dict([], Acc) ->
    lists:reverse([<<0:16, ?ENDOFOBJECT>> | Acc]);
to_amf_dict([{Key, Value} | Rest], Acc) ->
    to_amf_dict(Rest, [to_amf(Key), to_amf(Value) | Acc]).

to_amf_array(0, [], Acc) ->
    lists:reverse(Acc);
to_amf_array(Size, [Obj | Rest], Acc) ->
    to_amf_array(Size - 1, Rest, [to_amf(Obj) | Acc]). 
