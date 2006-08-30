-module(eswf_amf3).
-export([from_amf3/1, from_amf3/3]).
-export([from_amf3_integer/1, from_amf3_string/2,
	 from_amf3_array/3, from_amf3_object/3]).
-export([get/2]).

-define(UNDEFINED, 0).
-define(NULL, 1).
-define(FALSE, 2).
-define(TRUE, 3).
-define(INTEGER, 4).
-define(NUMBER, 5).
-define(STRING, 6).
-define(XML, 7).
-define(DATE, 8).
-define(ARRAY, 9).
-define(OBJECT, 10).
-define(XML_STRING, 11).

-define(OBJECT_OBJ_INLINE, 1).
-define(OBJECT_CLASS_INLINE, 2).
-define(OBJECT_PROP_DEF, 4).
-define(OBJECT_PROP_SERIAL, 8).

-define(INTEGER_MAX, 268435455).
-define(INTEGER_MIN, -268435456).

-record(amf3, {strings=nil, objects=nil, stringcount=0, objectcount=0}).

from_amf3(Data) ->
    <<Code, Rest/binary>> = iolist_to_binary(Data),
    {Value, Rest, State} = from_amf3(Code, Rest, #amf3{}),
    {{amf3, Value, State}, Rest}.
       
from_amf3(Code, Data, S) when Code == ?UNDEFINED ->
    {undefined, Data, S};
from_amf3(Code, Data, S) when Code == ?NULL ->
    {null, Data, S};
from_amf3(Code, Data, S) when Code == ?FALSE ->
    {false, Data, S};
from_amf3(Code, Data, S) when Code == ?TRUE ->
    {true, Data, S};
from_amf3(Code, Data, S) when Code == ?INTEGER ->
    {Num, Rest} = from_amf3_integer(Data),
    {Num, Rest, S};
from_amf3(Code, Data, S) when Code == ?NUMBER ->
    <<Float:64/float, Rest/binary>> = Data,
    {Float, Rest, S};
from_amf3(Code, Data, S) when Code == ?STRING ->
    from_amf3_string(Data, S);
from_amf3(Code, Data, S) when Code == ?XML ->
    {String, Rest, S1} = from_amf3_string(Data, S),
    {{xml, String}, Rest, S1};
from_amf3(Code, Data, S) when Code == ?DATE ->
    {Type, Rest1} = from_amf3_integer(Data),
    case Type band 1 of
	0 ->
	    <<Float:64/float, Rest2/binary>> = Rest1,
	    {Ref, S1} = add_object({date, Float}, S),
	    {Ref, Rest2, S1};
	1 ->
	    {{ref, Type bsr 1}, Rest1, S}
    end;
from_amf3(Code, Data, S) when Code == ?ARRAY ->
    {Type, Rest1} = from_amf3_integer(Data),
    Length = Type bsr 1,
    case Type band 1 of
	0 ->
	    from_amf3_array(Length, Rest1, S);
	1 ->
	    {{ref, Length}, Rest1, S}
    end;
from_amf3(Code, Data, S) when Code == ?OBJECT ->
    {Type, Rest1} = from_amf3_integer(Data),
    TypeInfo = Type bsr 1,
    case Type band 1 of
	0 ->
	    from_amf3_object(TypeInfo, Rest1, S);
	1 ->
	    {{ref, TypeInfo}, Rest1, S}
    end;
from_amf3(Code, Data, S) when Code == ?XML_STRING ->
    {Type, Rest1} = from_amf3_integer(Data),
    0 = Type band 1,
    Length = Type bsr 1,
    <<String:Length/binary, Rest2/binary>> = Rest1,
    {{xml, String}, Rest2, S}.


from_amf3_integer(Data) ->
    from_amf3_integer(Data, 0, 0).

from_amf3_integer(<<1:1, Num:7, Data/binary>>, Result, N) when N < 3 ->
    from_amf3_integer(Data, (Result bsl 7) bor Num, N + 1);
from_amf3_integer(<<0:0, Num:7, Data/binary>>, Result, N) when N < 3 ->
    {(Result bsl 7) bor Num, Data};
from_amf3_integer(<<Byte, Data/binary>>, Result, _N) ->
    Result1 = (Result bsl 8) bor Byte,
    Result3 = case Result1 band 16#10000000 of
		  16#10000000 ->
		      Extended = Result1 bor 16#e0000000,
		      <<Result2:32/signed>> = <<Extended:32>>,
		      Result2;
		  0 ->
		      Result1
	      end,
    {Result3, Data}.

from_amf3_string(Data, S) ->
    {Type, Rest} = from_amf3_integer(Data),
    Length = Type bsr 1,
    case Type band 1 of
	0 ->
	    <<String:Length/binary, Rest1/binary>> = Rest,
	    S1 = add_string(String, S),
	    {String, Rest1, S1};
	1 ->
	    {get_string(Length, S), Rest, S}
    end.


from_amf3_array(Length, <<1, Data/binary>>, S) ->
    {RefNum, S1} = new_object(S),
    {Array, Rest, S2} = from_amf3_array(Length, Data, S1, [], Length),
    {Ref, S3} = finish_object(RefNum, Array, S2),
    {Ref, Rest, S3}.

from_amf3_array(0, Data, S, Acc, OrigLength) ->
    {{array, OrigLength, lists:reverse(Acc)}, Data, S};
from_amf3_array(Length, Data, S, Acc, OrigLength) ->
    <<Code, Data1/binary>> = Data,
    {Item, Rest, S1} = from_amf3(Code, Data1, S),
    from_amf3_array(Length - 1, Rest, S1, [Item | Acc], OrigLength).

from_amf3_object(TypeInfo, Data, S) ->
    {RefNum, S1} = new_object(S),
    {Class, Rest1, S2} = from_amf3_object_class(TypeInfo, Data, S1),
    ObjType = (TypeInfo bsr 1) band 3,
    {Object, Rest2, S3} = from_amf3_object_object(ObjType, Class, Rest1, S2),
    {Ref, S4} = finish_object(RefNum, Object, S3),
    {Ref, Rest2, S4}.

from_amf3_object_class(TypeInfo, Data, S) when (TypeInfo band 1) == 1 ->
    todo,
    {TypeInfo, Data, S}.

from_amf3_object_object(ObjType, Class, Data, S) ->
    todo,
    {{Class, ObjType}, Data, S}.

add_object(Object, S) ->
    OldTree = case S#amf3.objects of
		  nil ->
		      gb_trees:from_orddict([]);
		  _Tree ->
		      _Tree
	      end,
    RefNum = S#amf3.objectcount,
    Tree = gb_trees:insert(RefNum, Object, OldTree),
    {{ref, RefNum}, S#amf3{objects=Tree, objectcount=1 + RefNum}}.

new_object(S) ->
    RefNum = S#amf3.objectcount,
    {RefNum, S#amf3{objectcount=1 + RefNum}}.

finish_object(RefNum, Object, S) ->
    OldTree = case S#amf3.objects of
		  nil ->
		      gb_trees:from_orddict([]);
		  _Tree ->
		      _Tree
	      end,
    Tree = gb_trees:insert(RefNum, Object, OldTree),
    {{ref, RefNum}, S#amf3{objects=Tree}}.

add_string(String, S) ->
    OldTree = case S#amf3.strings of
		  nil ->
		      gb_trees:from_orddict([]);
		  _Tree ->
		      _Tree
	      end,
    RefNum = S#amf3.stringcount,
    Tree = gb_trees:insert(RefNum, String, OldTree),
    {String, S#amf3{strings=Tree, stringcount=1 + RefNum}}.

get_object(RefNum, S) ->
    gb_trees:get(RefNum, S#amf3.objects).

get_string(RefNum, S) ->
    gb_trees:get(RefNum, S#amf3.strings).

get({ref, RefNum}, S) ->
    get_object(RefNum, S);
get(Object, _) ->
    Object.
