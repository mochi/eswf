%% @author Bob Ippolito <bob@redivi.com>
%% @copyright 2006 Bob Ippolito

%% @doc SWF Actions (AS1 VM).

-module(eswf_actions).
-export([encactions/1, encaction/1]).

%% @type iolist() = [char() | binary() | iolist()]
%% @type iodata() = iolist() | binary()

-define(ACTION_INIT_ARRAY, 16#42).
-define(ACTION_INIT_OBJECT, 16#43).
-define(ACTION_EQUALS, 16#0E).
-define(ACTION_STRICT_EQUALS, 16#66).
-define(ACTION_AND, 16#10).
-define(ACTION_OR, 16#11).
-define(ACTION_NOT, 16#12).
-define(ACTION_STRING_EQUALS, 16#13).
-define(ACTION_STRING_LENGTH, 16#14).
-define(ACTION_STRING_EXTRACT, 16#15).
-define(ACTION_STRING_ADD, 16#21).
-define(ACTION_STRING_LESS, 16#29).
-define(ACTION_PUSH, 16#96).
-define(ACTION_GET_VARIABLE, 16#1C).
-define(ACTION_SET_VARIABLE, 16#1D).
-define(ACTION_CALL_METHOD, 16#52).
-define(ACTION_CALL_FUNCTION, 16#3D).
-define(ACTION_NEW_OBJECT, 16#40).
-define(ACTION_PUSH_DUPLICATE, 16#4C).
-define(ACTION_GET_MEMBER, 16#4E).
-define(ACTION_SET_MEMBER, 16#4F).
-define(ACTION_POP, 16#17).
-define(ACTION_GET_URL, 16#83).
-define(ACTION_DEFINE_FUNCTION, 16#9B).
-define(ACTION_GET_TIME, 16#34).
-define(ACTION_RETURN, 16#3E).
-define(ACTION_IF, 16#9D).
-define(ACTION_STACK_SWAP, 16#4D).
-define(ACTION_MULTIPLY, 16#0C).
-define(ACTION_SUBTRACT, 16#0B).
-define(ACTION_GREATER, 16#67).
-define(ACTION_LESS, 16#0F).
-define(ACTION_DELETE, 16#3A).
-define(ACTION_GET_PROPERTY, 16#22).
-define(ACTION_SET_PROPERTY, 16#23).
-define(ACTION_STORE_REGISTER, 16#87).
-define(ACTION_TRACE, 16#26).
-define(ACTION_ENUMERATE2, 16#55).
-define(ACTION_TYPEOF, 16#44).
-define(ACTION_DEFINE_LOCAL, 16#3C).
-define(ACTION_DEFINE_LOCAL2, 16#41).
-define(ACTION_INSTANCE_OF, 16#54).
-define(ACTION_TO_NUMBER, 16#4A).
-define(ACTION_TO_STRING, 16#4B).
-define(ACTION_INCREMENT, 16#50).
-define(ACTION_DECREMENT, 16#51).
-define(ACTION_BIT_AND, 16#60).
-define(ACTION_BIT_OR, 16#61).
-define(ACTION_BIT_XOR, 16#62).
-define(ACTION_BIT_LSHIFT, 16#63).
-define(ACTION_BIT_RSHIFT, 16#64).
-define(ACTION_BIT_URSHIFT, 16#65).

-define(P_x, 0).
-define(P_y, 1).
-define(P_xscale, 2).
-define(P_yscale, 3).
-define(P_currentframe, 4).
-define(P_totalframes, 5).
-define(P_alpha, 6).
-define(P_visible, 7).
-define(P_width, 8).
-define(P_height, 9).
-define(P_rotation, 10).
-define(P_target, 11).
-define(P_framesloaded, 12).
-define(P_name, 13).
-define(P_droptarget, 14).
-define(P_url, 15).
-define(P_highquality, 16).
-define(P_focusrect, 17).
-define(P_soundbuftime, 18).
-define(P_quality, 19).
-define(P_xmouse, 20).
-define(P_ymouse, 21).

-define(PUSH_STRING, 0).
-define(PUSH_NULL, 2).
-define(PUSH_UNDEFINED, 3).
-define(PUSH_REGISTER, 4).
-define(PUSH_BOOLEAN, 5).
-define(PUSH_FLOAT, 6).
-define(PUSH_INTEGER, 7).
-define(PUSH_CONSTANT, 8).
-define(PUSH_CONSTANT2, 9).

revlen([], Length, Acc) ->
    {Length, Acc};
revlen([Item | Rest], Length, Acc) ->
    revlen(Rest, 1 + Length, [Item | Acc]).

revlen(List) ->
    revlen(List, 0, []).

revpairs([], Length, Acc) ->
    {Length, Acc};
revpairs([{Key, Value} | Rest], Length, Acc) ->
    revpairs(Rest, 1 + Length, [Key, Value | Acc]).


revpairs(Pairs) ->
    revpairs(Pairs, 0, []).

propvalue(x) ->
    ?P_x;
propvalue(y) ->
    ?P_y;
propvalue(xscale) ->
    ?P_xscale;
propvalue(yscale) ->
    ?P_yscale;
propvalue(currentframe) ->
    ?P_currentframe;
propvalue(totalframes) ->
    ?P_totalframes;
propvalue(alpha) ->
    ?P_alpha;
propvalue(visible) ->
    ?P_visible;
propvalue(width) ->
    ?P_width;
propvalue(height) ->
    ?P_height;
propvalue(rotation) ->
    ?P_rotation;
propvalue(target) ->
    ?P_target;
propvalue(framesloaded) ->
    ?P_framesloaded;
propvalue(name) ->
    ?P_name;
propvalue(droptarget) ->
    ?P_droptarget;
propvalue(url) ->
    ?P_url;
propvalue(highquality) ->
    ?P_highquality;
propvalue(focusrect) ->
    ?P_focusrect;
propvalue(soundbuftime) ->
    ?P_quality;
propvalue(xmouse) ->
    ?P_xmouse;
propvalue(ymouse) ->
    ?P_ymouse.

encpush(String) when is_list(String) orelse is_binary(String) ->
    [?PUSH_STRING, String, 0];
encpush({prop, Atom}) ->
    Value = propvalue(Atom),
    <<Value>>;
encpush(r0) ->
    encpush({register, 0});
encpush(r1) ->
    encpush({register, 1});
encpush(r2) ->
    encpush({register, 2});
encpush(r3) ->
    encpush({register, 3});
encpush(null) ->
    <<?PUSH_NULL>>;
encpush(undefined) ->
    <<?PUSH_UNDEFINED>>;
encpush({register, Number}) ->
    <<?PUSH_REGISTER, Number>>;
encpush(true) ->
    <<?PUSH_BOOLEAN, 1>>;
encpush(false) ->
    <<?PUSH_BOOLEAN, 0>>;
encpush(Float) when is_float(Float) ->
    <<Left:32, Right:32>> = <<Float:64/float-little>>,
    <<?PUSH_FLOAT, Right:32, Left:32>>;
encpush(Int) when is_integer(Int) ->
    <<?PUSH_INTEGER, Int:32/little>>;
encpush({pool, Index}) when Index < 256 ->
    <<?PUSH_CONSTANT, Index>>;
encpush({pool, Index}) ->
    <<?PUSH_CONSTANT2, Index:16/little>>;
encpush({raw, Code, Value}) ->
    [Code, Value].

encaction(Code, Body) when Code >= 16#80 ->
    Size = iolist_size(Body),
    [<<Code, Size:16/little>>, Body].

%% @spec encaction(Any) -> iodata()
%% @doc Convert a high-level SWF action to iodata().
encaction({push, Values}) ->
    encaction(?ACTION_PUSH, [encpush(X) || X <- Values]);
encaction({get_property, Target, Prop}) ->
    [encaction({push, [Target, propvalue(Prop)]}), <<?ACTION_GET_PROPERTY>>];
encaction({set_property, Target, Prop, Value}) ->
    [encaction({push, [Target, propvalue(Prop), Value]}),
     <<?ACTION_GET_PROPERTY>>];
encaction(get_property) ->
    <<?ACTION_GET_PROPERTY>>;
encaction(set_property) ->
    <<?ACTION_SET_PROPERTY>>;
encaction(get_variable) ->
    <<?ACTION_GET_VARIABLE>>;
encaction(set_variable) ->
    <<?ACTION_SET_VARIABLE>>;
encaction(call_method) ->
    <<?ACTION_CALL_METHOD>>;
encaction(call_function) ->
    <<?ACTION_CALL_FUNCTION>>;
encaction(push_duplicate) ->
    <<?ACTION_PUSH_DUPLICATE>>;
encaction(get_member) ->
    <<?ACTION_GET_MEMBER>>;
encaction(set_member) ->
    <<?ACTION_SET_MEMBER>>;
encaction(pop) ->
    <<?ACTION_POP>>;
encaction(get_time) ->
    <<?ACTION_GET_TIME>>;
encaction(return) ->
    <<?ACTION_RETURN>>;
encaction(stack_swap) ->
    <<?ACTION_STACK_SWAP>>;
encaction(multiply) ->
    <<?ACTION_MULTIPLY>>;
encaction(subtract) ->
    <<?ACTION_SUBTRACT>>;
encaction(greater) ->
    <<?ACTION_GREATER>>;
encaction(less) ->
    <<?ACTION_LESS>>;
encaction(delete) ->
    <<?ACTION_DELETE>>;
encaction(trace) ->
    <<?ACTION_TRACE>>;
encaction(new_object) ->
    <<?ACTION_NEW_OBJECT>>;
encaction('and') ->
    <<?ACTION_AND>>;
encaction('or') ->
    <<?ACTION_OR>>;
encaction('not') ->
    <<?ACTION_NOT>>;
encaction(equals) ->
    <<?ACTION_EQUALS>>;
encaction(strict_equals) ->
    <<?ACTION_STRICT_EQUALS>>;
encaction(string_equals) ->
    <<?ACTION_STRING_EQUALS>>;
encaction(string_extract) ->
    <<?ACTION_STRING_EXTRACT>>;
encaction(string_add) ->
    <<?ACTION_STRING_ADD>>;
encaction(string_length) ->
    <<?ACTION_STRING_LENGTH>>;
encaction(string_less) ->
    <<?ACTION_STRING_LESS>>;
encaction(init_array) ->
    <<?ACTION_INIT_ARRAY>>;
encaction(init_object) ->
    <<?ACTION_INIT_OBJECT>>;
encaction(enumerate) ->
    <<?ACTION_ENUMERATE2>>;
encaction(typeof) ->
    <<?ACTION_TYPEOF>>;
encaction(define_local) ->
    <<?ACTION_DEFINE_LOCAL>>;
encaction(define_local2) ->
    <<?ACTION_DEFINE_LOCAL2>>;
encaction(instance_of) ->
    <<?ACTION_INSTANCE_OF>>;
encaction(to_string) ->
    <<?ACTION_TO_STRING>>;
encaction(to_number) ->
    <<?ACTION_TO_NUMBER>>;
encaction(increment) ->
    <<?ACTION_INCREMENT>>;
encaction(decrement) ->
    <<?ACTION_DECREMENT>>;
encaction(bit_and) ->
    <<?ACTION_BIT_AND>>;
encaction(bit_or) ->
    <<?ACTION_BIT_OR>>;
encaction(bit_xor) ->
    <<?ACTION_BIT_XOR>>;
encaction(bit_lshift) ->
    <<?ACTION_BIT_LSHIFT>>;
encaction(bit_rshift) ->
    <<?ACTION_BIT_RSHIFT>>;
encaction(bit_urshift) ->
    <<?ACTION_BIT_URSHIFT>>;
encaction(var) ->
    encaction(define_local2);
encaction({var, Name}) ->
    encaction([{push, [Name]},
	       define_local2]);
encaction({get_variable, Name}) ->
    encaction([{push, [Name]},
	       get_variable]);
encaction({new_object, Name}) ->
    encaction({new_object, Name, []});
encaction({new_object, Name, L}) ->
    encaction([{push, lists:reverse(L, [length(L), Name])},
	       new_object]);
encaction({enumerate, L}) ->
    encaction([enumerate,
	       {dowhile, [push_duplicate,
			  {push, [undefined]},
			  strict_equals,
			  {'if', L},
			  {push, [undefined]},
			  strict_equals,
			  'not']}]);
encaction({store_register, N}) ->
    encaction(?ACTION_STORE_REGISTER, <<N>>);
encaction({store_register, N, L}) ->
    encaction([L,
	       {store_register, N},
	       pop]);
encaction({init_array, Array}) ->
    {Length, RevArray} = revlen(Array),
    encaction([{push, RevArray},
	       {push, [Length]},
	       init_array]);
encaction({init_object, Pairs}) ->
    {Length, RevPairs} = revpairs(Pairs),
    encaction([{push, RevPairs},
	       {push, [Length]},
	       init_object]);
encaction({'if', Bytes}) when is_integer(Bytes) ->
    encaction(?ACTION_IF, <<Bytes:16/little>>);
encaction({'if', Actions}) when is_list(Actions) ->
    Encoded = [encaction(X) || X <- Actions],
    [encaction({'if', iolist_size(Encoded)}) | Encoded];
encaction({dowhile, Body}) when is_list(Body) ->
    Encoded = encaction(Body),
    JumpOffset = -(5 + iolist_size(Encoded)),
    [Encoded, encaction({'if', JumpOffset})];
encaction({define_function, Name, Params, Actions}) ->
    {ParamsList, ParamsCount} = lists:mapfoldl(fun (Param, Acc) ->
						       {[Param, 0], 1 + Acc}
					       end, 0, Params),
    ActionsList = encactions(Actions),
    ActionsSize = iolist_size(ActionsList),
    Body = [Name, 0, <<ParamsCount:16/little>>, ParamsList,
	    <<ActionsSize:16/little>>],
    Size = iolist_size(Body),
    [<<?ACTION_DEFINE_FUNCTION, Size:16/little>>, Body, ActionsList];
encaction({get_url, Url, Target}) ->
    encaction(?ACTION_GET_URL, [Url, 0, Target, 0]);
encaction({raw, Code}) ->
    <<Code>>;
encaction({raw, Code, Body}) ->
    encaction(Code, Body);
encaction(List) when is_list(List) ->
    [encaction(X) || X <- List].

%% @spec encactions(Actions::List) -> iodata()
%% @doc Convert a list of high-level SWF actions to iodata().
encactions(Actions) ->
    [[encaction(X) || X <- Actions], 0].
