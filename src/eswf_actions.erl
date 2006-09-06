%% @author Bob Ippolito <bob@redivi.com>
%% @copyright 2006 Bob Ippolito

%% @doc SWF Actions (AS1 VM).

-module(eswf_actions).
-export([encactions/1, encaction/1]).

%% @type iolist() = [char() | binary() | iolist()]
%% @type iodata() = iolist() | binary()

-define(ACTION_PUSH, 16#96).
-define(ACTION_GET_VARIABLE, 16#1C).
-define(ACTION_CALL_METHOD, 16#52).
-define(ACTION_PUSH_DUPLICATE, 16#4C).
-define(ACTION_GET_MEMBER, 16#4E).
-define(ACTION_SET_MEMBER, 16#4F).
-define(ACTION_POP, 16#17).
-define(ACTION_GET_URL, 16#83).
-define(ACTION_DEFINE_FUNCTION, 16#9B).

-define(PUSH_STRING, 0).
-define(PUSH_NULL, 2).
-define(PUSH_UNDEFINED, 3).
-define(PUSH_REGISTER, 4).
-define(PUSH_BOOLEAN, 5).
-define(PUSH_FLOAT, 6).
-define(PUSH_INTEGER, 7).
-define(PUSH_CONSTANT, 8).
-define(PUSH_CONSTANT2, 9).

encpush(String) when is_list(String) ->
    [?PUSH_STRING, String, 0];
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
encaction(get_variable) ->
    <<?ACTION_GET_VARIABLE>>;
encaction(call_method) ->
    <<?ACTION_CALL_METHOD>>;
encaction(push_duplicate) ->
    <<?ACTION_PUSH_DUPLICATE>>;
encaction(get_member) ->
    <<?ACTION_GET_MEMBER>>;
encaction(set_member) ->
    <<?ACTION_SET_MEMBER>>;
encaction(pop) ->
    <<?ACTION_POP>>;
encaction({define_function, Name, Params, Actions}) ->
    {ParamCount, RevParams} = lists:mapfoldl(fun (Param, {C, Acc}) ->
						  {1 + C, [[Param, 0] | Acc]}
					  end, {0, []}, Params),
    ActionsBin = iolist_to_binary(encactions(Actions)),
    ActionsLen = size(ActionsBin),
    Body = [Name, 0, <<ParamCount:16/little>>, lists:reverse(RevParams),
	    <<ActionsLen:16/little>>, ActionsBin],
    encaction(?ACTION_DEFINE_FUNCTION, Body);
encaction({get_url, Url, Target}) ->
    encaction(?ACTION_GET_URL, [Url, 0, Target, 0]);
encaction({raw, Code}) ->
    <<Code>>;
encaction({raw, Code, Body}) ->
    encaction(Code, Body).

%% @spec encactions(Actions::List) -> iodata()
%% @doc Convert a list of high-level SWF actions to iodata().
encactions(Actions) ->
    [[encaction(X) || X <- Actions], 0].
