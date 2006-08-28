-module(eswf_decode).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, code_change/3]).

start_link(Reader) ->
    gen_server:start_link(?MODULE, [Reader], []).

init(Reader) ->
    Reader = eswf_reader:reader(Reader),
    {ok, Reader}.

handle_call(Request, _From, State) ->
    {reply, Request, State}.

handle_cast(close, State) ->
    {noreply, State(close)}.

code_change(_OldVsn, State, _Extra) ->
    State.

terminate(_Reason, State) ->
    State(close),
    ok.
