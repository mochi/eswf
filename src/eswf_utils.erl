%% @copyright 2007 Mochi Media, Inc.
%% @author Matthew Dempsky <matthew@mochimedia.com>

%% @doc The <code>eswf_utils</code> module includes a collection of
%% utility functions used by other eswf functions.

-module(eswf_utils).
-author('matthew@mochimedia.com').


-export([edit/2]).

%% @spec edit(Binary::binary(), Edits) -> iodata()
%% where
%%       Edits = [{Begin::int(), End::int(), NewData::iodata()}]
%%
%% @doc Performs a series of subsequence substitutions on a binary
%% value.  For each tuple <code>{Begin, End, NewData}</code> in
%% <code>Edits</code>, the byte range [<code>Begin</code>,
%% <code>End</code>) in <code>Binary</code> is replaced by
%% <code>NewData</code>.
%%
%% The <code>Begin</code> and <code>End</code> values are always
%% interpreted as offsets into the original binary value.  Byte ranges
%% in <code>Edits</code> must not overlap and must occur in
%% monotonically increasing order.
edit(Binary, Edits) ->
    edit(Binary, Edits, 0, []).

edit(Binary, [], Offset, Acc) ->
    <<_Skip:Offset/binary, Rest/binary>> = Binary,
    lists:reverse(Acc, Rest);
edit(Binary, [{Begin, End, NewData} | Edits], Offset, Acc) ->
    N = Begin - Offset,
    <<_Skip:Offset/binary, Between:N/binary, _Rest/binary>> = Binary,
    NewAcc = [NewData, Between | Acc],
    edit(Binary, Edits, End, NewAcc).
