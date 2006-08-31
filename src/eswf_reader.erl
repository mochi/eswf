%% @author Bob Ippolito <bob@redivi.com>
%% @copyright 2006 Bob Ippolito

%% @doc Streaming CPS-style reading functions for files and binaries with a 
%%      zlib wrapper.

%% @type iolist() = [char() | binary() | iolist()]
%% @type iodata() = iolist() | binary()
%% @type readable() = string() | binary()
%%                    | {file, File}
%%                    | {inflate, readable()}

-module(eswf_reader).

-export([reader/1, test/0]).

%% @spec reader(readable()) -> Reader
%% @doc Return a function Reader0/1 such that Reader0(N) returns:
%%          {Reader1, binary()} |
%%          {Reader1, eof}.
%%
%%      Like other file reading APIs, the length of the returned binary will
%%      be equal to the number of bytes asked for unless eof is reached.
%%
%%      Reader0(close) may be called to ensure that any state associated with
%%      the reader is closed premature to stream end.
reader(<<>>) -> 
    fun (_) ->
	    {reader(<<>>), eof}
    end;
reader(Path) when is_list(Path) ->
    case file:open(Path, [read, read_ahead]) of
	{ok, Pid} ->
	    reader({file, Pid})
    end;
reader(Binary) when is_binary(Binary) ->
    fun (close) ->
	    {reader(<<>>), eof};
	(Bytes) when Bytes =< size(Binary) ->
	    <<Data:Bytes/binary, Rest/binary>> = Binary,
	    {reader(Rest), Data};
	(_) ->
	    {reader(<<>>), Binary}
    end;
reader({file, File}) ->
    fun (close) ->
	    file:close(File),
	    {reader(<<>>), eof};
	(Bytes) ->
	    case file:read(File, Bytes) of
		{ok, Data} ->
		    {reader({file, File}), iolist_to_binary(Data)};
		eof ->
		    file:close(File),
		    {reader(<<>>), eof}
	    end
    end;
reader({inflate, Reader}) ->
    Z = zlib:open(),
    zlib:inflateInit(Z),
    next_inflate(Z, Reader, 0, []).
    

next_inflate(Z, Reader, BytesTotal, Acc) ->
    fun (close) ->
	    zlib:close(Z),
	    Reader(close),
	    {reader(<<>>), eof};
	(Bytes) when Bytes =< BytesTotal ->
	    [Data | Rest] = chunk_bytes(Acc, Bytes, Bytes, []),
	    Next = next_inflate(Z, Reader, BytesTotal - Bytes, Rest),
	    {Next, Data};
	(Bytes) ->
	    case Reader(4096) of 
		{_, eof} ->
		    zlib:inflateEnd(Z),
		    zlib:close(Z),
		    Reader1 = reader(iolist_to_binary(Acc)),
		    Reader1(Bytes);
		{Reader1, RawData} ->
		    Data = zlib:inflate(Z, RawData),
		    NextTotal = BytesTotal + iolist_size(Data),
		    Next = next_inflate(Z, Reader1, NextTotal, [Data | Acc]),
		    Next(Bytes)
	    end
    end.

chunk_bytes([First | Rest], BytesTotal, BytesLeft, Acc) ->
    FirstSize = iolist_size(First),
    if FirstSize >= BytesLeft ->
	    AllData = iolist_to_binary(lists:reverse([First | Acc])),
	    <<Data:BytesTotal/binary, LeftOver/binary>> = AllData,
	    [Data, LeftOver | Rest];
	true ->
	    chunk_bytes(Rest, BytesTotal, BytesLeft - FirstSize, [First | Acc])
    end.

%% @spec test() -> ok
%% @doc Run tests for eswf_reader.
test() ->
    ok = test(reader_binary),
    ok = test(inflate_binary),
    ok.

test(reader_binary) ->
    R0 = reader(<<"foobarbaz">>),
    {R1, <<"foo">>} = R0(3),
    {R2, <<"barb">>} = R1(4),
    {R3, <<"az">>} = R2(10),
    {R4, eof} = R3(1),
    {_, eof} = R4(100),
    ok;
test(inflate_binary) ->
    R0 = reader(iolist_to_binary([<<"test">>, zlib:compress(<<"foobarbaz">>)])),
    {R1, <<"test">>} = R0(4),
    R2 = reader({inflate, R1}),
    {R3, <<"foo">>} = R2(3),
    {R4, <<"barb">>} = R3(4),
    {R5, <<"az">>} = R4(10),
    {R6, eof} = R5(1),
    {_, eof} = R6(100),
    ok.
