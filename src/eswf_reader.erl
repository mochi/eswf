-module(eswf_reader).

-export([reader/1]).

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
