-module(eswf_remoting).

from_remoting(<<0, 3, HeaderCount:16, HeadersAndRest/binary>>) ->
    {Headers, <<BodyCount:16, BodyData/binary>>} = 
	headers(HeaderCount, HeadersAndRest, []),
    {Body, <<>>} =
	bodytags(BodyCount, BodyData, []),
    {remoting, Headers, Body}.
	
