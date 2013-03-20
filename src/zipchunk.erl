%% @author Matthew Dempsky <matthew@mochimedia.com>
%% @copyright 2007 Mochi Media, Inc.
%%
%% @doc The <code>zipchunk</code> module provides functions for
%% zlib-compressed byte sequence templates, allowing for ahead-of-time
%% compression of fixed chunks.  It is designed to be used within
%% higher-level templating systems.

%% @todo Generalize API to allow other deflate-based schemes; e.g.,
%% raw deflate and gzip.  For gzip to be useful, however, it would be
%% useful if crc32combine could call to zlib's native crc32_combine
%% function.

%% @todo Provide API for streaming output (e.g., HTTP chunking).

-module(zipchunk).
-author('matthew@mochimedia.com').


-export([optimize/1, fill/2]).
-export([sumnull/1, sumchunk/3, sumjoin/4]).
-export([test/0]).

%% @type template() = [{hole, Key::term()} | {chunk, iodata()}
%%                     | {zchunk, iodata(), Length::integer(), Chunksum::integer()}].
%%
%% A <code>template()</code> is a sequence of rule tuples tagged by an
%% atom.  A <code>hole</code> rule indicates the location of a
%% sequence of bytes that will be provided later.  A
%% <code>chunk</code> rule places a byte sequence into the resulting
%% zlib stream's logical plaintext sequence.  A <code>zchunk</code>
%% rule places a byte sequence directly into the resulting zlib stream
%% and provides the <code>Length</code> and <code>Chunksum</code> of
%% the corresponding logical plaintext sequence.

%% @spec optimize(template()) -> template()
%%
%% @doc Optimizes a <code>template()</code> by replacing
%% <code>chunk</code> rules with <code>zchunk</code> rules where
%% appropriate.
optimize(Rules) ->
    Z = zlib:open(),
    ok = zlib:deflateInit(Z, best_compression),
    _Data = zlib:deflate(Z, [], full),
    try
        optimize(Rules, Z, [], [])
    after
        ok = zlib:close(Z)
    end.

optimize([], _Z, Acc, []) ->
    lists:reverse(Acc);
optimize([R = {hole, _Key} | Rest], Z, Acc, []) ->
    optimize(Rest, Z, [R | Acc], []);
optimize([{chunk, Plain} | Rest], Z, Acc, ChunksAcc) ->
    optimize(Rest, Z, Acc, [Plain | ChunksAcc]);
optimize(Rest, Z, Acc, ChunksAcc) when ChunksAcc =/= [] ->
    optimize(Rest, Z, compute_chunk(Z, lists:reverse(ChunksAcc), 20, Acc), []).


%% @spec fill(template(), Fun) -> {Length, iodata()}
%% where
%%       Fun = (Key::term()) -> iodata()
%%
%% @doc Builds a zlib stream from a template, filling any holes with
%% <code>Fun(Key)</code>.  <code>Length</code> is the number of bytes
%% in the corresponding plaintext byte sequence.
fill(Template, Fun) ->
    Z = zlib:open(),
    %% XXX: Allow configuration options to replace `default'.
    ok = zlib:deflateInit(Z, default),
    _Data = zlib:deflate(Z, [], full),
    try
        fill(Template, Fun, Z, [], 0, sumnull(adler32), [])
    after
        ok = zlib:close(Z)
    end.

fill([], _Fun, _Z, Acc, Length, Sum, []) ->
    Encoded = [120, 156, lists:reverse(Acc), 3, 0, <<Sum:32>>],
    {Length, Encoded};
fill([{zchunk, Encoded, Length2, Sum2} | Rest], Fun, Z, Acc, Length, Sum, []) ->
    fill(Rest, Fun, Z, [Encoded | Acc], Length2 + Length, sumjoin(adler32, Sum, Sum2, Length2), []);
fill([{chunk, Plain} | Rest], Fun, Z, Acc, Length, Sum, ChunksAcc) ->
    fill(Rest, Fun, Z, Acc, Length, Sum, [Plain | ChunksAcc]);
fill([{hole, Key} | Rest], Fun, Z, Acc, Length, Sum, ChunksAcc) ->
    fill(Rest, Fun, Z, Acc, Length, Sum, [Fun(Key) | ChunksAcc]);
fill(Rest, Fun, Z, Acc, Length, Sum, ChunksAcc) when ChunksAcc =/= [] ->
    fill(compute_chunk(Z, lists:reverse(ChunksAcc), 0, Rest), Fun, Z, Acc, Length, Sum, []).


compute_chunk(Z, Plain, Limit, Rest) ->
    case iolist_to_binary(Plain) of
        <<>> ->
            Rest;
        FlatPlain when size(FlatPlain) < Limit ->
            [{chunk, FlatPlain} | Rest];
        FlatPlain ->
            Encoded = deflate(Z, FlatPlain, []),
            Chunksum = sumchunk(adler32, Z, FlatPlain),
            Length = size(FlatPlain),
            [{zchunk, Encoded, Length, Chunksum} | Rest]
    end.

%% Flushing the zlib stream doesn't actually wait until it's empty, so
%% instead we have to do this stupid workaround.  I submitted a patch
%% here: http://article.gmane.org/gmane.comp.lang.erlang.patches/185
deflate(Z, FlatPlain, Acc) ->
    try
        Buf = zlib:deflate(Z, FlatPlain, full),
        deflate(Z, [], [Buf | Acc])
    catch
        error:buf_error ->
            lists:reverse(Acc)
    end.


%% @type sumtype() = adler32 | crc32 | none.

%% @spec sumnull(Type::sumtype()) -> integer()
%%
%% @doc Returns the checksum of a null byte string.
sumnull(adler32) ->
    1;
sumnull(crc32) ->
    0;
sumnull(none) ->
    0.

%% @spec sumchunk(Type::sumtype(), Z::zstream(), Plain::binary()) -> integer()
%%
%% @doc Computes a partial checksum of a binary chunk.  
sumchunk(adler32, Z, Plain) ->
    zlib:adler32(Z, 0, Plain);
sumchunk(crc32, Z, Plain) ->
    zlib:crc32(Z, 16#ffffffff, Plain) bxor 16#ffffffff;
sumchunk(none, _Z, _Plain) ->
    0.

%% @spec sumjoin(Type::sumtype(), S::integer(), T::integer(), Length::integer()) -> integer()
%%
%% @doc Computes the checksum of the concatenation of two byte
%% sequences from their checksums.  <code>S</code> is the full
%% checksum of the first byte sequence, and <code>T</code> and
%% <code>Length</code> are the partial checksum and length
%% (respectively) of the second byte sequence.
sumjoin(adler32, S, T, Length) ->
    adler32join(S, T, Length);
sumjoin(crc32, S, T, Length) ->
    crc32join(S, T, Length);
sumjoin(none, _S, _T, _Length) ->
    0.

%% Internal sumjoin/4 implementation details.

adler32join(S, T, Length) ->
    S1 = S rem 65536,
    S2 = S div 65536,
    T1 = T rem 65536,
    T2 = T div 65536,
    U1 = (S1  + T1) rem 65521,
    U2 = (S2 + S1 * Length + T2) rem 65521,
    U1 + U2 * 65536.

%% @todo Change to use zlib's crc32_combine.  I <a
%% href="http://article.gmane.org/gmane.comp.lang.erlang.patches/183"
%% >submitted a patch for this</a>, but to my knowledge it has not
%% been accepted.
crc32join(S, T, Length) ->
    Z = zlib:open(),
    Result = crc32join(S, T, Length, Z),
    ok = zlib:close(Z),
    Result.

crc32join(S, T, 0, _Z) ->
    S bxor T;
crc32join(S, T, N, Z) when N >= 16 ->
    SNew = zlib:crc32(Z, S, <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>),
    crc32join(SNew, T, N - 16, Z);
crc32join(S, T, N, Z) when N >= 1 ->
    SNew = zlib:crc32(Z, S, <<0>>),
    crc32join(SNew, T, N - 1, Z).


%% @spec test() -> ok
%%
%% @doc Performs <code>zipchunk</code> unit tests and returns
%% <code>ok</code>.
test() ->
    application:start(crypto),
    ok = test_join(),
    ok = test_optimize_and_fill(),
    ok = test_deflate(),
    ok.

test_join() ->
    Z = zlib:open(),
    SumchunkTest =
        fun(M, N, Sum, Type) ->
                {A, B} = split_binary(C = crypto:rand_bytes(M + N), M),
                ASum = sumchunk(Type, Z, A),
                BSum = sumchunk(Type, Z, B),
                CSum = zlib:Sum(Z, C),
                CSum = sumjoin(Type, sumjoin(Type, sumnull(Type), ASum, size(A)), BSum, size(B)),
                ok
        end,
    F = fun(_, _, _, <<>>) -> ok;
           (F, Sum, Type, <<M, N, Rest/binary>>) ->
                SumchunkTest(M, N, Sum, Type),
                F(F, Sum, Type, Rest)
        end,
    Tests = crypto:rand_bytes(10000),
    ok = F(F, adler32, adler32, Tests),
    ok = F(F, crc32, crc32, Tests),
    ok = zlib:close(Z),
    ok.

test_optimize_and_fill() ->
    Tests =
        [{[], fun(_) -> "" end, []},
         {[{chunk, <<"123">>}, {chunk, <<"456">>}],
          fun(_) -> "" end,
          "123456"},
         {[{chunk, <<"123">>}, {hole, "456"}, {chunk, <<"789">>}],
          fun(S) -> ["xy", S, "ab"] end,
          "123xy456ab789"},
         {[{chunk, <<"12">>}, {chunk, <<>>}, {chunk, <<"3">>}, {hole, "456"}, {chunk, <<"789">>}],
          fun(S) -> ["xy", S, "ab"] end,
          "123xy456ab789"},
         begin
             A = crypto:rand_bytes(50000),
             B = crypto:rand_bytes(100000),
             Msg = "Some reasonably long (but not too long) text to compress",
             {[{chunk, A}, {hole, []}, {chunk, B}],
              fun(_) -> Msg end,
              [A, Msg, B]}
         end,
         {[{chunk, <<>>}, {chunk, []}, {chunk, <<>>}],
          fun(_) -> [] end,
          ""},
         {[{hole, ""}], fun(X) -> X end, []}
        ],
    [ok = test(R, F, E) || {R, F, E} <- Tests],
    ok.

test(Template, Fun, Expected) ->
    {Length, Encoded} = fill(Template, Fun),
    Template2 = optimize(Template),
    {Length, Encoded2} = fill(Template2, Fun),
    Plain = zlib:uncompress(iolist_to_binary(Encoded)),
    Plain = zlib:uncompress(iolist_to_binary(Encoded2)),
    Length = size(Plain),
    Plain = iolist_to_binary(Expected),
    ok.

test_deflate() ->
    Z = zlib:open(),
    ok = zlib:deflateInit(Z),
    _ = zlib:deflate(Z, [], full),
    Orig = crypto:rand_bytes(1000000),
    Encoded = [deflate(Z, Orig, []) || _ <- lists:seq(1, 10)],
    ok = zlib:close(Z),
    [Orig = zlib:unzip(iolist_to_binary([E, 3, 0])) || E <- Encoded],
    ok.
