%% @copyright 2007 Mochi Media, Inc.
%% @author Matthew Dempsky <matthew@mochimedia.com>
%%
%% @doc Create and manipulate SWF stencils.

%% @todo Consider API issues before stabilizing.  E.g., what values
%% should the user's callback function be applied to?  Should it be
%% able to completely replace tags?  Should the user be responsible
%% for encoding values, or should the swfstencil?  Should the key
%% spaces for different encodings be disjoint to help mitigate the
%% risk of the user accidentally reusing keys (or misencoding)?  The
%% disjoint key space could even be opaque to the user, so they need
%% to build the tuples using function calls to help prevent them from
%% incorrectly encoding things.

-module(swfstencil).
-export([stencilize/3, brush/3, fill/2]).
-export([version/1, version/2]).

-record(stencil, {swfversion, zipchunks}).

brush(as2, Key, String) ->
    {{as2string, Key}, [String, 0]}; %% XXX
brush(as3, Key, String) ->
    {{as3string, Key}, eswf_abc:encode_string(String)};
brush(export, Key, String) ->
    {{export, Key}, [String, 0]}.

%% @spec version(Stencil::stencil()) -> integer()
%%
%% @doc Return the SWF version of <code>Stencil</code>.
version(Stencil) when is_record(Stencil, stencil) ->
    Stencil#stencil.swfversion.

%% @spec version(Stencil::stencil(), Version::integer()) -> stencil()
%%
%% @doc Return a new SWF stencil but with version
%% <code>Version</code>.  (<code>Version</code> must be greater than
%% or equal to the current version.)
version(Stencil, Version) when is_record(Stencil, stencil), Version >= Stencil#stencil.swfversion ->
    Stencil#stencil{swfversion=Version}.


stencilize(Binary, Fun, Acc) ->
    <<C, $W, $S, Version, Length:32/little, Body0/binary>> = Binary,
    %% According to sswf.sf.net, the SWF file format does not allow
    %% zlib compression before version 6.
    if Version < 6 -> throw({error, badversion});
       true -> ok
    end,
    Body = case C of
               $F -> Body0;
               $C -> zlib:uncompress(Body0)
           end,
    Length = 8 + size(Body),
    <<RB:5, _:3, _Rest/binary>> = Body,
    HeaderSize = (5 + RB * 4 + 7) div 8 + 4,
    <<Header:HeaderSize/binary, Tags/binary>> = Body,
    {TagsTemplate, NewAcc} = do_tags(Tags, Fun, [], Acc),
    SWFTemplate = [{chunk, Header} | TagsTemplate],
    ZipChunks = zipchunk:optimize(SWFTemplate),
    Stencil = #stencil{zipchunks=ZipChunks, swfversion=Version},
    {Stencil, NewAcc}.

%% XXX: According to
%% http://sswf.sourceforge.net/SWFalexref.html#swf_tag, the
%% DefineBits{Lossless{,2},JPEG{,2,3}} and SoundStreamBlock tags (6,
%% 19, 20, 21, 35, and 36) must always be encoded in the longer form.
make_tag_header(Code, Length) when Length >= 63 ->
    <<((Code bsl 6) + 63):16/little, Length:32/little>>;
make_tag_header(Code, Length) ->
    <<((Code bsl 6) + Length):16/little>>.


fill(#stencil{swfversion=Version, zipchunks=SWFTemplate}, Brushes) ->
    %% This part is still slightly gross.
    AS3 = fun(Key) -> proplists:get_value({as3string, Key}, Brushes) end,
    AS2 = fun(Key) -> proplists:get_value({as2string, Key}, Brushes) end,
    Export = fun(Key) -> proplists:get_value({export, Key}, Brushes) end,
    Fun = fun({as3tagheader, BaseSize, Keys}) ->
                  ExtraSize = lists:sum([iolist_size(AS3(Key)) || Key <- Keys]),
                  make_tag_header(82, BaseSize + ExtraSize);
             ({as3string, Key}) ->
                  AS3(Key);
             ({as2tagheader, Code, BaseSize, Keys}) ->
                  ExtraSize = lists:sum([iolist_size(AS2(Key)) || Key <- Keys]),
                  make_tag_header(Code, BaseSize + ExtraSize);
             ({as2insnlen, BaseSize, Keys}) ->
                  ExtraSize = lists:sum([iolist_size(proplists:get_value(Key, Brushes)) || Key <- Keys]),
                  <<(BaseSize + ExtraSize):16/little>>;
             ({as2string, Key}) ->
                  AS2(Key);
             ({exportheader, BaseSize, Keys}) ->
                  ExtraSize = lists:sum([iolist_size(Export(Key)) || Key <- Keys]),
                  make_tag_header(52, BaseSize + ExtraSize);
             ({export, Key}) ->
                  Export(Key)
          end,
    {Length, Body} = zipchunk:fill(SWFTemplate, Fun),
    [<<"CWS", Version, (Length + 8):32/little>> | Body].


do_tags(Binary, Fun, Acc, UserAcc) ->
    case read_tag(Binary) of
        eof ->
            {lists:flatten(lists:reverse(Acc)), UserAcc};
        {Code, Body, Rest} ->
            {NewElt, NewUserAcc} = do_tag(Code, Body, Fun, UserAcc),
            do_tags(Rest, Fun, [NewElt | Acc], NewUserAcc)
    end.

read_tag(<<>>) ->
    eof;
read_tag(<<CodeAndLength:16/little, Rest/binary>>) ->
    Code = CodeAndLength bsr 6,
    {Length, R1} =
        case CodeAndLength band 16#3f of
            63 ->
                <<Length0:32/little, R0/binary>> = Rest,
                {Length0, R0};
            Length0 ->
                {Length0, Rest}
        end,
    {Body, R2} = split_binary(R1, Length),
    {Code, Body, R2}.

do_tag(12, Body, Fun, UserAcc) ->
    %% DoAction
    Fun2 = fun(Key, {UA, Keys}) ->
                   case Fun({as2, Key}, UA) of
                       {{punch, Term}, NUA} ->
                           {{punch, {as2string, Term}}, {NUA, [Term | Keys]}};
                       {skip, NUA} ->
                           {skip, {NUA, Keys}}
                   end
           end,
    {Template, {NewUserAcc, Keys}} = eswf_actions_utils:stencilify(Body, Fun2, {UserAcc, []}),
    FixedSize = lists:sum([iolist_size(X) || {chunk, X} <- Template]),
    ExtraTagSize = lists:sum([2 || {hole, {as2insnlen, _, _}} <- Template]),
    NewElt = [{hole, {as2tagheader, 12, FixedSize + ExtraTagSize, Keys}} | Template],
     {NewElt, NewUserAcc};
do_tag(59, <<SpriteID:16/little, Rest/binary>>, Fun, UserAcc) ->
    %% DoInitAction
    %% XXX: Lame
    {NewElt, NewUserAcc} = do_tag(12, Rest, Fun, UserAcc),
    [{hole, {as2tagheader, 12, BaseSize, Keys}} | RestElt] = NewElt,
    NewElt2 = [{hole, {as2tagheader, 59, 2 + BaseSize, Keys}},
               {chunk, <<SpriteID:16/little>>}
               | RestElt],
    {NewElt2, NewUserAcc};
do_tag(56, <<Count:16/little, Rest/binary>>, Fun, UserAcc) ->
    %% Export
    Fun2 = fun(Key, UA) ->
                   case Fun({export, Key}, UA) of
                       {{punch, Term}, NUA} ->
                           {{punch, {export, Term}}, NUA};
                       {skip, NUA} ->
                           {skip, NUA}
                   end
           end,
    {Template, NewUserAcc} = export_stencilify(Rest, Count, Fun2, UserAcc, []),
    FixedSize = lists:sum([iolist_size(X) || {chunk, X} <- Template]),
    Keys = [Key || {hole, Key} <- Template],
    NewElt = [{hole, {exportheader, FixedSize, Keys}} | Template],
    {NewElt, NewUserAcc};
do_tag(82, Body, Fun, UserAcc) ->
    %% DoABCDefine
    HeaderSize = findnull(Body, 4) + 1,
    {Header, ABCSegment} = split_binary(Body, HeaderSize),
    Fun2 = fun(Key, {UA, Keys}) ->
                   case Fun({as3, Key}, UA) of
                       {{punch, Term}, NUA} ->
                           {{punch, {as3string, Term}}, {NUA, [Term | Keys]}};
                       {skip, NUA} ->
                           {skip, {NUA, Keys}}
                   end
           end,
    {Template, {NewUserAcc, Keys}} = eswf_abc:stencilify(ABCSegment, Fun2, {UserAcc, []}),
    Size = lists:sum([iolist_size(X) || {chunk, X} <- Template]),
    NewElt = [{hole, {as3tagheader, HeaderSize + Size, Keys}}, {chunk, Header} | Template],
    {NewElt, NewUserAcc};
do_tag(Code, Body, _Fun, UserAcc) when is_binary(Body) ->
    NewElt = {chunk, [make_tag_header(Code, size(Body)), Body]},
    {NewElt, UserAcc}.


export_stencilify(<<>>, 0, _Fun, UserAcc, Acc) ->
    {lists:reverse(Acc), UserAcc};
export_stencilify(<<SpriteID:16, Rest/binary>>, N, Fun, UserAcc, Acc) when N > 0 ->
    {String, <<0, Rest2/binary>>} = split_binary(Rest, findnull(Rest, 0)),
    {Action, NewUserAcc} = Fun(String, UserAcc),
    Chunk =
        case Action of
            skip ->
                {chunk, [String, 0]};
            {punch, Key} ->
                {hole, Key}
        end,
    NewAcc = [Chunk, {chunk, <<SpriteID:16>>} | Acc],
    export_stencilify(Rest2, N - 1, Fun, NewUserAcc, NewAcc).


findnull(Binary, N) ->
    case Binary of
        <<_Pre:N/binary, 0, _Rest/binary>> ->
            N;
        _Else ->
            findnull(Binary, N + 1)
    end.
