%% @copyright 2007 Mochi Media, Inc.
%% @author Matthew Dempsky <matthew@mochimedia.com>
%%
%% @doc Create and manipulate SWF stencils.

%% @todo Consider API issues before stabilizing.  E.g., what values
%% should the user's callback function be applied to?  Should it be
%% able to completely replace tags?

%% @todo Add proper edocs.

-module(swfstencil).
-export([stencilize/3, brush/3, fill/2]).
-export([version/1, version/2]).

-include("tags.hrl").

-record(stencil, {swfversion, zipchunks}).

brush(as2, Key, String) ->
    {{as2string, Key}, eswf_actions_utils:encode_string(String)};
brush(as3, Key, String) ->
    {{as3string, Key}, eswf_abc:encode_string(String)};
brush(binary, Key, String) ->
    {{binary, Key}, String};
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
%% <code>Version</code>.  (SWF Stencil version is used if Version is lower)
version(Stencil, Version) when is_record(Stencil, stencil) ->
    Stencil#stencil{swfversion=erlang:max(Version, Stencil#stencil.swfversion)}.

stencilize(Binary, Fun, Acc) ->
    <<C, $W, $S, Version, Length:32/little, Body0/binary>> = Binary,
    %% File compression requires SWF 6 or later.
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

%% XXX: According to Adobe's "SWF File Format Specification Version 9"
%% PDF, the guard here should be "Length >= 63", but in practice that
%% breaks some SWFs.
make_tag_header(Code, Length) when Length >= 1 ->
    <<((Code bsl 6) + 63):16/little, Length:32/little>>;
make_tag_header(Code, Length) ->
    <<((Code bsl 6) + Length):16/little>>.


fill(#stencil{swfversion=Version, zipchunks=SWFTemplate}, Brushes0) ->
    Brushes = gb_trees:from_orddict(orddict:from_list(Brushes0)),
    Brush = fun(Key) ->
                    try gb_trees:get(Key, Brushes)
                    catch error:function_clause ->
                            throw({missing_stencil_key, Key})
                    end
            end,
    Fun = fun({simple, Key}) ->
                  Brush(Key);
             ({tagheader, Code, BaseSize, Keys}) ->
                  ExtraSize = lists:sum([iolist_size(Brush(Key))
                                         || Key <- Keys]),
                  make_tag_header(Code, BaseSize + ExtraSize);
             ({as2insnlen, BaseSize, Keys}) ->
                  ExtraSize = lists:sum([iolist_size(Brush(Key))
                                         || Key <- Keys]),
                  <<(BaseSize + ExtraSize):16/little>>;

             %% Legacy brush formats
             ({as2string, Key}) ->
                  Brush({as2string, Key});
             ({as3string, Key}) ->
                  Brush({as3string, Key});
             ({export, Key}) ->
                  Brush({export, Key});

             %% Legacy tag header key formats
             ({as2tagheader, Code, BaseSize, Keys}) ->
                  ExtraSize = lists:sum([iolist_size(Brush({as2string, Key}))
                                         || Key <- Keys]),
                  make_tag_header(Code, BaseSize + ExtraSize);
             ({as3tagheader, BaseSize, Keys}) ->
                  ExtraSize = lists:sum([iolist_size(Brush({as3string, Key}))
                                         || Key <- Keys]),
                  make_tag_header(?DoABC, BaseSize + ExtraSize);
             ({exportheader, BaseSize, Keys}) ->
                  ExtraSize = lists:sum([iolist_size(Brush(Key))
                                         || Key <- Keys]),
                  make_tag_header(?ExportAssets, BaseSize + ExtraSize)
          end,
    {Length, Body} = zipchunk:fill(SWFTemplate, Fun),
    [<<"CWS", Version, (Length + 8):32/little>> | Body].


do_tags(Binary, Fun, Acc, UserAcc) ->
    case read_tag(Binary) of
        eof ->
            {lists:flatten(lists:reverse(Acc)), UserAcc};
        {Code, Body, Rest} ->
            try do_tag(Code, Body, Fun, UserAcc) of
                {NewElt0, NewUserAcc} ->
                    NewElt = case NewElt0 of
                                 skip ->
                                     {chunk, [make_tag_header(Code, size(Body)),
                                              Body]};
                                 _NewElt0 ->
                                     NewElt0
                     end,
                    do_tags(Rest, Fun, [NewElt | Acc], NewUserAcc)
            catch
                Type:What ->
                    error_logger:error_report(["swfstencil:do_tag failed",
                                               {caught, {Type, What}},
                                               {code, Code}]),
                    NewElt = {chunk, [make_tag_header(Code, size(Body)), Body]},
                    do_tags(Rest, Fun, [NewElt | Acc], UserAcc)
            end
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

do_tag(?DoAction=Code, Body0, Fun, UserAcc) ->
    do_action(0, Code, Body0, Fun, UserAcc);
do_tag(?DoInitAction=Code, Body0, Fun, UserAcc) ->
    do_action(2, Code, Body0, Fun, UserAcc);
do_tag(?SymbolClass=Code, Rest, Fun, UserAcc) ->
    CodeKey = symbol_class,
    Fun2 = fun(Key, UA) ->
                   case Fun({CodeKey, Key}, UA) of
                       {{punch, Term}, NUA} ->
                           {{punch, {simple, {CodeKey, Term}}}, NUA};
                       {skip, NUA} ->
                           {skip, NUA}
                   end
           end,
    do_export_or_symbol_class(Code, Rest, Fun2, UserAcc);
do_tag(?ExportAssets=Code, Rest, Fun, UserAcc) ->
    CodeKey = export,
    Fun2 = fun({_CharID, Key}, UA) ->
                   case Fun({CodeKey, Key}, UA) of
                       {{punch, Term}, NUA} ->
                           {{punch, {simple, {CodeKey, Term}}}, NUA};
                       {skip, NUA} ->
                           {skip, NUA}
                   end
           end,
    do_export_or_symbol_class(Code, Rest, Fun2, UserAcc);
do_tag(?DoABC=Code, Body, Fun, UserAcc) ->
    do_abc(findnull(Body, 4) + 1, Code, Body, Fun, UserAcc);
do_tag(?DoABCNoDefine=Code, Body, Fun, UserAcc) ->
    do_abc(0, Code, Body, Fun, UserAcc);
do_tag(?DefineBinaryData=Code, Body, Fun, UserAcc) ->
    do_define_binary_data(Code, Body, Fun, UserAcc);
do_tag(_Code, Body, _Fun, UserAcc) when is_binary(Body) ->
    {skip, UserAcc}.

do_action(PrefixSize, Code, Body0, Fun, UserAcc) ->
    {Prefix, Body} = split_binary(Body0, PrefixSize),
    Fun2 = fun(Key, {UA, Keys}) ->
                   case Fun({as2, Key}, UA) of
                       {{punch, Term}, NUA} ->
                           NewKey = {as2string, Term},
                           {{punch, NewKey}, {NUA, [NewKey | Keys]}};
                       {skip, NUA} ->
                           {skip, {NUA, Keys}}
                   end
           end,
    {Template0, {NewUserAcc, Keys}}
        = eswf_actions_utils:stencilify(Body, Fun2, {UserAcc, []}),
    %% XXX: Stupid.
    Template = lists:map(fun({hole, {as2string, Term}}) ->
                                 {hole, {simple, {as2string, Term}}};
                            (Else) ->
                                 Else
                         end, Template0),
    NewElt = case Keys of
                 [] ->
                     skip;
                 _Keys ->
                     FixedSize = lists:sum([iolist_size(X)
                                            || {chunk, X} <- Template]),
                     ExtraTagSize = lists:sum([2 || {hole, {as2insnlen, _, _}}
                                                        <- Template]),
                     Size = PrefixSize + FixedSize + ExtraTagSize,
                     [{hole, {tagheader, Code, Size, Keys}},
                      {chunk, Prefix} | Template]
             end,
     {NewElt, NewUserAcc}.

do_abc(HeaderSize, Code, Body, Fun, UserAcc) ->
    {Header, ABCSegment} = split_binary(Body, HeaderSize),
    Fun2 = fun(Key, {UA, Keys}) ->
                   case Fun({as3, Key}, UA) of
                       {{punch, Term}, NUA} ->
                           NewKey = {as3string, Term},
                           {{punch, {simple, NewKey}}, {NUA, [NewKey | Keys]}};
                       {skip, NUA} ->
                           {skip, {NUA, Keys}}
                   end
           end,
    {Template, {NewUserAcc, Keys}}
        = eswf_abc:stencilify(ABCSegment, Fun2, {UserAcc, []}),
    NewElt = case Keys of
                 [] ->
                     skip;
                 _Keys ->
                     Size = lists:sum([iolist_size(X)
                                       || {chunk, X} <- Template]),
                     [{hole, {tagheader, Code, HeaderSize + Size, Keys}},
                      {chunk, Header} | Template]
             end,
{NewElt, NewUserAcc}.

do_define_binary_data(Code, <<CharId:16/little, Reserved:32, Blob/binary>>, Fun, UserAcc) ->
    CodeKey = binary,
    case Fun({CodeKey, {CharId, Blob}}, UserAcc) of
        {{punch, Term}, NewUserAcc} ->
            Key = {CodeKey, Term},
            NewElt = [{hole, {tagheader, Code, 6, [Key]}},
                      {chunk, <<CharId:16/little, Reserved:32>>},
                      {hole, {simple, Key}}],
            {NewElt, NewUserAcc};
        {skip, NewUserAcc} ->
            {skip, NewUserAcc}
    end.

do_export_or_symbol_class(Code, <<Count:16/little, Rest/binary>>, Fun2, UserAcc) ->
    {Template, NewUserAcc} = export_stencilify(Rest, Count, Fun2, UserAcc, []),
    Keys = [Key || {hole, {simple, Key}} <- Template],
    NewElt = case Keys of
                 [] ->
                     skip;
                 _Keys ->
                     FixedSize = lists:sum([iolist_size(X)
                                            || {chunk, X} <- Template]),
                     [{hole, {tagheader, Code, 2 + FixedSize, Keys}},
                      {chunk, <<Count:16/little>>} | Template]
             end,
    {NewElt, NewUserAcc}.

export_stencilify(<<>>, 0, _Fun, UserAcc, Acc) ->
    {lists:reverse(Acc), UserAcc};
export_stencilify(<<SpriteID:16/little, Rest/binary>>, N, Fun, UserAcc, Acc) when N > 0 ->
    {String, <<0, Rest2/binary>>} = split_binary(Rest, findnull(Rest, 0)),
    {Action, NewUserAcc} = Fun({SpriteID, String}, UserAcc),
    Chunk =
        case Action of
            skip ->
                {chunk, [String, 0]};
            {punch, Key} ->
                {hole, Key}
        end,
    NewAcc = [Chunk, {chunk, <<SpriteID:16/little>>} | Acc],
    export_stencilify(Rest2, N - 1, Fun, NewUserAcc, NewAcc).


findnull(Binary, N) ->
    <<_Pre:N/binary, Rest/binary>> = Binary,
    findnull_1(Rest, N).

findnull_1(<<0, _Rest/binary>>, N) ->
    N;
findnull_1(<<_Ch, Rest/binary>>, N) ->
    findnull_1(Rest, N + 1).
