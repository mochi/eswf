%% @author Bob Ippolito <bob@redivi.com>
%% @copyright 2006 Bob Ippolito

%% @doc Create SWF tags.

-module(eswf_tags).
-export([encbits/1, enc/1, enctag/1, enctag/2]).

%% @type bitlist() = [char()]. Where all items are 0 or 1.
%% @type iolist() = [char() | binary() | iolist()]
%% @type iodata() = iolist() | binary()

-define(TWIP(N), (N * 20)).
-define(PIXEL(N), (N * 0.05)).

-define(SET_BACKGROUND_COLOR, 9).
-define(FILE_ATTRIBUTES, 69).
-define(SHOW_FRAME, 1).
-define(DO_ACTION, 12).

-define(USE_NETWORK, 1).
-define(HAS_METADATA, 16).

encbits_matrix_part([], Acc) ->
    Acc;
encbits_matrix_part([A, B] = Parts, Acc) ->
    Bits = eswf_bits:calc(fixed, Parts),
    eswf_bits:enc(fixed, 5, Bits,
	       eswf_bits:enc(fixed, Bits, A,
			  eswf_bits:enc(fixed, Bits, B, Acc))).

encbits({rect, Xmin, Xmax, Ymin, Ymax}, Acc) ->
    Parts = [trunc(?TWIP(X)) || X <- [Xmin, Xmax, Ymin, Ymax]],
    Bits = eswf_bits:calc(signed, Parts),
    Rest = lists:foldr(fun (X, TmpAcc) -> 
			       eswf_bits:enc(signed, Bits, X, TmpAcc)
		       end, Acc, Parts),
    eswf_bits:enc(signed, 5, Bits, Rest);

encbits({matrix, Scale, Rotate, Translate}, Acc) ->
    Parts = [Scale, Rotate, [?TWIP(X) || X <- Translate]],
    lists:foldr(fun encbits_matrix_part/2, Acc, Parts).

encbits(Any) ->
    encbits(Any, []).

%% @spec enc(Any) -> iodata()
%% @doc Convert a high-level SWF data type to iodata().
enc({rgb, R, G, B}) ->
    <<R, G, B>>;
enc({rgba, R, G, B, A}) ->
    <<R, G, B, A>>;
enc({rect, Xmin, Xmax, Ymin, Ymax}) ->
    eswf_bits:to_bytes(encbits({rect, Xmin, Xmax, Ymin, Ymax}));
enc({matrix, Scale, Rotate, Translate}) ->
    eswf_bits:to_bytes(encbits({matrix, Scale, Rotate, Translate})).

enctag(Code, Body, Size) when Size < 16#3f ->
    [<<((Code bsl 6) bor Size):16/little>>, Body];
enctag(Code, Body, Size) ->
    [<<((Code bsl 6) bor 16#3f):16/little, Size:32/little>>, Body].

%% @spec enctag(Code::char(), Body::iodata()) -> iolist()
%% @doc Convert a raw SWF tag to iodata().
enctag(Code, Body) ->
    enctag(Code, Body, iolist_size(Body)).

%% @spec enctag(Any) -> iolist()
%% @doc Convert a high-level SWF tag to iodata().
enctag({file_attributes, Flags}) ->
    enctag(?FILE_ATTRIBUTES, <<Flags:32/little>>);
enctag({set_background_color, Color}) ->
    enctag(?SET_BACKGROUND_COLOR, enc(Color));
enctag({do_action, Actions}) ->
    enctag(?DO_ACTION, eswf_actions:encactions(Actions));
enctag(show_frame) ->
    enctag(?SHOW_FRAME, []).
