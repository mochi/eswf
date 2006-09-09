%% @author Bob Ippolito <bob@redivi.com>
%% @copyright 2006 Bob Ippolito

%% @doc Create SWF tags.

-module(eswf_tags).
-export([encbits/1, enc/1, enctag/1, enctag/2]).
-export([encshape/2]).
%% @type bitlist() = [char()]. Where all items are 0 or 1.
%% @type iolist() = [char() | binary() | iolist()]
%% @type iodata() = iolist() | binary()

-define(TWIP(N), (N * 20)).
-define(PIXEL(N), (N * 0.05)).

-define(SET_BACKGROUND_COLOR, 9).
-define(FILE_ATTRIBUTES, 69).
-define(SHOW_FRAME, 1).
-define(DEFINE_SHAPE, 2).
-define(DEFINE_SHAPE2, 22).
-define(DEFINE_SHAPE3, 32).
-define(PLACE_OBJECT2, 26).
-define(DEFINE_SPRITE, 39).
-define(DO_ACTION, 12).
-define(DEFINE_TEXT, 11).
-define(DEFINE_FONT2, 48).

-define(ENDTAG, <<0:16/little>>).

-define(USE_NETWORK, 1).
-define(HAS_METADATA, 16).

encbits_matrix_part([], Acc) ->
    %% no scale or translation is represented as the empty list []
    [0 | Acc];
encbits_matrix_part([A, B] = Parts, Acc) ->
    %% matrix is [1 | NumBits | A | B], so we do it backwards for performance
    Bits = eswf_bits:calc(fixed, Parts),
    Acc1 = eswf_bits:enc(fixed, Bits, B, Acc),
    Acc2 = eswf_bits:enc(fixed, Bits, A, Acc1),
    Acc3 = eswf_bits:enc(unsigned, 5, Bits, Acc2),
    [1 | Acc3].

encbits({rect, Xmin, Xmax, Ymin, Ymax}, Acc) ->
    Parts = [trunc(?TWIP(X)) || X <- [Xmin, Xmax, Ymin, Ymax]],
    Bits = eswf_bits:calc(signed, Parts),
    Rest = lists:foldr(fun (X, TmpAcc) -> 
			       eswf_bits:enc(signed, Bits, X, TmpAcc)
		       end, Acc, Parts),
    eswf_bits:enc(unsigned, 5, Bits, Rest);

encbits({matrix, Scale, Rotate, Translate}, Acc) ->
    [XT, YT] = [?TWIP(X) || X <- Translate],
    Bits = eswf_bits:calc(signed, [XT, YT]),
    Acc1 = eswf_bits:enc(signed, Bits, YT, Acc),
    Acc2 = eswf_bits:enc(signed, Bits, XT, Acc1),
    Acc3 = eswf_bits:enc(unsigned, 5, Bits, Acc2),
    lists:foldr(fun encbits_matrix_part/2, Acc3, [Scale, Rotate]).

encbits(Any) ->
    encbits(Any, []).

encshapes(Shapes) ->
    encshapes(Shapes, {[], []}, []).

encshapes([], {Fills, Lines}, RevShapes) ->
    FillInts = [X || X <- Fills, is_integer(X)],
    LineInts = [X || X <- Lines, is_integer(X)],
    FillBits = eswf_bits:calc(unsigned, FillInts),
    LineBits = eswf_bits:calc(unsigned, LineInts),
    encshapesfinish(RevShapes, {FillBits, LineBits}, [0, 0, 0, 0, 0, 0]);
encshapes([Shape | Rest], FillLine, RevShapes) ->
    encshapes(Rest, encshapetraverse(Shape, FillLine), [Shape | RevShapes]).

encshapetraverse({style_change, _Move, FillStyle0, FillStyle1, LineStyle,
		  _FillStyles, _LineStyles}, {Fills, Lines}) ->
    {[FillStyle0, FillStyle1 | Fills], [LineStyle | Lines]};
encshapetraverse(_, FillLine) ->
    FillLine.

encshapesfinish([], {FillBits, LineBits}, Acc) ->
    Acc1 = eswf_bits:enc(unsigned, 4, LineBits, Acc),
    Acc2 = eswf_bits:enc(unsigned, 4, FillBits, Acc1),
    eswf_bits:to_bytes(lists:flatten(Acc2));
encshapesfinish([{straight, H, 0} | Rest], FillLine, Acc) ->
    HT = ?TWIP(H),
    NumBits = lists:max([eswf_bits:calc(signed, [HT]), 2]),
    TypeFlag = 1,
    StraightFlag = 1,
    NumBitsEnc = eswf_bits:enc(unsigned, 4, NumBits - 2),
    GeneralLineFlag = 0,
    VertLineFlag = 0,
    DeltaX = eswf_bits:enc(signed, NumBits, HT),
    Body = [TypeFlag,
	    StraightFlag,
	    NumBitsEnc,
	    GeneralLineFlag,
	    VertLineFlag,
	    DeltaX],
    encshapesfinish(Rest, FillLine, [Body | Acc]);
encshapesfinish([{straight, 0, V} | Rest], FillLine, Acc) ->
    VT = ?TWIP(V),
    NumBits = lists:max([eswf_bits:calc(signed, [VT]), 2]),
    TypeFlag = 1,
    StraightFlag = 1,
    NumBitsEnc = eswf_bits:enc(unsigned, 4, NumBits - 2),
    GeneralLineFlag = 0,
    VertLineFlag = 1,
    DeltaY = eswf_bits:enc(signed, NumBits, VT),
    Body = [TypeFlag,
	    StraightFlag,
	    NumBitsEnc,
	    GeneralLineFlag,
	    VertLineFlag,
	    DeltaY],
    encshapesfinish(Rest, FillLine, [Body | Acc]);
encshapesfinish([{style_change, Move, FillStyle0, FillStyle1, LineStyle,
		  FillStyles, LineStyles} | Rest], {FBits, LBits}, Acc) ->
    {Flag0, Acc0} = sc_styles(FillStyles, LineStyles, Acc),
    {Flag1, Acc1} = sc_bits(LineStyle, LBits, Acc0),
    {Flag2, Acc2} = sc_bits(FillStyle1, FBits, Acc1),
    {Flag3, Acc3} = sc_bits(FillStyle0, FBits, Acc2),
    {Flag4, Acc4} = sc_move(Move, Acc3),
    Acc5 = [0, Flag0, Flag1, Flag2, Flag3, Flag4 | Acc4],
    encshapesfinish(Rest, {FBits, LBits}, Acc5).

sc_styles(null, null, Acc) ->
    {0, Acc}.
   

sc_bits(null, _NumBits, Acc) ->
    {0, Acc};
sc_bits(Num, NumBits, Acc) ->
    {1, [eswf_bits:enc(unsigned, NumBits, Num) | Acc]}.

sc_move(null, Acc) ->
    {0, Acc};
sc_move({H, V}, Acc) ->
    HT = ?TWIP(H),
    VT = ?TWIP(V),
    NumBits = eswf_bits:calc(signed, [HT, VT]),
    {1, [eswf_bits:enc(unsigned, 5, NumBits),
	 eswf_bits:enc(signed, NumBits, HT),
	 eswf_bits:enc(signed, NumBits, VT) | Acc]}.
    

%% @spec enc(Any) -> iodata()
%% @doc Convert a high-level SWF data type to iodata().
enc({rgb, R, G, B}) ->
    <<R, G, B>>;
enc({rgba, R, G, B, A}) ->
    <<R, G, B, A>>;
enc({rect, Xmin, Xmax, Ymin, Ymax}) ->
    eswf_bits:to_bytes(encbits({rect, Xmin, Xmax, Ymin, Ymax}));
enc({matrix, Scale, Rotate, Translate}) ->
    eswf_bits:to_bytes(encbits({matrix, Scale, Rotate, Translate}));
enc({string, String}) when is_binary(String) ->
    [String, 0];
enc({string, String}) when is_atom(String) ->
    [atom_to_list(String), 0];
enc({string, String}) when is_list(String) ->
    [String, 0].

encstyles(Fills, V) ->
    encstyles(lists:reverse(Fills), 0, [], V).

% Only version 1 supported
encstyles([], Count, Acc, V) when V > 1, Count >= 255 ->
    [<<255, Count:16/little>> | lists:reverse(Acc)];
encstyles([], Count, Acc, _V) when Count < 255 ->
    [Count | lists:reverse(Acc)];
encstyles([{fillstyle, solid, {rgba, R, G, B, A}} | Rest], Count, Acc, V) 
  when V >= 3 ->
    NextAcc = [<<0, R, G, B, A>> | Acc],
    encstyles(Rest, 1 + Count, NextAcc, V);
encstyles([{linestyle, Width, {rgba, R, G, B, A}} | Rest], Count, Acc, V) 
  when V >= 3 ->
    W = trunc(?TWIP(Width)),
    NextAcc = [<<W:16/little, R, G, B, A>> | Acc],
    encstyles(Rest, 1 + Count, NextAcc, V);
encstyles([{fillstyle, solid, {rgb, R, G, B}} | Rest], Count, Acc, V) 
  when V < 3 ->
    NextAcc = [<<0, R, G, B>> | Acc],
    encstyles(Rest, 1 + Count, NextAcc, V);
encstyles([{linestyle, Width, {rgb, R, G, B}} | Rest], Count, Acc, V) 
  when V < 3 ->
    W = trunc(?TWIP(Width)),
    NextAcc = [<<W:16/little, R, G, B>> | Acc],
    encstyles(Rest, 1 + Count, NextAcc, V).

encshape({shape_with_style, FillStyles, LineStyles, Shapes}, V) ->
    FillBytes = encstyles(FillStyles, V),
    LineBytes = encstyles(LineStyles, V),
    ShapeBytes = encshapes(Shapes),
    [FillBytes, LineBytes, ShapeBytes].

enctag(Code, Body, Size) when Size < 16#3f ->
    [<<((Code bsl 6) bor Size):16/little>>, Body];
enctag(Code, Body, Size) ->
    [<<((Code bsl 6) bor 16#3f):16/little, Size:32/little>>, Body].

%% @spec enctag(Code::char(), Body::iodata()) -> iolist()
%% @doc Convert a raw SWF tag to iodata().
enctag(Code, Body) ->
    enctag(Code, Body, iolist_size(Body)).

encdefineshape(Code, V, ShapeID, Bounds, Shape) ->
    enctag(Code, [<<ShapeID:16/little>>, enc(Bounds), encshape(Shape, V)]).

%% @spec enctag(Any) -> iolist()
%% @doc Convert a high-level SWF tag to iodata().
enctag({file_attributes, Flags}) ->
    enctag(?FILE_ATTRIBUTES, <<Flags:32/little>>);
enctag({set_background_color, Color}) ->
    enctag(?SET_BACKGROUND_COLOR, enc(Color));
enctag({do_action, Actions}) ->
    enctag(?DO_ACTION, eswf_actions:encactions(Actions));
enctag(show_frame) ->
    enctag(?SHOW_FRAME, []);
enctag({define_text, CharacterID, Data}) ->
    enctag(?DEFINE_TEXT, [<<CharacterID:16/little>>, Data]);
enctag({define_font2, FontID, Data}) ->
    enctag(?DEFINE_FONT2, [<<FontID:16/little>>, Data]);
enctag({define_shape, ShapeID, Bounds, ShapeWithStyle}) ->
    encdefineshape(?DEFINE_SHAPE, 1, ShapeID, Bounds, ShapeWithStyle);
enctag({define_shape2, ShapeID, Bounds, ShapeWithStyle}) ->
    encdefineshape(?DEFINE_SHAPE2, 2, ShapeID, Bounds, ShapeWithStyle);
enctag({define_shape3, ShapeID, Bounds, ShapeWithStyle}) ->
    encdefineshape(?DEFINE_SHAPE3, 3, ShapeID, Bounds, ShapeWithStyle);
enctag({define_sprite, SpriteID, Tags}) ->
    {FrameCount, RevBody} = lists:foldl(
			   fun (show_frame, {Count, Acc}) ->
				   {1 + Count, [enctag(show_frame) | Acc]};
			       (Tag, {Count, Acc}) ->
				   {Count, [enctag(Tag) | Acc]}
			   end, {0, []}, Tags),
    Head = <<SpriteID:16/little, FrameCount:16/little>>,
    enctag(?DEFINE_SPRITE, [Head | lists:reverse([?ENDTAG | RevBody])]);
enctag({define_solidrect, ShapeID, Bounds, Color}) ->
    DefineShape = case Color of
		      {rgba, _, _, _, _} ->
			  define_shape3;
		      _ ->
			  define_shape
		  end,
    {rect, Xmin, Xmax, Ymin, Ymax} = Bounds,
    W = Xmax - Xmin,
    H = Ymax - Ymin,
    ShapeWithStyle = {shape_with_style, 
		      [{fillstyle, solid, Color}],
		      [{linestyle, 1, Color}],
		      [{style_change, {Xmin, Ymin}, null, 1, null, null, null},
		       {straight, W, 0},
		       {straight, 0, H},
		       {straight, -W, 0},
		       {straight, 0, -H}]},
    enctag({DefineShape, ShapeID, Bounds, ShapeWithStyle});
enctag({place_anon, Depth, CharacterID, Translate}) ->
    Flags = 2#00000110, %% Name, Matrix, Character
    Body = [<<Flags, Depth:16/little, CharacterID:16/little>>,
	    enc({matrix, [], [], Translate})],
    enctag(?PLACE_OBJECT2, Body);
enctag({place_movieclip, Depth, CharacterID, Translate, Name}) ->
    Flags = 2#00100110, %% Name, Matrix, Character
    Body = [<<Flags, Depth:16/little, CharacterID:16/little>>,
	    enc({matrix, [], [], Translate}),
	    enc({string, Name})],
    enctag(?PLACE_OBJECT2, Body).
