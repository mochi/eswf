%% @author Bob Ippolito <bob@redivi.com>
%% @copyright 2006 Bob Ippolito

%% @doc Create SWF files.

-module(eswf).

-export([encswf/4, decswf/1]).
-export([swf_redir/2, swf_redir/4]).

%% @type iolist() = [char() | binary() | iolist()]
%% @type iodata() = iolist() | binary()

%% This is really bad, should be done as some kinda CPS or a FSM maybe.
decswf(Binary) ->
    <<C, $W, $S, Version, _FileLength:32/little, B0/binary>> = Binary,
    B1 = case C of
	     $F ->
		 B0;
	     $C ->
		 zlib:uncompress(B0)
	 end,
    <<RB:5, _:3, _/binary>> = B1,
    Padding = case (8 - ((5 + RB * 4) band 7)) of
		  0 ->
		      0;
		  P -> P
	      end,
    <<RB:5, 0:RB, W0:RB/signed, 0:RB, H0:RB/signed, _:Padding,
     FR0:16/little, FrameCount:16/little, B2/binary>> = B1,
    Width = W0 / 20.0,
    Height = H0 / 20.0,
    FrameRate = FR0 / 256.0,
    Tags = read_tags(B2),
    {Version, {Width, Height}, FrameRate, FrameCount, Tags}.

read_tags(Binary) ->
    read_tags(Binary, []).

read_tags(<<0:16/little, _/binary>>, Acc) ->
    %% EndTag
    lists:reverse(Acc);
read_tags(<<CodeAndLength:16/little, B0/binary>>, Acc) ->
    <<Code:10, ShortLength:6>> = <<CodeAndLength:16>>,
    {Length, BodyRest} = case ShortLength of
			     63 ->
				 <<L1:32/little, B1/binary>> = B0,
				 {L1, B1};
			     _ ->
				 {ShortLength, B0}
			end,
    <<Body:Length/binary, Rest/binary>> = BodyRest,
    read_tags(Rest, [{Code, Body} | Acc]).
				
				
%% @spec encswf(Version, {Width, Height}, Fps, Tags) -> iodata()
%% @doc Return an uncompressed SWF file containing the tags in Tags.
encswf(Version, {Width, Height}, Fps, Tags) ->
    {EncTags, Frames} = lists:mapfoldl(
			  fun
			     (show_frame, Acc) ->
				  {eswf_tags:enctag(show_frame), 1 + Acc};
			     (Tag, Acc) ->
				  {eswf_tags:enctag(Tag), Acc}
			  end,
			  0, Tags),    
    Bounds = eswf_tags:enc({rect, 0, Width, 0, Height}),
    ShiftFps = trunc(Fps * 256),
    Header = <<ShiftFps:16/little, Frames:16/little>>,
    Bytes = [Bounds, Header, EncTags, <<0:16/little>>],
    Size = 8 + iolist_size(Bytes),
    Signature = <<"FWS", Version, Size:32/little>>,
    [Signature, Bytes].


%% @spec swf_redir(Url, {Width, Height}) -> iodata()
%% @equiv swf_redir(Url, {Width, Height}, 12, 6)
swf_redir(Url, Dimensions) ->
    swf_redir(Url, Dimensions, 12, 6).

%% @spec swf_redir(Url, {Width, Height}, Fps, FlashVersion) -> iodata()
%% @doc Return a SWF that does a loadMovie to Url with a registration point
%% at the center. The center of the loaded movie clip will be at (0, 0).
swf_redir(Url, {Width, Height}, Fps, FlashVersion) ->
    Left = -(Width * 0.5),
    Top = -(Height * 0.5),
    Tags = [{file_attributes, 1},
	    {set_background_color, {rgb, 255, 255, 255}},
	    {do_action, [{push, [Url, 1, 1, "ad", 2, "this"]},
			 get_variable,
			 {push, ["createEmptyMovieClip"]},
			 call_method,
			 push_duplicate,
			 push_duplicate,
			 {push, ["_x", Left]},
			 set_member,
			 {push, ["_y", Top]},
			 set_member,
			 {push, ["loadMovie"]},
			 call_method,
			 pop]},
	    show_frame],
    encswf(FlashVersion, {Width, Height}, Fps, Tags).

