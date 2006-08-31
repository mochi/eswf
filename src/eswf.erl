%% @author Bob Ippolito <bob@redivi.com>
%% @copyright 2006 Bob Ippolito

%% @doc Create SWF files.

-module(eswf).

-export([encswf/4, decswf/1, swf_reader/1]).
-export([swf_redir/2, swf_redir/4]).

%% @type iolist() = [char() | binary() | iolist()]
%% @type iodata() = iolist() | binary()

decswf(Binary) ->
    {Version, Size, FrameRate, FrameCount, NextTag} = swf_reader(Binary),
    {Version, Size, FrameRate, FrameCount, all_tags(NextTag(next), [])}.

swf_reader(Reader) when is_function(Reader) ->
    {R0, Signature} = Reader(8),
    <<C, $W, $S, Version, _Length:32/little>> = Signature,
    R1 = case C of
	     $F ->
		 R0;
	     $C ->
		 swf_reader:reader({inflate, R0})
	 end,
    {R2, P0} = R1(1),
    <<RB:5, _:3>> = P0,
    Padding = case (8 - ((5 + RB * 4) band 7)) of
		  0 ->
		      0;
		  P -> P
	      end,
    {R3, B0} = R2((((RB * 4) + Padding - 3) div 8) + 4),
    B1 = iolist_to_binary([P0, B0]),
    <<RB:5, 0:RB, W0:RB/signed, 0:RB, H0:RB/signed, _:Padding,
     FR0:16/little, FrameCount:16/little>> = B1,
    Width = W0 / 20.0,
    Height = H0 / 20.0,
    FrameRate = FR0 / 256.0,
    {Version, {Width, Height}, FrameRate, FrameCount, next_tag(R3)};
swf_reader(Other) ->
    swf_reader(eswf_reader:reader(Other)).
    
next_tag(Reader) ->
    fun (close) ->
	    {RLast, eof} = Reader(close),
	    {next_tag(RLast), eof};
	(next) ->
	    {R0, <<CodeAndLength:16/little>>} = Reader(2),
	    case <<CodeAndLength:16>> of
		<<0:16/little>> ->
		    NT = next_tag(R0),
		    NT(close);
		<<Code:10, ShortLength:6>> ->
		    {R3, Body} = case ShortLength of
				     63 ->
					 {R1, <<L1:32/little>>} = R0(4),
					 R1(L1);
				     _ ->
					 R0(ShortLength)
				 end,
		    {next_tag(R3), {Code, Body}}
	    end
    end.

all_tags({_, eof}, Acc) ->
    lists:reverse(Acc);
all_tags({NextTag, Tag}, Acc) ->
    all_tags(NextTag(next), [Tag | Acc]).				
				
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

