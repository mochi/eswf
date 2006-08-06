%% @author Bob Ippolito <bob@redivi.com>
%% @copyright 2006 Bob Ippolito

%% @doc Create SWF files.

-module(eswf).

-export([encswf/4]).
-export([swf_redir/2, swf_redir/4]).

%% @type iolist() = [char() | binary() | iolist()]
%% @type iodata() = iolist() | binary()

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
    Bytes = [EncTags, <<0:16/little>>],
    Size = iolist_size(Bytes),
    Signature = <<"FWS", Version, Size:32/little>>,
    Bounds = eswf_tags:enc({rect, 0, Width, 0, Height}),
    ShiftFps = trunc(Fps * 256),
    Header = <<ShiftFps:16/little, Frames:16/little>>,
    [Signature, Bounds, Header, Bytes].


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

