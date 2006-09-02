%% @author Bob Ippolito <bob@redivi.com>
%% @copyright 2006 Bob Ippolito

%% @doc Create URL redirect SWF files.

-module(eswf_redir).

-export([action/2, swf/2, swf/4]).

%% @type iolist() = [char() | binary() | iolist()]
%% @type iodata() = iolist() | binary()

%% @spec swf(Url, {Width, Height}) -> iodata()
%% @equiv swf(Url, {Width, Height}, 12, 6)
swf(Url, Dimensions) ->
    swf(Url, Dimensions, 12, 6).

%% @spec swf(Url, {Width, Height}, Fps, FlashVersion) -> iodata()
%% @doc Return a SWF that does a loadMovie to Url with a registration point
%%      at the center. The center of the loaded movie clip will be at (0, 0).
swf(Url, Dimensions, Fps, FlashVersion) ->
    Tags = [{file_attributes, 1},
	    {set_background_color, {rgb, 255, 255, 255}},
	    action(Url, Dimensions),
	    show_frame],
    eswf:encswf(FlashVersion, Dimensions, Fps, Tags).

%% @spec action(Url, {Width, Height}) -> {do_action, Actions}
%% @doc Return a do_action tag that does a loadMovie to Url with a
%%      registration point at the center. The center of the loaded movie clip
%%      will be at (0, 0).
action(Url, {Width, Height}) ->
    Left = -(Width * 0.5),
    Top = -(Height * 0.5),
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
		 pop]}.
