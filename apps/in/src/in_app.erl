-module(in_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->

	 % erlang:set_cookie('proc_buff@127.0.0.1', proc_buff).

    in_sup:start_link().

stop(_State) ->
    ok.
