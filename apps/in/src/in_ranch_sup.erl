-module(in_ranch_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-define(CHILD(I, Type),
    {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-define(CHILD(I, Args, Type),
    {I, {I, start_link, [Args]}, permanent, 5000, Type, [I]}).

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init(Args) ->
    {listen_opts,Opts} = proplists:lookup(listen_opts,Args),
    {tcp_v4_port,Port} = proplists:lookup(tcp_v4_port,Args),
	{ok, _} = ranch:start_listener(in_ranch, 100, ranch_tcp, 
                                   [{port, Port}] ++ Opts, in_ranch_protocol, []),

    {ok, { {one_for_one, 5, 10}, [
        % ?CHILD(, supervisor)
    ]} }.