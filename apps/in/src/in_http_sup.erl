-module(in_http_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-define(CHILD(I, Args, Type),
    {I, {I, start_link, [Args]}, permanent, 5000, Type, [I]}).

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Args]).

init(Args) ->
    {ok, { {one_for_one, 5, 10}, [
        ?CHILD(in_http_stream, Args, worker)
    ]} }.