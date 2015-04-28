-module(in_tcp_v4_socket_stream_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([start_child/1,
         children/0
        ]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    C = in_tcp_v4_socket_stream,
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {ok, {SupFlags,
          [{stream_id,{C, start_link, []},temporary, 1000, worker, [C]}
          ]}
    }.

start_child(Socket) ->
    supervisor:start_child(?MODULE, [Socket]).

children() ->
    supervisor:which_children(?MODULE).