-module(in_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-include_lib("ipo/include/ipo.hrl").

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = conf_to_childspec(),
    {ok, { {one_for_one, 100, 10}, 
            [?CHILD(in_proc_buff, worker)] ++ Children
         } 
    }.

conf_to_childspec() ->
    {ok,IncPro} = application:get_env(in, incoming_protocols),
    lists:foldl(fun({protocol,Details},Acc) when is_list(Details) ->
        Mod = proto_type_to_mod(proplists:lookup(type, Details)),
        [?CHILD(Mod, Details, supervisor) | Acc]
    end, [], IncPro).

%% XXX: make some port clash checker.......

proto_type_to_mod({type,ranch}) -> 
    in_ranch_sup;
proto_type_to_mod({type,tcp_v4_socket}) ->
    in_tcp_v4_socket_sup;
proto_type_to_mod({type,http}) ->
    in_http_sup.