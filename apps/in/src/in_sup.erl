-module(in_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(CHILD(I, Args, Type),
    {I, {I, start_link, [Args]}, permanent, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = conf_to_childspec(),
    {ok, { {one_for_one, 5, 10}, Children} }.

conf_to_childspec() ->
    {ok,IncPro} = application:get_env(in, incoming_protocols),

    lists:foldl(fun({protocol,Details},Acc) when is_list(Details) ->
        Mod = type_to_mod(proplists:lookup(type, Details)),
        [?CHILD(Mod, Details, supervisor) | Acc]
    end, [], IncPro).

type_to_mod({type,tcp_v4_socket}) ->
    in_tcp_v4_socket_sup;
type_to_mod({type,http}) ->
    in_http_sup.