-module(out_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), 
	{I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-define(CHILD(I, Args, Type),
    {I, {I, start_link, [Args]}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	Children = conf_to_childspec(),
    {ok, { {one_for_one, 5, 10}, [?CHILD(out_worker, worker)] ++ Children} }.

conf_to_childspec() ->
    {ok,OutPro} = application:get_env(out, outgoing_protocols),
    lists:foldl(fun({protocol,Details},Acc) when is_list(Details) ->
        Mod = proto_type_to_mod(proplists:lookup(type, Details)),
        [?CHILD(Mod, Details, supervisor) | Acc]
    end, [], OutPro).

%% XXX: Use Ranch!!!

proto_type_to_mod({type,tcp_v4_socket}) ->
    out_tcp_v4_socket_sup;
proto_type_to_mod({type,http}) ->
    out_http_sup.