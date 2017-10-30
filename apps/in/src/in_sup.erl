-module(in_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-export([ranch_sockets/0]).

-define(CHILD(I, Args, Type),
    {I, {I, start_link, [Args]}, permanent, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, { {one_for_one, 100, 10}, conf_to_childspec()}}.

%% TODO: check for duplicate ports in sys.config. ( or crash )
%% TODO: for ranch connections, check that the refs are unique. ( or crash )

conf_to_childspec() ->
    {ok, IncProtos} = application:get_env(in, incoming_protocols),
    lists:foldl(fun({protocol, ProtoArgs}, ChildSpecAcc) when is_list(ProtoArgs) ->
        case proplists:lookup(type, ProtoArgs) of
            {type, ranch_tcp_ipv4} ->
                Ref = proplists:get_value(unique_reference, ProtoArgs),
                NumAcceptrs = proplists:get_value(num_acceptors, ProtoArgs),
                TcpIpV4Port = proplists:get_value(tcp_v4_port, ProtoArgs),
                ListenerSpec = ranch:child_spec(
                    Ref, NumAcceptrs, ranch_tcp,
                    [{port, TcpIpV4Port}], in_ranch_tcp_ipv4_protocol, ProtoArgs
                ),
                [ListenerSpec | ChildSpecAcc];
            {type, tcp_v4_socket} ->
                [?CHILD(in_tcp_v4_socket_sup, ProtoArgs, supervisor) | ChildSpecAcc];
            {type, http} ->
                %% Http most likely also does not need a supervisor, if cowboy used...
                [?CHILD(in_http_sup, ProtoArgs, supervisor) | ChildSpecAcc]
        end
    end, [], IncProtos).

% From the examples:
% ranch_tcp_ipv4_1
% ranch_tcp_ipv4_2
% ranch_tcp_ipv4_3

ranch_sockets() ->
    lists:foldl(fun
            ({{ranch_listener_sup, R}, RanchListenerSupPid, _, _}, Acc) ->
                RLSChildren = supervisor:which_children(RanchListenerSupPid),
                {ranch_conns_sup, RanchConnsSupPid, _, _} =
                    lists:keyfind(ranch_conns_sup, 1, RLSChildren),
                Connections = supervisor:which_children(RanchConnsSupPid),
                lists:map(fun({T, Pid, worker, [M]}) ->
                        {Pid, R, T, M}
                    end, Connections) ++ Acc;
            (_, Acc) ->
                Acc
    end, [], supervisor:which_children(in_sup)).