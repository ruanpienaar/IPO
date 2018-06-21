-module(in_ranch_tcp4_protocol).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-behaviour(ranch_protocol).
-export([
    start_link/4,
    set_active_once/1,
    set_active_true/1,
    set_active_false/1
]).

-define(TIMEOUT, 60000).

start_link(Ref, Socket, Transport, Opts) ->
    {ok, proc_lib:spawn_link(?MODULE, init, [{Ref, Socket, Transport, Opts}])}.

set_active_once(Pid) ->
    Pid ! {setopts, [{active, once}]}.

set_active_true(Pid) ->
    Pid ! {setopts, [{active, true}]}.

set_active_false(Pid) ->
    Pid ! {setopts, [{active, false}]}.


init({Ref, Socket, Transport, Opts}) ->
    % io:format("Opts : ~p~n", [Opts]),
    ok = ranch:accept_ack(Ref),
    SocketOpts = proplists:get_value(socket_opts, Opts),
    SocketActiveState = proplists:get_value(active, SocketOpts, once),
    ok = Transport:setopts(Socket, SocketOpts),
    gen_server:enter_loop(?MODULE, [], #{
        ref => Ref,
        socket => Socket,
        transport => Transport,
        opts => Opts,
        socket_active_state => {active, SocketActiveState}
    }, 60000).

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({setopts, O=[{active, A}]}, #{ socket := Socket, transport := Transport } = State) ->
    ok = Transport:setopts(Socket, O),
    {noreply, State#{ socket_active_state => {active, A} }};
handle_info({tcp, Socket, Data},
        #{ socket := Socket,
           socket_active_state := {active, once},
           opts := Opts } = State) ->
    io:format("~p~n", [Data]),
    %% Here you would potentially want to control the rate at which incomming traffic arrive
    % ok = Transport:setopts(Socket, [{active, once}]),
    {ok, _} = timer:send_after(
        proplists:get_value(rate_limited, Opts, 1000),
        self(),
        {setopts, [{active, once}]}
    ),
    {noreply, State};
handle_info({tcp, Socket, Data},
        #{ socket := Socket, socket_active_state := {active, true} } = State) ->
    io:format("~p~n", [Data]),
    {noreply, State};
handle_info({tcp_closed, Socket}, #{ socket := Socket } = State) ->
    {stop, tcp_closed, State}.

terminate(_Reason, #{
        socket := Socket,
        transport := Transport } = _State) ->
    ok = Transport:close(Socket).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
