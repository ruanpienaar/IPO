-module(in_ranch_protocol).

-behaviour(gen_server).
-behaviour(ranch_protocol).

-define(STATE,in_ranch_protocol_state).

-record(?STATE,{
    ranch_listener_pid,
	socket,
	transport,
    ref
}).

-export([ start_link/1,
          start_link/4
]).

-export([ init/1,
          init/4,
          handle_call/3,
          handle_cast/2,
          handle_info/2,
          terminate/2,
          code_change/3
]).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).
 
init(Args) ->
    {listen_opts, Opts} = proplists:lookup(listen_opts, Args),
    {tcp_v4_port, Port} = proplists:lookup(tcp_v4_port, Args),
    {ok, RanchListener} = ranch:start_listener(in_ranch, 100, ranch_tcp,
                                   [{port, Port}] ++ Opts, in_ranch_protocol, []),
    {ok, #?STATE{ ranch_listener_pid = RanchListener }}.

init(Ref, Socket, Transport, _Opts = []) ->
    ok = proc_lib:init_ack({ok, self()}),
    %% Perform any required state initialization here.
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}]),
    % gen_server:enter_loop(?MODULE, [], {state, Socket, Transport}).
    gen_server:enter_loop(?MODULE, [], #?STATE{ ref = Ref, 
                                                socket = Socket,
                                                transport = Transport }).

% TODO: Fix
% 23:41:06.987 [error] CRASH REPORT Process <0.1533.0> with 0 neighbours exited with reason: no function clause matching in_ranch_protocol:handle_info({tcp_closed,#Port<0.4084>}, {in_ranch_protocol_state,#Port<0.4084>,ranch_tcp}) line 32 in gen_server:terminate/7 line 826

handle_call(Request, _From, State) ->
	io:format("HANDLE call ~p\n", [Request]),
	{reply, {error, unknown_call}, State}.

handle_cast(Msg, State) ->
	io:format("HANDLE cast ~p\n", [Msg]),
	{noreply, State}.

handle_info({tcp_closed, _Socket}, #?STATE{ ref = Ref } = State) ->
    %% Have the supervisor restart this Socket Listener.
    io:format("tcp_closed!!\n", []),
    % ok = ranch:stop_listener(Ref),
    {noreply, State};
handle_info({tcp, Socket, Data}, #?STATE{ transport = Transport, socket = Socket } = State) ->
	ok = Transport:setopts(Socket, [{active, once}]),
	io:format(".", []),
	%% TODO: build a generic forwarder + Queue mech
	ok = in_proc_buff:forward(Data),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.