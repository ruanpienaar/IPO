-module(in_ranch_protocol).

-behaviour(gen_server).
-behaviour(ranch_protocol).
-define(STATE,in_ranch_protocol_state).
-record(?STATE,{
	socket,
	transport
}).
-export([start_link/4]).
-export([init/4, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).
 
init(Ref, Socket, Transport, _Opts = []) ->
    ok = proc_lib:init_ack({ok, self()}),
    %% Perform any required state initialization here.
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}]),
    % gen_server:enter_loop(?MODULE, [], {state, Socket, Transport}).
    gen_server:enter_loop(?MODULE, [], #?STATE{ socket = Socket,  transport = Transport }).

handle_call(Request, _From, State) ->
	io:format("HANDLE call ~p\n", [Request]),
	{reply, {error, unknown_call}, State}.

handle_cast(Msg, State) ->
	io:format("HANDLE cast ~p\n", [Msg]),
	{noreply, State}.

handle_info({tcp, Socket, Data}, #?STATE{ transport = Transport, socket = Socket } = State) ->
	ok = Transport:setopts(Socket, [{active, once}]),
	io:format(".", []),
	ok = in_proc_buff:forward(Data),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.