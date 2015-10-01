-module (out_tcp_v4_socket).
-export([send/1]).

-export([start_link/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(STATE, in_tcp_v4_socket_state).
-record(?STATE,{socket,
				host,
				port,
				opts}).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Args], []).

send(Data) ->
	gen_server:call(?MODULE, {send, Data}, infinity).

init([Args]) ->
	{tcp_v4_host, Host} = proplists:lookup(tcp_v4_host, Args),
	{tcp_v4_port, Port} = proplists:lookup(tcp_v4_port, Args),
	{connect_opts, Opts} = proplists:lookup(connect_opts, Args),
	%% XXX: maybe retry socket failures ???
	{ok, Socket} = gen_tcp:connect(Host, Port, Opts),
    {ok, #?STATE{socket = Socket,
    			 host = Host,
    			 port = Port,
    			 opts = Opts }}.

handle_call({send, Data}, _From, #?STATE{ socket = Socket } = State) ->
	ok = gen_tcp:send(Socket, Data),
	{reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.