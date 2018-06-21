-module(in_tcp4_socket).
-export([start_link/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(STATE, in_tcp_socket_stream_state).
-record(?STATE,{ socket }).

start_link(Socket) ->
    gen_server:start_link(?MODULE, [Socket], []).

init([Socket]) ->
    io:format("...\ninit : ~p\n",[Socket]),
    ok = inet:setopts(Socket, [{active, once}]),
    {ok, #?STATE{ socket = Socket }}.

handle_call(Request, _From, State) ->
    io:format("Handle call: ~p\n",[Request]),
    {reply, {error, unknown_call}, State}.

handle_cast(Msg, State) ->
    io:format("Handle cast: ~p\n",[Msg]),
    {noreply, State}.

handle_info({tcp, Socket, _Data}, #?STATE{ socket = Socket } = State) ->
    ok = inet:setopts(Socket, [{active, once}]),
    io:format(".", []),
    %ok = in_proc_buff:forward(Data),
    {noreply, State}.

terminate(Reason, _State) ->
    %% XXX maybe log ets entries to disk.......
    lager:error("Module ~p terminated ...\n",[?MODULE,Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.