-module(in_tcp_v4_socket).
-export([start_link/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([accept/1]).

-define(STATE, in_tcp_v4_socket_state).
-record(?STATE,{}).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Args], []).

init([Args]) ->
    io:format("Args : ~p\n",[Args]),
    {tcp_v4_port,Port} = proplists:lookup(tcp_v4_port,Args),
    {listen_opts,Opts} = proplists:lookup(listen_opts,Args),
    {ok, LSocket} = gen_tcp:listen(Port,Opts),
    spawn_link(?MODULE, accept, [LSocket]),
    {ok, #?STATE{}}.

handle_call(Request, _From, State) ->
    io:format("Unhandled Requst ~p ~n", [Request]),
    {reply, {error, unknown_call}, State}.

handle_cast(Msg, State) ->
    io:format("Unhandled Msg ~p ~n", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    io:format("Unhandled Info ~p ~n", [Info]),
    {noreply, State}.

terminate(Reason, _State) ->
    io:format("Terminate ~p ~p~n", [?MODULE, Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

accept(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            case in_tcp_v4_socket_stream_sup:start_child(Socket) of
                {ok, ConnCtrlPID} ->
                    io:format("New Controlling Child...~p\n",[ConnCtrlPID]),
                    case gen_tcp:controlling_process(Socket, ConnCtrlPID) of
                        ok ->
                            accept(ListenSocket);
                        {error, _Error} ->
                            %%TODO : implement some behavior
                            accept(ListenSocket)
                    end;
                {error,_Reason} ->
                    %%TODO : implement some behavior
                    accept(ListenSocket)
            end;
        _Error ->
            %%TODO : implement some behavior
            accept(ListenSocket)
    end.