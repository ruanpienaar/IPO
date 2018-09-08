-module(in_udp4_socket).
-export([start_link/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(STATE, in_udp_v4_socket_state).
-record(?STATE,{socket, msg_len}).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Args], []).

init([Args]) ->
    % dbg:tracer(),
    % dbg:p(all, call),
    % dbg:tpl(gen_udp, cx),
    {udp_v4_port,Port} = proplists:lookup(udp_v4_port,Args),
    {open_opts,OpenOpts} = proplists:lookup(open_opts,Args),
    {message_length,MsgLen} = proplists:lookup(message_length,Args),
    {ok, Socket} = gen_udp:open(Port, OpenOpts),
    self() ! recv,
    {ok, #?STATE{
        socket = Socket,
        msg_len = MsgLen
    }}.

handle_call(Request, _From, State) ->
    io:format("Unhandled Requst ~p ~n", [Request]),
    {reply, {error, unknown_call}, State}.

handle_cast(Msg, State) ->
    io:format("Unhandled Msg ~p ~n", [Msg]),
    {noreply, State}.

handle_info(recv, #?STATE{ socket = Socket, msg_len = ML } = State) ->
    case gen_udp:recv(Socket, ML) of
        {ok, {Address, Port, Packet}} ->
            % io:format("{ok, {~p, ~p, ~p}}",
            %     [Address, Port, Packet]),


            % TODO: change to timeout...
            %       or tight loop
            self() ! recv,
            {noreply, State, };
        {error, Reason} ->
            {stop, {error, Reason}, State}
    end;
handle_info(Info, State) ->
    io:format("Unhandled Info ~p ~n", [Info]),
    {noreply, State}.

terminate(Reason, _State) ->
    io:format("Terminate ~p ~p~n", [?MODULE, Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.