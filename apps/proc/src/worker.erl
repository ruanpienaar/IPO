%io:format("", []),

%% This worker connects to 2 ports, in_port, out_port

-module(worker).
-export([
    start_link/0, start_link/2,
    send_something_out/1,
    start_in/1,
    start_out/1
]).

-behaviour(gen_fsm).
-export([init/1, handle_info/3, terminate/3, code_change/4, handle_event/3, handle_sync_event/4]).

%% Possible State's:
-export([
    connecting/2, connecting/3,
    only_out_connected/2, only_out_connected/3,
    only_in_connected/2, only_in_connected/3,
    connected/2, connected/3
]).

-define(SERVER, ?MODULE).

%%------------------------------------------------
%% API:

start_link() ->
    start_link({"0.0.0.0", 9090}, {"0.0.0.0", 9090}).

start_link({InHost, InPort}, {OutPort, OutHost}) ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [{InHost, InPort}, {OutPort, OutHost}], []).

send_something_out(Data) ->
    gen_fsm:sync_send_event(?MODULE, {send, Data}).

%%------------------------------------------------

init([{InHost, InPort}, {OutHost, OutPort}]) ->
    {ok, connecting, _State=#{
        in_host => InHost
        , in_port => InPort
        , out_host => OutHost
        , out_port => OutPort}, 0}.

%%------------------------------------------------

connecting(timeout, #{ in_host := InHost, in_port := InPort, out_host := OutHost, out_port := OutPort } = State) ->
    io:format("CONNECTING!!!!!"),
    case do_connect(OutHost, OutPort) of
        {ok, OutPortPid} ->
            ok = gen_tcp:controlling_process(OutPortPid, self()),
            case do_connect(InHost, InPort) of
                {ok, InPortPid} ->
                    ok = gen_tcp:controlling_process(InPortPid, self()),
                    {next_state, connected, State#{ in_port_pid => InPortPid, out_port_pid => OutPortPid }};
                {error, InReason} ->
                    io:format("...~p...~p:~p...\n",[InReason, InHost, InPort]),
                    {next_state, only_out_connected, State#{ out_port_pid => OutPortPid }, 1000}
            end;
        {error, OutReason} ->
            io:format("...~p...~p:~p...\n",[OutReason, OutHost, OutPort]),
            {next_state, connecting, State, 1000}
    end.

connecting(_Event, _From, State) ->
    {reply, ok, connecting, State}.

only_out_connected(timeout, #{ in_host := InHost, in_port := InPort } = State) ->
    case do_connect(InHost, InPort) of
        {ok, InPortPid} ->
            ok = gen_tcp:controlling_process(InPortPid, self()),
            {next_state, connected, State#{ in_port_pid => InPortPid }};
        {error, InReason} ->
            io:format("...~p...~p:~p...\n",[InReason, InHost, InPort]),
            {next_state, only_out_connected, State, 0}
    end.

only_out_connected(_Event, _From, State) ->
    {reply, ok, only_out_connected, State}.

only_in_connected(timeout, #{ out_host := OutHost, out_port := OutPort } = State) ->
    case do_connect(OutHost, OutPort) of
        {ok, OutPortPid} ->
            ok = gen_tcp:controlling_process(OutPortPid, self()),
            
            {next_state, connected, State#{ out_port_pid => OutPortPid }};
            
        {error, OutReason} ->
            io:format("...~p...~p:~p...\n",[OutReason, OutHost, OutPort]),
            {next_state, connecting, State, 1000}
    end.

only_in_connected(_Event, _From, State) ->
    {reply, ok, only_in_connected, State}.

connected(_Event, State) ->
    {reply, ok, connected, State}.

connected({send, Data}, _From, #{ out_port_pid := P } = State) ->
    {reply, gen_tcp:send(P, Data), connected, State};
connected(_Event, _From, State) ->
    {reply, ok, connected, State}.

%%------------------------------------------------

handle_event(Event, StateName, State) ->
    io:format("handle_event(~p,~p,~p)\n", [Event, StateName, State]),
    {next_state, StateName, State}.

%%------------------------------------------------

handle_sync_event(Event, From, StateName, State) ->
    io:format("handle_sync_event(~p,~p,~p,~p)\n", [Event, From, StateName, State]),
    {reply, ok, StateName, State}.

%%------------------------------------------------
handle_info({tcp, InPortPid, Data}, StateName, #{ in_port_pid := InPortPid, out_port_pid := OutPortPid } = State) ->
    case do_send(OutPortPid, Data) of
        ok ->
            {next_state, StateName, State};
        error ->
            {next_state, check_connections, State, 0}
    end;
%% TODO: do we really want bi-directional traffic ?
handle_info({tcp, OutPortPid, Data}, StateName, #{ in_port_pid := InPortPid, out_port_pid := OutPortPid } = State) ->
    case do_send(InPortPid, Data) of
        ok ->
            {next_state, StateName, State};
        error ->
            {next_state, check_connections, State, 0}
    end;
handle_info({tcp_closed, Socket}, connected, #{ in_port_pid := InPortPid } = State) when Socket =:= InPortPid ->
    {next_state, only_out_connected, State#{ in_port_pid => undefined }, 0};
handle_info({tcp_closed, Socket}, connected, #{ out_port_pid := OutPortPid } = State) when Socket =:= OutPortPid ->
    {next_state, only_in_connected, State#{ out_port_pid => undefined }};
handle_info(Info, StateName, State) ->
    io:format("handle_info(~p,~p,~p)\n", [Info, StateName, State]),
    {next_state, StateName, State}.

%%------------------------------------------------

terminate(Reason, _StateName, _State) ->
    io:format("Terminate Reason: ~p\n", [Reason]),
    ok.

%%------------------------------------------------

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%------------------------------------------------

do_connect(Host, Port) ->
    io:format("do_connect ~p ~p~n", [Host, Port]),
    case gen_tcp:connect(Host, Port, [binary, {packet, 2}]) of 
        {ok, PortPid} ->
            {ok, PortPid};
        {error, Reason} ->
            io:format("...Connection failed:~p...\n",[Reason]),
            {error, Reason}
    end.

%%------------------------------------------------
%% Helpers

start_in(Port) -> %% Send in
    listen_loop_try(in, Port).

start_out(Port) -> %% Recv Out
    listen_loop_try(out, Port).
%%------------------------------------------------

listen_loop_try(Mode, Port) ->
    io:format("...~p...listening...\n", [Port]),
    case gen_tcp:listen(Port, [binary, {active,false}, {reuseaddr, true}, {packet, 2}]) of 
        {ok, LSock} ->
            listen_loop_try(Mode,LSock,Port);
        {error, eaddrinuse} ->
            io:format("...~p...address in use...\n", [Port]);
        {error, Error} ->
            io:format("...~p...~p...\n",[Error, Port]),
            listen_loop_try(Mode, Port)
    end.

listen_loop_try(in, LSock, Port) ->
    {ok,PortPid} = gen_tcp:accept(LSock),
    send_loop(LSock, Port, PortPid);
listen_loop_try(out, LSock, Port) ->
    {ok,PortPid} = gen_tcp:accept(LSock),
    recv_loop(LSock, Port, PortPid).

recv_loop(LSock, Port, PortPid) ->
    case gen_tcp:recv(PortPid, 0) of 
        {ok, Data} ->
            io:format("~p\n",[Data]),
            recv_loop(LSock, Port, PortPid);
        {error, Error} ->
            gen_tcp:close(PortPid),
            io:format("...~p...recv error ~p...\n", [Port, Error]),
            listen_loop_try(out, LSock, Port)
    end.

send_loop(LSock, Port, PortPid) ->
    timer:sleep(1000),
    case do_send(PortPid, <<"Something">>) of
        error ->
            listen_loop_try(in, LSock, Port);
        ok ->
            send_loop(LSock, Port, PortPid)
    end.

do_send(PortPid, Data) ->
    try 
        ok = gen_tcp:send(PortPid, Data)
    catch
        C:E ->
            gen_tcp:close(PortPid),
            io:format("...send error ~p...\n", [{C,E}]),
            error
    end.
    % case gen_tcp:recv(PortPid, 0) of 
    %     {ok, Data} ->
    %         io:format("~p\n",[Data]),
    %         recv_loop(LSock, Port, PortPid);
    %     {error, Error} ->
    %         gen_tcp:close(PortPid),
    %         io:format("...~p...recv error ~p...\n", [Port, Error]),
    %         listen_loop_try(LSock, Port)
    % end.
