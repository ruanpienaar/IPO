-module(fsm).
-export([
    start_link/0,
    async_get_state/0,
    can_process/0
]).

-behaviour(gen_fsm).
-export([init/1, handle_info/3, terminate/3, code_change/4, handle_event/3, handle_sync_event/4]).
-export([try_connect/2,
         take_control/2,
         connected/2,
         disconnecting/2
]).

-define(SERVER, ?MODULE).
-define(WAIT, 1000).

%% ----------

start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, {}, []).

% Query what state the fsm is in.
async_get_state() ->
    gen_fsm:sync_send_all_state_event(?MODULE, get_state).

can_process() ->
    gen_fsm:sync_send_all_state_event(?MODULE, can_process).

%% ----------

%% Note: 
%% I'm not including the StateName/3 variant, as people are not supposed to be calling that.

init({}) ->
    {ok, try_connect, #{ host => "localhost", port => 9999 }, 0}.

try_connect(timeout, #{ host := H, port := P} = State) ->
    io:format("try_connect/2 handled event:~p~n", [timeout]),
    case do_connect(H, P) of
        {ok, Socket} ->
            {next_state, take_control, State#{ socket => Socket }, 0};
        {error, Reason} ->
            io:format("connect failed ~p~n", [Reason]),
            try_connect_again(State)
    end.

try_connect_again(State) ->
    {next_state, try_connect, State, ?WAIT}.

take_control(timeout, #{ socket := Socket } = State) ->
    case gen_tcp:controlling_process(Socket, self()) of
        {error, Reason} ->
            io:format("could not take control over socket ~p~n", [Reason]),
            ok = gen_tcp:close(Socket),
            {next_state, try_connect, State#{socket => undefined}, ?WAIT};
        ok ->
            {next_state, connected, State}
    end.

connected(_Event, State) ->
    {next_state, connected, State}.

disconnecting(_Event, State) ->
    {next_state, disconnecting, State}.
 
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(get_state, _From, StateName, State) ->
    {reply, {ok, StateName}, StateName, State};
handle_sync_event(can_process, _From, StateName=ready, State) ->
    {reply, true, StateName, State};
handle_sync_event(can_process, _From, StateName, State) ->
    {reply, false, StateName, State};
handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

handle_info({tcp_closed, Socket}, connected, #{ socket := Socket } = State) ->
    try_connect_again(State#{ socket => undefined });
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

do_connect(Host, Port) ->
    io:format("do_connect ~p ~p~n", [Host, Port]),
    case gen_tcp:connect(Host, Port, [binary, {packet, 2}]) of 
        {ok, PortPid} ->
            {ok, PortPid};
        {error, Reason} ->
            io:format("...Connection failed:~p...\n",[Reason]),
            {error, Reason}
    end.