-module(in_proc_buff).

-behaviour(gen_server).
-include_lib("amqp_client/include/amqp_client.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([forward/1]).

-define(STATE, in_proc_buff_state).
-record(?STATE, {
    connected = false,
    amqp_connection_opts,
    amqp_connection,
    amqp_channel,
    mqtt,
    drain_tref, %% TODO: Check that the draining timer is still running.
    connection_retry_tref
}).

%% ***
%% What about channels that stay open when this GS crashes!

%% ---------------------------------------------------------------------------

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

forward(Msg) ->
    gen_server:call(?MODULE, {forward, Msg}, infinity).


%% ---------------------------------------------------------------------------

init({}) ->
    false = process_flag(trap_exit, true),
    self() ! connect,
    {ok, #?STATE{}}.

%% ---------------------------------------------------------------------------

handle_call({forward, Msg}, _From, #?STATE{ connected = false } = State) ->
    % true = store_msg(Msg),
    error_logger:error_msg("D", []),
    {reply, ok, State};
handle_call({forward, Msg}, _From, State) ->
    case fwd_msg(State, Msg) of
        {ok, State2} ->
            {reply, ok, State2};
        {error, State3} ->
            {stop, publish_error, State3}
    end;
handle_call(_Request, _From, State) ->
	{reply, {error, unknown_call}, State}.

%% ---------------------------------------------------------------------------

handle_cast(_Msg, State) ->
	{noreply, State}.


%% ---------------------------------------------------------------------------

% 1:
% [in_proc_buff] Received {'EXIT', <0.1408.0>, socket_closed_unexpectedly}22:34:03.250 [warning] lager_error_logger_h dropped 79 messages in the last second that exceeded the limit of 50 messages/sec
% .[in_proc_buff] Received {'EXIT', <0.1528.0>, {infrastructure_died,
%                                               {writer,send_failed,badarg}}

% 2:
% (in@127.0.0.1)2> [in_proc_buff] Received {'EXIT', <0.2320.0>, socket_closed_unexpectedly}[in_proc_buff] Received {'EXIT', <0.2329.0>, killed}22:35:22.190 [error] gen_server <0.2320.0> terminated with reason: socket_closed_unexpectedly
% 22:35:22.190 [error] CRASH REPORT Process <0.2320.0> with 0 neighbours exited with reason: socket_closed_unexpectedly in gen_server:terminate/7 line 826
% 22:35:22.191 [error] Supervisor {<0.2318.0>,amqp_connection_sup} had child connection started with amqp_gen_connection:start_link(<0.2319.0>, {amqp_params_network,<<"guest">>,<<"guest">>,<<"/">>,"127.0.0.1",5672,0,0,0,infinity,none,[#Fun<amq..>,...],...}) at <0.2320.0> exit with reason socket_closed_unexpectedly in context child_terminated
% 22:35:22.191 [error] Supervisor {<0.2318.0>,amqp_connection_sup} had child connection started with amqp_gen_connection:start_link(<0.2319.0>, {amqp_params_network,<<"guest">>,<<"guest">>,<<"/">>,"127.0.0.1",5672,0,0,0,infinity,none,[#Fun<amq..>,...],...}) at <0.2320.0> exit with reason reached_max_restart_intensity in context shutdown

handle_info({'EXIT', Chan, shutdown}, #?STATE{ amqp_channel = Chan } = State) ->
    io:format("[~p] Received {'EXIT', ~p, ~p}", [?MODULE, Chan, shutdown]),
    try_close(State),
    {stop, channel_shutdown, State#?STATE{ connected = false,
                                  amqp_channel = undefined,
                                  amqp_connection = undefined }};
handle_info({'EXIT', FromPid, Reason}, State) ->
    % ok = amqp_channel:close(Chan),
    % ok = amqp_connection:close(Conn),
    % {noreply, State#?STATE{
    %                     connected = false,
    %                     amqp_connection_opts = undefined,
    %                     amqp_connection = undefined,
    %                     amqp_channel = undefined
    %         }};
    % TODO: check if Conn and Chan is still alive, and then close it.
    io:format("[~p] Received {'EXIT', ~p, ~p}", [?MODULE, FromPid, Reason]),
    {noreply, State};
handle_info(connect, State) ->
    {ok, DrainTRef} = check_sink(self()),
    State2 = State#?STATE{ drain_tref = DrainTRef },
    {ok, PB} = application:get_env(in, proc_buff),
    {amqp, AMQP} = proplists:lookup(amqp, PB),
    {connection, ConnOpts} = proplists:lookup(connection,AMQP),
    ConnParams =
        case proplists:lookup(type, ConnOpts) of
            {type,network} ->
                {username,U}  = proplists:lookup(username, ConnOpts),
                {passwd,Pw} = proplists:lookup(passwd, ConnOpts),
                {host,H}  = proplists:lookup(host, ConnOpts),
                {port,Po} = proplists:lookup(port, ConnOpts),
                #amqp_params_network{username=U, password=Pw, host=H, port=Po};
            {type,direct} ->
                {username,U}  = proplists:lookup(username, ConnOpts),
                {passwd,Pw} = proplists:lookup(passwd, ConnOpts),
                {node,Node} = proplists:lookup(node, ConnOpts),
                #amqp_params_direct{username=U, password=Pw, node=Node}
        end,
    case amqp_connection:start(ConnParams) of
        {ok, Conn} ->
            {ok, Chan} = amqp_connection:open_channel(Conn),
            true = erlang:link(Conn),
            true = erlang:link(Chan),
            DQ =
                #'queue.declare'{
                    ticket = 0,
                    queue = <<"ipo">>,
                    passive = false,
                    durable = true,
                    exclusive = false,
                    auto_delete = false,
                    nowait = false,
                    arguments = []
                },
            #'queue.declare_ok'{} = amqp_channel:call(Chan, DQ),
            {noreply, State2#?STATE{
                        connected = true,
                        amqp_connection_opts = ConnOpts,
                        amqp_connection = Conn,
                        amqp_channel = Chan
            }};
        {error, _Reason} ->
            {ok, TimerRef} = retry_connect(self),
            {noreply, State2#?STATE{
                        connected = false,
                        amqp_connection_opts = undefined,
                        amqp_connection = undefined,
                        amqp_channel = undefined,
                        connection_retry_tref = TimerRef
                      }
            }
    end;
handle_info(check_sink, #?STATE{ amqp_channel = _Chan, connected = false } = State) ->
    {ok, DrainTRef} = check_sink(self()),
    {noreply, State#?STATE{ drain_tref = DrainTRef }};
handle_info(check_sink, #?STATE{ amqp_channel = _Chan, connected = true } = State) ->
    {ok, State4, _TblStatus} = drain_buffer(State),
    {ok, DrainTRef} = check_sink(self()),
    {noreply, State4#?STATE{ drain_tref = DrainTRef }};
handle_info(Info, State) ->
    io:format("~p Handle info ~p \n", [?MODULE, Info]),
	{noreply, State}.

%% ---------------------------------------------------------------------------

terminate(Reason, #?STATE{ amqp_connection = _Conn, amqp_channel = _Chan} = State) ->
    io:format("[~p] terminate ~p\nState: ~p\n", [?MODULE, Reason, State]),
    try_close(State).

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ---------------------------------------------------------------------------

try_close(#?STATE{ amqp_connection = Conn, amqp_channel = Chan } = _State) ->
    try
        case amqp_channel:close(Chan) of
            Result when Result == ok; Result == closing ->
                ok;
            Result ->
                io:format("Couldn't close channel : ~p\n",[Result]),
                throw(Result)
        end,
        case amqp_connection:close(Conn) of
            ok ->
                ok;
            Error ->
                io:format("Connection closing : ~p\n", [Error]),
                throw(Error)
        end
    catch
        C:E ->
            io:format("Channel/Connection closing failure:~p ~p\n",[C, E]),
            {C, E, erlang:get_stacktrace()}
    end.

fwd_msg(#?STATE{ amqp_channel = Chan } = State, Msg) ->
    P = #'P_basic'{ content_type = <<"text/plain">> },
    Pub = #'basic.publish'{
        % exchange = Exchange,
        routing_key = <<"ipo">>
    },
    AMQPMsg = #amqp_msg{props = P,
                        payload = Msg},
    try
        ok = amqp_channel:call(Chan, Pub, AMQPMsg),
        {ok, State}
    catch
        C:E ->
            io:format("Failed publishing message:~p ~p ~p\n",[C, E, erlang:get_stacktrace()]),
            true = store_msg(Msg),
            %% TODO: maybe check the status of those pids:
            {error, State#?STATE{ connected = false,
                                  amqp_channel = undefined,
                                  amqp_connection = undefined }}
    end.

% drain('$end_of_table', State) ->
%     {ok, State};
% drain(Key, State) ->
%     io:format("D",[]),
%     [{Key,Msg}] = ets:lookup(?MODULE, Key),
%     case fwd_msg(State, Msg) of
%         {ok, State2} ->
%             true = ets:delete_object(?MODULE, {Key,Msg}),
%             Next = ets:next(?MODULE, Key),
%             drain(Next, State2);
%         {error, State3} ->
%             {error, State3}
%     end.

drain_buffer(State) ->
    % true = ets:safe_fixtable(?MODULE, true),
    % {ok, State4, TblStatus} =
    %     case ets:first(?MODULE) of
    %         '$end_of_table' ->
    %             {ok, State, empty};
    %         First ->
    %             case drain(First, State) of
    %                 {ok, State2} ->
    %                     {ok, State2, draining};
    %                 {error, State3} ->
    %                     {ok, State3, error}
    %             end
    %     end,
    % true = ets:safe_fixtable(?MODULE, false),
    {ok, State, empty}.

retry_connect(_Pid) ->
    {ok, _TRef} = timer:send_after(250, ?MODULE, connect).

check_sink(_Pid) ->
    {ok, _TRef} = timer:send_after(5000, ?MODULE, check_sink).

store_msg(_Msg) ->
    % Key = erlang:unique_integer([positive, monotonic]),
    % true = ets:insert_new(?MODULE, {Key, Msg}).
    ok.