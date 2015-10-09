-module(in_proc_buff).

-behaviour(gen_server).
-include_lib("amqp_client/include/amqp_client.hrl").

-export([start_link/0]).
-export([
	forward/1
]).

-define(STATE, in_proc_buff_state).
-record(?STATE, {
    connected = false,
    amqp_connection_opts,
    amqp_connection,
    amqp_channel,
    mqtt
}).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ---------------------------------------------------------------------------

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

forward(Msg) -> 
    gen_server:call(?MODULE, {forward, Msg}, infinity).

%% What about channels that stay open when this GS crashes!

%% ---------------------------------------------------------------------------

init({}) ->
    false = process_flag(trap_exit, true),
    self() ! connect,
    {ok, #?STATE{}}.

handle_call({forward, Msg}, _From, #?STATE{ connected = false } = State) ->
    %% Maybe throttle here...?
    Key = erlang:unique_integer([positive, monotonic]),
    true = ets:insert_new(?MODULE, {Key, Msg}),
    {reply, ok, State};
handle_call({forward, Msg}, _From, #?STATE{ connected = true, amqp_channel = Chan } = State) ->
    fwd_msg(Msg, Chan),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
	{reply, {error, unknown_call}, State}.

handle_cast(check_drained, #?STATE{ amqp_channel = Chan } = State) ->
    case drain_buffer(Chan) of 
        empty ->
            {noreply, State};
        draining ->
            check_drained(self()),
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({'EXIT',_FromPid,Reason}, #?STATE{ amqp_connection = Conn, amqp_channel = Chan} = _State) ->
    ok = amqp_channel:close(Chan),
    ok = amqp_connection:close(Conn),
    {stop, Reason};
handle_info(connect, State) ->
    {ok,PB} = application:get_env(in, proc_buff),
    {amqp,AMQP} = proplists:lookup(amqp, PB),
    {connection,ConnOpts} = proplists:lookup(connection,AMQP),
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
            DQ = 
                #'queue.declare'{
                    ticket = 0,
                    queue = <<"ipo_in">>,
                    passive = false,
                    durable = true,
                    exclusive = false,
                    auto_delete = false,
                    nowait = false,
                    arguments = []
                },
            %% Maybe case>>
            #'queue.declare_ok'{} = amqp_channel:call(Chan, DQ),

            %% Check the ETS table, and drain that first.......before starting
            % XXX: i was trying to use a duplicate bag...
            drain_buffer(Chan),
            check_drained(self()),

            {noreply, #?STATE{
                        connected = true,
                        amqp_connection_opts = ConnOpts,
                        amqp_connection = Conn,
                        amqp_channel = Chan
            }};
        {error, Reason} ->
            io:format("...failed to connect to rabbitmq...~p...\n",[Reason]),
            timer:sleep(25),
            {stop, Reason, State}
    end;
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, #?STATE{ amqp_connection = Conn, amqp_channel = Chan} = _State) ->
    amqp_channel:close(Chan),
    amqp_connection:close(Conn),
    ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ---------------------------------------------------------------------------

fwd_msg(Msg, Chan) ->
    %% XXX: on publish failure, timer:call after X seconds, and retry...
    % Publish
    P = #'P_basic'{ content_type = <<"text/plain">> },
    Pub = #'basic.publish'{
        % exchange = Exchange,
        routing_key = <<"ipo_in">>
    },
    AMQPMsg = #amqp_msg{props = P, 
                        payload = Msg},
    ok = amqp_channel:cast(Chan, Pub, AMQPMsg).

drain('$end_of_table', _Chan) ->
    %% Clear table...
    %% true = ets:delete_all_objects(?MODULE),
    ok;
drain(Key, Chan) ->
    io:format("Draining ~p\n\n",[Key]), 
    [{Key,Msg}] = ets:lookup(?MODULE, Key),
    %% XXX: also check sent properly...
    ok = fwd_msg(Msg, Chan),
    Next = ets:next(?MODULE, Key),
    drain(Next, Chan).

drain_buffer(Chan) ->
    true = ets:safe_fixtable(?MODULE, true),
    TblStatus = 
        case ets:first(?MODULE) of 
            '$end_of_table' ->
                empty;
            First ->
                ok = drain(First, Chan),
                draining
        end,
    true = ets:safe_fixtable(?MODULE, false),
    TblStatus.

%retry_connect(Pid) ->
%    io:format("Retry connection at 5000 sec...\n"),
    %{ok,_} = timer:send_after(5000, ?MODULE, connect, [Pid]).
%    ok.

check_drained(Pid) ->
    timer:apply_after(250, gen_server, cast, [Pid, check_drained]).