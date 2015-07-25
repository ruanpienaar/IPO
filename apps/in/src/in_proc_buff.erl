-module(in_proc_buff).

-behaviour(gen_server).
-include_lib("amqp_client/include/amqp_client.hrl").

-export([start_link/0]).
-export([
	forward/1
]).

-define(STATE, in_proc_buff_state).
-record(?STATE, {
    amqp_connection_opts,
    amqp_connection,
    amqp_channel,
    mqtt
}).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

forward(Msg) -> 
    case whereis(?MODULE) of 
        undefined ->
            %% Maybe throttle here...?
            Key = erlang:unique_integer([positive, monotonic]),
            true = ets:insert_new(?MODULE, {Key, Msg}),
            ok;
        _Pid ->
            gen_server:call(?MODULE, {forward, Msg}, 60000)
    end.

%% What about channels that stay open when this GS crashes!

%% ---------------------------------------------------------------------------


init({}) ->
    false = process_flag(trap_exit, true), 
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
    {ok, Conn} = amqp_connection:start(ConnParams),
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
    true = ets:safe_fixtable(?MODULE, true),
    case ets:first(?MODULE) of 
        '$end_of_table' ->
            ok;
        First ->
            % The initialization will pause the startup, when draining, 
            % also notice the infinity on {forward, msg}, so that too will hang, until ready.
            ok = drain(First, Chan)
    end,
    true = ets:safe_fixtable(?MODULE, false),

    {ok, #?STATE{
                amqp_connection_opts = ConnOpts,
                amqp_connection = Conn,
                amqp_channel = Chan
    }}.

handle_call({forward, Msg}, _From, #?STATE{ amqp_channel = Chan } = State) ->
    fwd_msg(Msg, Chan),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
	{reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({'EXIT',_FromPid,Reason}, #?STATE{ amqp_connection = Conn, amqp_channel = Chan} = _State) ->
    amqp_channel:close(Chan),
    amqp_connection:close(Conn),
    {stop, Reason};
handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, #?STATE{ amqp_connection = Conn, amqp_channel = Chan} = _State) ->
    amqp_channel:close(Chan),
    amqp_connection:close(Conn),
    ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

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
    true = ets:delete_all_objects(?MODULE),
    ok;
drain(Key, Chan) ->
    io:format("Draining ~p\n\n",[Key]), 
    [{Key,Msg}] = ets:lookup(?MODULE, Key),
    %% XXX: also check sent properly...
    ok = fwd_msg(Msg, Chan),
    Next = ets:next(?MODULE, Key),
    drain(Next, Chan).