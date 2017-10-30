-module(proc_out_buff).

-behaviour(gen_server).
-include_lib("amqp_client/include/amqp_client.hrl").

-export([start_link/0,
         start_link/1
        ]).
-export([
	forward/1
]).

-define(STATE, proc_out_buff_state).
-record(?STATE, {
    amqp_connection_opts,
    amqp_connection,
    amqp_channel,
    mqtt
}).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
	gen_server:start_link(?MODULE, {}, []).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

forward(Msg) ->
	gen_server:call(?MODULE, {forward, Msg}).

%% ---------------------------------------------------------------------------

init(Args) ->
    io:format("~p init\nArgs ~p\n",[?MODULE, Args]),
    {ok,PB} = application:get_env(proc, out_buff),
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
            queue = <<"ipo">>,
            passive = false,
            durable = true,
            exclusive = false,
            auto_delete = false,
            nowait = false,
            arguments = []
        },
    %% Maybe case>>
    #'queue.declare_ok'{} = amqp_channel:call(Chan, DQ),
    {ok, #?STATE{
                amqp_connection_opts = ConnOpts,
                amqp_connection = Conn,
                amqp_channel = Chan
    }}.

handle_call({forward, Msg}, _From, #?STATE{ amqp_channel = AC } = State) ->
    P = #'P_basic'{ content_type = <<"text/plain">> },
    Pub = #'basic.publish'{
        % exchange = Exchange,
        routing_key = <<"ipo_out">>
    },
    AMQPMsg = #amqp_msg{props = P,
                        payload = Msg},
    ok = amqp_channel:cast(AC, Pub, AMQPMsg),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
	{reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(Info, State) ->
    io:format("handle_info ~p\n",[Info]),
	{noreply, State}.

terminate(_Reason, #?STATE{ amqp_connection = Conn, amqp_channel = Chan} = _State) ->
    bunny:close(Conn, Chan).

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.