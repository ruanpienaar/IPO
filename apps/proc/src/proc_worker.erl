-module(proc_worker).

-behaviour(gen_server).
-include_lib("amqp_client/include/amqp_client.hrl").

-export([start_link/0]).
-export([
	manually_consume/0
]).

-define(STATE, proc_worker_state).
-record(?STATE, {
    amqp_connection_opts,
    amqp_connection,
    amqp_channel,
    mqtt
}).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

manually_consume() ->
	ok.

init({}) ->
    {ok,PB} = application:get_env(proc, proc_buff),
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
    BC = #'basic.consume'{ queue = <<"ipo_in">> },
    #'basic.consume_ok'{} = amqp_channel:subscribe(Chan, BC, self()),
    {ok, #?STATE{
                amqp_connection_opts = ConnOpts,
                amqp_connection = Conn,
                amqp_channel = Chan
    }}.

handle_call(Req, _From, State) ->
    io:format("handle_call ~p\n",[Req]),
    {reply, {error, unknown_call}, State}.

handle_cast(Msg, State) ->
    io:format("handle_cast ~p\n",[Msg]),
    {noreply, State}.

handle_info({#'basic.deliver'{delivery_tag = DT}, 
             #amqp_msg{ payload = Data }}, #?STATE{amqp_channel = Chan} = State) ->
    io:format("Executing Data : ~p\n",[Data]),
    NewData = proc:execute(Data),

    ok = proc_out_buff:forward(NewData),

    %% Acknoledge
    ACK = #'basic.ack'{
        delivery_tag = DT,
        multiple = false
    },
    ok = amqp_channel:call(Chan, ACK),

    {noreply, State};

handle_info(Info, State) ->
    io:format("handle_info ~p\n",[Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.