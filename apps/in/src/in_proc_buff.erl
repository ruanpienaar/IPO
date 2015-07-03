-module(in_proc_buff).

-behaviour(gen_server).

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
	gen_server:call(?MODULE, {forward, Msg}).

init({}) ->
    {ok,PB} = application:get_env(in, proc_buff),
    {amqp,AMQP} = proplists:lookup(amqp, PB),
    {connection,ConnOpts} = proplists:lookup(connection,AMQP),
    {username,U}  = proplists:lookup(username, ConnOpts),
    {passwd,Pw} = proplists:lookup(passwd, ConnOpts),
    {vhost,VH} = proplists:lookup(vhost, ConnOpts),
    {host,H}  = proplists:lookup(host, ConnOpts),
    {port,Po} = proplists:lookup(port, ConnOpts),
    {ok,[Conn, Chan]} = bunny:connect(U, Pw, VH, H, Po),
    ok = bunny:queue_declare(Chan, <<"in_proc_buff_queue">>),
	{ok, #?STATE{
        amqp_connection_opts = ConnOpts,
        amqp_connection = Conn,
        amqp_channel = Chan
    }}.

handle_call({forward, Msg}, _From, #?STATE{ amqp_channel = AC } = State) ->
    %% Possibly use noreply, and only reply, when you get a receipt back....
    ok = bunny:send_msg(AC, Exhange =   <<"">>, <<"in_proc_buff_queue">>, Msg),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
	{reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, State) ->
    %% XXX: close connection and channel
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.