-module(in_proc_buff).

-behaviour(gen_server).

-export([start_link/0]).
-export([
	forward/1
]).

-record(in_proc_buff_state, {}).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

forward(Msg) ->
	gen_server:call(?MODULE, {forward, Msg}).

init({}) ->
	{ok, #in_proc_buff_state{}}.

handle_call(_Request, _From, State) ->
	{reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.