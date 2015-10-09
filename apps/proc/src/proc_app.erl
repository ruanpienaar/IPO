-module(proc_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case proc_sup:start_link() of 
    	{ok, Pid} ->
	        [
    	    begin
    	        {ok,_} = proc_out_buff_sup:start_child(X),
    	        {ok,_} = proc_worker_sup:start_child(X)
    	    end
    	    || X <- lists:seq(1,10)
    	    ],
    	    {ok, Pid};    	
    	Reason ->
    		{error, Reason}
    end.

stop(_State) ->
    ok.
