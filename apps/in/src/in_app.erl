-module(in_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->

	%% Maybe move into module, and make private...?
    in_proc_buff = 
        ets:new(in_proc_buff, [named_table,
							   duplicate_bag,
                               public,
                               {write_concurrency,true},
                               {read_concurrency,true}
                              ]),

    in_sup:start_link().

stop(_State) ->
    ok.