-module(proc_out_buff_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1,
         start_child/1
]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, Module, Type), {Id, {Module, start_link, []}, temporary, 1000, Type, [Module]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 50, 10}, []} }.
    
start_child(Id) ->
	supervisor:start_child(?MODULE, ?CHILD(Id, proc_out_buff, worker)).

