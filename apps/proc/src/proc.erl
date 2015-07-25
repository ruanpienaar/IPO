-module(proc).
-export([
	execute/1
]).

execute(Data) ->
	io:format("... PROCESS SOME DATA HERE ..."),
	_NewData = Data.