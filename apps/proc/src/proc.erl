-module(proc).
-export([
	execute/1
]).

execute(Data) ->
	io:format("X"),
	_NewData = Data.