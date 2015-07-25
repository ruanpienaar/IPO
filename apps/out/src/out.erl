-module (out).
-export([send/1]).

send(Data) ->
	%% get a worker...
	%% choose a proto...
	gen_server:call(out_tcp_v4_socket, {send, Data}).
