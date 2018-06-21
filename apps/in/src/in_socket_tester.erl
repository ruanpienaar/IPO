-module(in_socket_tester).
-export([test_port/0]).

test_port() ->
    Socks = sockets(8888),
    data(Socks).

sockets(Port) ->
    [ connect(Port) || _X <- lists:seq(1,10)].

connect(Port) ->
    {ok,S} = gen_tcp:connect("0.0.0.0", Port,
                    [binary, {reuseaddr, true}, {active, once}], 1000),
    S.

data(Socks) ->
    data(Socks,Socks).

data(Socks,[]) ->
    data(Socks,Socks);
data(Socks,[H|T]) ->
    timer:sleep(9),
    % {MegaSecs,Secs,MicroSecs} = erlang:now(),
    % L = integer_to_list( MegaSecs + Secs + MicroSecs ),
    % ok = gen_tcp:send(H, list_to_binary(L)),
    ok = gen_tcp:send(H, <<"Stuff">>),
    data(Socks,T).