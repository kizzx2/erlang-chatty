#!/usr/bin/env escript
%%! -sname server
main([]) ->
    c_server_udp:start_link(),
    io:format("Server is serving...~n"),
    timer:sleep(infinity).
