#!/usr/bin/env escript
%%! -sname client-david

main([]) ->
    c_client_ui:main({127, 0, 0, 1}, "David").
