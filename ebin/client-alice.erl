#!/usr/bin/env escript
%%! -sname client-alice

main([]) ->
    c_client_ui:main({127, 0, 0, 1}, "Alice").
