-module(c_client_udp).

-export([ connect/2
        , start_link/0
        , show/2
        , change_name/2
        , disconnect/1
        , speak/2
        ]).

-define(REEXPORT_0(M, F), F() -> M:F()).
-define(REEXPORT_1(M, F), F(A0) -> M:F(A0)).
-define(REEXPORT_2(M, F), F(A0, A1) -> M:F(A0, A1)).

% API

connect(ClientPid, ServerAddr) ->
    {ok, ServerPid} = c_server_udp:get_pid(ServerAddr),
    c_client:connect(ClientPid, ServerPid).

?REEXPORT_0(c_client, start_link).
?REEXPORT_2(c_client, show).
?REEXPORT_2(c_client, change_name).
?REEXPORT_1(c_client, disconnect).
?REEXPORT_2(c_client, speak).
