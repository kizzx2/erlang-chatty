-module(c_client_ui).
-export([main/2]).

get_line(Name) ->
    io:format("~s> ", [Name]),
    io:get_line("").

main(ServerAddr, Name) ->
    {ok, Client} = c_client_udp:start_link(),
    c_client_udp:connect(Client, ServerAddr),
    c_client_udp:change_name(Client, Name),
    loop(Client, Name).

loop(Client, Name) ->
    Msg = string:strip(get_line(Name), right, $\n),
    case Msg of
        "" -> loop(Client, Name);
        "/disconnect" -> c_client_udp:disconnect(Client);
        _ ->
            c_client_udp:speak(Client, Msg),
            loop(Client, Name)
    end.
