-module(c_server_udp).
-behavior(gen_server).

-include("c_server_protocol.hrl").

-export([ code_change/3
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , init/1
        , terminate/2
        ]).

-export([ start_link/0
        , stop/1
        , get_pid/1
        ]).

-record(state, {port, server_core}).

% API

start_link() ->
    gen_server:start_link(?MODULE, [], []).

stop(ServerPid) ->
    gen_server:call(ServerPid, stop).

get_pid(ServerAddr) ->
    {ok, SendPort} = gen_udp:open(0, [binary]),
    Ref = make_ref(),
    Packet = term_to_binary({get_pid, {self(), Ref}}),
    gen_udp:send(SendPort, ServerAddr, ?SERVER_PORT_NUMBER, Packet),
    receive
        {set_pid, ServerPid, Ref} -> {ok, ServerPid};
        _Unk -> error(bad_response)
    end.

% gen_server

init([]) ->
    {ok, ServerCore} = c_server:start_link(),
    {ok, #state
        { port = gen_udp:open(?SERVER_PORT_NUMBER, [binary])
        , server_core = ServerCore
        }}.

handle_call(stop, _From, State) ->
    {stop, shutdown, State}.

handle_cast(_Req, _State) ->
    error(invalid_cast).

handle_info({udp, _Socket, _IP, _InPortNo, Packet}, #state{server_core = ServerCorePid} = State) ->
    case binary_to_term(Packet) of
        {get_pid, {ClientPid, Ref} } -> ClientPid ! {set_pid, ServerCorePid, Ref};
        _Unk -> error(bad_udp)
    end,
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
