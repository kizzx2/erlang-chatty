-module(c_server).
-behavior(gen_server).

-export([ code_change/3
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , init/1
        , terminate/2
        ]).

-export([ start_link/0
        , connect/2
        , disconnect/1
        , cmd/3
        , speak/2
        , change_name/2
        ]).

-record(state, {clients}).

% API

start_link() ->
    gen_server:start_link(?MODULE, [], []).

connect(ServerPid, ClientName) ->
    gen_server:call(ServerPid, {connect, ClientName}).

disconnect(ServerPid) ->
    gen_server:call(ServerPid, disconnect).

cmd(ServerPid, Cmd, Args) ->
    gen_server:call(ServerPid, {cmd, Cmd, Args}).

speak(ServerPid, Msg) ->
    gen_server:cast(ServerPid, {speak, self(), Msg}).

broadcast(ServerPid, Msg) ->
    gen_server:cast(ServerPid, {broadcast, Msg}).

change_name(ServerPid, NewName) ->
    gen_server:call(ServerPid, {change_name, NewName}).

% Controls

init([]) ->
    {ok, #state{clients = dict:new()}}.

% Client -> Server

handle_call({connect, ClientName}, {ClientPid, _Tag}, #state{clients = Clients} = State) ->
    io:format("~s has joined the room~n", [ClientName]),
    broadcast(self(), ClientName ++ " has joined the room"),
    {reply, ok, State#state{clients = dict:append(ClientPid, ClientName, Clients)}};

handle_call(disconnect, {ClientPid, _Tag}, #state{clients = Clients} = State) ->
    {ok, Name} = dict:find(ClientPid, Clients),
    io:format("~s has left the room~n", [Name]),
    {reply, ok, State#state{clients = dict:erase(ClientPid, Clients)}};

handle_call({change_name, NewName}, {ClientPid, _Tag}, #state{clients = Clients} = State) ->
    {ok, Name} = dict:find(ClientPid, Clients),
    io:format("~s changes name to ~s~n", [Name, NewName]),
    broadcast(self(), Name ++ " changes name to " ++ NewName),
    {reply, ok, State#state{clients = dict:store(ClientPid, NewName, Clients)}};

handle_call({cmd, Cmd, _Args}, {_ClientPid, _Tag}, State) ->
    Reply = case Cmd of
        _Unknown -> {error, unknown_command}
    end,
    {reply, Reply, State}.

handle_cast({broadcast, Msg}, #state{clients = Clients} = State) ->
    do_broadcast(Msg, Clients),
    {noreply, State};

handle_cast({speak, ClientPid, Msg}, #state{clients = Clients} = State) ->
    {ok, Name} = dict:find(ClientPid, Clients),
    io:format("~s: ~s~n", [Name, Msg]),
    Line = io_lib:format("~s: ~s", [Name, Msg]),
    do_broadcast(Line, Clients, [ClientPid]),
    {noreply, State}.

% Misc.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Internal

do_broadcast(Line, Clients) ->
    do_broadcast(Line, Clients, []).

do_broadcast(Line, Clients, Excludes) ->
    [ c_client:show(Client, Line)
        || Client <- lists:subtract(dict:fetch_keys(Clients), Excludes) ].
