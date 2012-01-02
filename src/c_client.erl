-module(c_client).
-behavior(gen_server).

-export([ code_change/3
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , init/1
        , terminate/2
        ]).

-export([ start_link/0
        , show/2
        , change_name/2
        , connect/2
        , disconnect/1
        , speak/2
        ]).

-record(state, {name, server}).

% API

start_link() ->
    gen_server:start_link(?MODULE, [], []).

show(ClientPid, Line) ->
    gen_server:cast(ClientPid, {show, Line}).

change_name(ClientPid, NewName) ->
    gen_server:call(ClientPid, {change_name, NewName}).

connect(ClientPid, ServerPid) ->
    gen_server:call(ClientPid, {connect, ServerPid}).

disconnect(ClientPid) ->
    gen_server:call(ClientPid, disconnect).

speak(ClientPid, Msg) ->
    gen_server:call(ClientPid, {speak, Msg}).

% Controls

init([]) -> {ok, #state{}}.

% User input -> Client

handle_call({connect, ServerPid}, _From, State) ->
    {reply,
        c_server:connect(ServerPid, "Anonymous"),
        State#state{server = ServerPid}};

handle_call({speak, Msg}, _From, #state{server = Server} = State) ->
    {reply,
        c_server:speak(Server, Msg),
        State};

handle_call({cmd, Cmd, Args}, _From, #state{server = Server} = State) ->
    {reply,
        c_server:cmd(Server, Cmd, Args),
        State};

handle_call({change_name, NewName}, _From, #state{server = Server} = State) ->
    {reply,
        c_server:change_name(Server, NewName), State};

handle_call(disconnect, _From, #state{server = Server} = State) ->
    c_server:disconnect(Server),
    {stop, shutdown, State}.

% Server -> Client

handle_cast({show, Line}, State) ->
    io:format("~s~n", [Line]),
    {noreply, State}.

% Misc.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

