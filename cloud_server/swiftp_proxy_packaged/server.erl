-module(swiftp_proxy.server).
-export([start/0, start/1, ping/0, listener/4]).
-import(swiftp_proxy.log, [log/3]).
-import(swiftp_proxy.db, [create_master_schema/0, join_db/1]).

-define(DEVICE_PORT, 2222).
-define(CLIENT_PORT, 2221).

% Called when we are the first node of a cluster (create a new db schema)
start() ->
    case create_master_schema() of
        ok -> 
            log(info, "Master schema created~n", []),
            start_listeners();
        _X -> 
            log(error, "Master schema create failed, stopping.~n", [])
    end,
    init:stop().
    
% Called when we are to join an existing cluster. The argument comes from
% the command line (e.g. erl -s server start 'name@host' -detached)
start([ConnectTo]) ->
    case join_db(ConnectTo) of
        ok -> 
            log(info, "Connected to mnesia cluster ok", []),
            start_listeners();
        X ->
            log(error, "Mnesia join failed: ~p~n", [X])
    end,
    init:stop().

start_listeners() ->
    log(info, "Running!~n", []),
    process_flag(trap_exit, true),
    LogPid = spawn_link_logger(), 
    register(random_thread, spawn_link(rand, start, [])),
    _Registry = spawn_link(session_registry, start, []),
    _DeviceThreadSpawner = spawn_link(?MODULE, listener, [?DEVICE_PORT,
                                                    "SessionListener",
                                                    device_session,
                                                    start]),
    _ClientThreadSpawner = spawn_link(?MODULE, listener, [?CLIENT_PORT,
                                                    "MatcherListener",
                                                    connection_matcher,
                                                    start]),
    % Loop until we receive a quit request or exception/error occurs
    ReceiveLoop = fun(F, InLogPid) ->
        receive 
            {quit, Why} -> 
                log(info, "Got quit request with: ~p~n", [Why]);
            {'EXIT', LogPid, normal} ->
                io:format("Logger exited normally, not respawning~n", []),
                F(F, none);
            {'EXIT', LogPid, Reason} ->
                io:format("Logger exited, restarting. Reason: ~p~n", [Reason]),
                NewLogPid = spawn_link_logger(),
                F(F, NewLogPid);
            X -> 
                log(debug, "Main thread got message: ~p~n", [X]),
                F(F, InLogPid)
        end
    end,
    ReceiveLoop(ReceiveLoop, LogPid),
    log(info, "Server stopping.~n", []).

spawn_link_logger() ->
    spawn_link(log, start, ["/swiftp_proxy/logs", "proxy.log", debug, 10]).

listener(Port, Name, Mod, Func) ->
    case util:tcp_listen(Port) of 
        {ok, TcpListener} -> listener(Port, Name, Mod, Func, TcpListener);
        X -> log(error, "~p listener TCP listen error: ~p~n", [Name, X])
    end.
listener(Port, Name, Mod, Func, TcpListener) ->
    case gen_tcp:accept(TcpListener) of
        {ok, Socket} -> 
            Child = spawn(Mod, Func, [Socket]),
            gen_tcp:controlling_process(Socket, Child),
            listener(Port, Name, Mod, Func, TcpListener);
        X -> 
            log(error, "Accept error: ~p~n", [X]),
            gen_tcp:close(TcpListener)
    end.

% Called via RPC from remote nodes to test communication.
ping() ->
    ok.



