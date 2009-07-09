-module(server).
-export([start/0, start/1, ping/0, listener/4, become_cluster_logger/0]).
-import(log, [log/3]).
-import(db, [create_master_schema/0, join_db/1]).

-define(DEVICE_PORT, 2222).
-define(CLIENT_PORT, 2221).

% Called when we are the first node of a cluster (create a new db schema)
start() ->
    become_cluster_logger(),  % The master starts out as cluster logger
    ok = create_master_schema(),
    log(info, "Master schema created, DB ready~n", []),
    db_init_completed(),
    init:stop().
    
% Called when we are to join an existing cluster. The argument comes from
% the command line (e.g. erl -s server start 'name@host')
start([ConnectTo]) ->
    case net_adm:ping(ConnectTo) of
        pang ->
            io:format("Couldn't ping master ~p~n", [ConnectTo]);
        pong ->
            global:sync(),   % sync global names so logging works
            join_db(ConnectTo),
            log(info, "Connected to mnesia cluster ok, DB ready~n", []),
            db_init_completed()
    end,
    init:stop().

db_init_completed() ->
    process_flag(trap_exit, true),
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
    ReceiveLoop = fun(F) ->
        receive 
            {quit, Why} -> 
                log(info, "Got quit request with: ~p~n", [Why]);
            X -> 
                log(debug, "Main thread got message: ~p~n", [X]),
                F(F)
        end
    end,
    ReceiveLoop(ReceiveLoop),
    log(info, "Server stopping.~n", []).

become_cluster_logger() ->
    spawn(log, start, ["/swiftp_proxy/logs", "proxy.log", debug, 10]).    

%% log_respawner(LogPid) ->
%%     process_flag(trap_exit, true),
%%     % This function will be called with 'none' if no logger exists yet
%%     case LogPid of
%%         none -> 
%%             spawn_link(log, start, ["/swiftp_proxy/logs", "proxy.log", debug, 10]);
%%         _ ->
%%             ok
%%     end,
%%     receive
%%         {'EXIT', LogPid, normal} ->
%%             io:format("Log respawner quitting, normal exit~n", []);
%%         {'EXIT', LogPid, Reason} ->
%%             io:format("Log thread died with reason ~p, respawning~n", [Reason]),
%%             log_respawner(none);
%%         Other ->
%%             io:format("Log respawner go unexpected message ~p~n", [Other]),
%%             log_respawner(LogPid)
%%     end.


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



