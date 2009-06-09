-module(server).
-export([start/0, listener/4]).
-import(log, [log/2, log/3]).

-define(DEVICE_PORT, 2222).
-define(CLIENT_PORT, 2221).

start() ->
    log(info, "Running!~n"),
    process_flag(trap_exit, true),
    register(random_thread, spawn_link(rand, start, [])),
    _Registry = spawn_link(session_registry, start, []),
    _SessionSpawner = spawn_link(?MODULE, listener, [?DEVICE_PORT,
                                                    "SessionListener",
                                                    session,
                                                    start]),
    _MatcherSpawner = spawn_link(?MODULE, listener, [?CLIENT_PORT,
                                                    "MatcherListener",
                                                    connection_matcher,
                                                    start]),
    % Loop until we receive a quit request or exception/error occurs
    ReceiveLoop = fun(F) ->
        receive 
            {quit, Why} -> 
                log("Got quit request with: ~p~n", [Why]);
            X -> 
                log("Main thread got message: ~p~n", [X]),
                F()
        end
    end,
    ReceiveLoop(ReceiveLoop),
    log(info, "Server stopping.~n").

listener(Port, Name, Mod, Func) ->
    case tcp_listen(Port) of 
        {ok, TcpListener} -> listener(Port, Name, Mod, Func, TcpListener);
        X -> log("~p listener TCP listen error: ~p~n", [Name, X])
    end.
listener(Port, Name, Mod, Func, TcpListener) ->
    case gen_tcp:accept(TcpListener) of
        {ok, Socket} -> 
            Child = spawn(Mod, Func, [Socket]),
            gen_tcp:controlling_process(Socket, Child),
            listener(Port, Name, Mod, Func, TcpListener);
        X -> 
            log("Accept error: ~p~n", [X]),
            gen_tcp:close(TcpListener)
    end.

tcp_listen(Port) -> 
    gen_tcp:listen(Port, [binary, %{packet, 4}, 
                                   {reuseaddr, true}, 
                                   {active, true}]).



