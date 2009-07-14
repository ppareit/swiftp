-module(swiftp_proxy).
-behaviour(application).
-export([start/2, stop/1, client_session_create/0, device_session_create/0, 
         init/1, do_app_start/0]).
-import(log, [log/3]).

-define(DEVICE_PORT, 2222).
-define(CLIENT_PORT, 2121).
% TODO: find out what these mean and tweak them
-define(MAX_RESTART, 5).
-define(MAX_TIME, 60).

do_app_start() ->
    application:start(?MODULE).

% application behavior start callback
start(_Type, _Args) ->
    % Decide whether to start new DB cluster or join an existing one
    % We expect a -dbstart command line parameter. It can take the form:
    %   -dbstart master_new   (create a new database on this node)
    %   -dbstart master_old   (start a database using existing schema)
    %   -dbstart child 'somenode@host' (delete schema and join target node)
    io:format("In start()\n", []),
    case init:get_argument(dbstart) of
        {ok, [["master_new"]]} ->
            ok = db:create_master_schema(),
            log(info, "Master schema created, DB ready~n", []),
            supervisor:start_link({local, swiftp_proxy}, swiftp_proxy, [log_yes]);
        {ok, [["reuse"]]} ->
            ok = db:reuse_schema(),
            supervisor:start_link({local, swiftp_proxy}, swiftp_proxy, [log_yes]);
        {ok, ["child", MasterNode]} ->
            pong = net_adm:ping(MasterNode),
            global:sync(),   % sync global names so logging works
            db:join_db(MasterNode),
            log(info, "Connected to mnesia cluster ok, DB ready~n", []),
            supervisor:start_link({local, swiftp_proxy}, swiftp_proxy, [log_no]);
        Other ->
            io:format("Give -dbstart of master_new|reuse|child\n"),
            io:format("You gave dbstart of: ~p~n", [Other]),
            {error, bad_dbstart_type}
    end.

init([WhetherLog]) ->
    ChildrenExceptLogger = 
        [
            % The listener that SwiFTP Android devices will connect to
            {
                device_tcp_listener,   % Internal ID
                {tcp_listener, start_link, [?DEVICE_PORT, 
                                            device_session,
                                            fun ?MODULE:device_session_create/0]},
                permanent,
                2000,
                worker,
                [tcp_listener] 
            },
            % Supervisor for device session processes
            {
                device_session_sup,
                {supervisor, start_link, [{local, device_session_sup},
                                          device_session_sup,
                                          []]},
                permanent,
                infinity,
                supervisor,
                []
            },
            % The listener that FTP clients will connect to
            {
                client_tcp_listener,   % Internal ID
                {tcp_listener, start_link, [?CLIENT_PORT,
                                            client_session,
                                            fun ?MODULE:client_session_create/0]},
                permanent,
                2000,
                worker,
                [tcp_listener]
            },
            % Supervisor for client session processes
            {
                client_session_sup,
                {supervisor, start_link, [{local, client_session_sup},
                                          client_session_sup,
                                          []]},
                permanent,
                infinity,
                supervisor,
                []
            },
            % The local random number generator process
            {
                rand_worker,
                {rand, start_link, []},
                permanent,
                1000,
                worker,
                [rand]
            },
            % The local session registry process
            {
                session_registry,
                {session_registry, start_link, []},
                permanent,
                1000,
                worker,
                [session_registry]
            }
        ],
    Children = case WhetherLog of 
        log_no ->
            ChildrenExceptLogger;
        log_yes ->
            [log_child_spec() | ChildrenExceptLogger]
    end,
    {ok, {{one_for_one, ?MAX_RESTART, ?MAX_TIME}, Children}}.


%% Returns a valid child spec that could be given to a supervisor.
log_child_spec() ->
    {log,
     {log, start_link, ["./logs", 
                        "proxy.log", 
                        debug]},
     permanent,
     2000,
     worker,
     [log_sup]
    }.

stop(_State) ->
    log(info, "Application stop() called~n", []),
    log(info, "Stopping Mnesia...~n", []),
    mnesia:stop(),
    log(info, "Done stopping~n", []).


device_session_create() ->
    {ok, _Pid} = supervisor:start_child(device_session_sup, []).

client_session_create() ->
    {ok, _Pid} = supervisor:start_child(client_session_sup, []).

%% db_init_completed() ->
%%     process_flag(trap_exit, true),
%%     register(random_thread, spawn_link(rand, start, [])),
%%     _Registry = spawn_link(session_registry, start, []),
%%     _DeviceThreadSpawner = spawn_link(?MODULE, listener, [?DEVICE_PORT,
%%                                                     "SessionListener",
%%                                                     device_session,
%%                                                     start]),
%%     _ClientThreadSpawner = spawn_link(?MODULE, listener, [?CLIENT_PORT,
%%                                                     "MatcherListener",
%%                                                     connection_matcher,
%%                                                     start]),
%%     % Loop until we receive a quit request or exception/error occurs
%%     ReceiveLoop = fun(F) ->
%%         receive 
%%             {quit, Why} -> 
%%                 log(info, "Got quit request with: ~p~n", [Why]);
%%             X -> 
%%                 log(debug, "Main thread got message: ~p~n", [X]),
%%                 F(F)
%%         end
%%     end,
%%     ReceiveLoop(ReceiveLoop),
%%     log(info, "Server stopping.~n", []).

%% spawn_cluster_logger() ->
%%     spawn(log, start, ["/swiftp_proxy/logs", "proxy.log", debug, 10]).    

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


%% listener(Port, Name, Mod, Func) ->
%%     case util:tcp_listen(Port, [{active, false}, 
%%                                 binary, 
%%                                 {backlog, 10},
%%                                 {reuseaddr, true},
%%                                 inet % IPv4 only
%%                                ]) of 
%%         {ok, TcpListener} -> listener(Port, Name, Mod, Func, TcpListener);
%%         X -> log(error, "~p listener TCP listen error: ~p~n", [Name, X])
%%     end.
%% listener(Port, Name, Mod, Func, TcpListener) ->
%%     case gen_tcp:accept(TcpListener) of
%%         {ok, Socket} -> 
%%             Child = spawn(Mod, Func, [Socket]),
%%             gen_tcp:controlling_process(Socket, Child),
%%             listener(Port, Name, Mod, Func, TcpListener);
%%         X -> 
%%             log(error, "Accept error: ~p~n", [X]),
%%             gen_tcp:close(TcpListener)
%%     end.
