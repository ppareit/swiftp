-module(swiftp_proxy).
-behaviour(application).
-export([start/2, stop/1, client_session_create/0, device_session_create/0, 
         init/1, do_app_start/0]).

-define(DEVICE_PORT, 2222).
-define(CLIENT_PORT, 2121).

% TODO: find out what these mean and tweak them
-define(MAX_RESTART, 5).
-define(MAX_TIME, 60).

do_app_start() ->
    io:format("Calling application:start()~n", []),
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
        {ok, [["child", MasterNode]]} ->
            MasterNodeAtom = list_to_atom(MasterNode),
            pong = net_adm:ping(MasterNodeAtom),
            global:sync(),   % sync global names so logging works
            db:join_db(MasterNodeAtom),
            log(info, "Connected to mnesia cluster ok, DB ready~n", []),
            supervisor:start_link({local, swiftp_proxy}, swiftp_proxy, [log_no]);
        Other ->
            io:format("Give one of -dbstart master_new|reuse|child\n"),
            io:format("You gave dbstart of: ~p~n", [Other]),
            {error, bad_dbstart_type}
    end.

init([WhetherLog]) ->
    ChildrenExceptLogger = 
        [
            % The local session registry process
            {
                session_registry,
                {session_registry, start_link, []},
                permanent,
                1000,
                worker,
                [session_registry]
            },
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
            }
        ],
    Children = case WhetherLog of 
        log_no ->
            ChildrenExceptLogger;
        log_yes ->
            [log_child_spec() | ChildrenExceptLogger]
    end,
%%     yaws_wrapper:start(),
    
    %%     log(info, "Starting httpd~n", []),
%%     ok = inets:start(),
%%     {ok, _Pid} = inets:start(httpd, [{port, 4443}, 
%%                                      {modules, [mod_alias, 
%%                                                 mod_auth, 
%%                                                 mod_esi, 
%%                                                 mod_actions, 
%%                                                 mod_cgi, 
%%                                                 mod_dir, 
%%                                                 mod_get, 
%%                                                 mod_head, 
%%                                                 mod_log, 
%%                                                 mod_disk_log]},
%%                                      {server_root, "httpd_server_root"}, 
%%                                      {document_root, "httpd_doc_root"},
%%                                      {ssl_certificate_file, "ssl/www_swiftp_org.crt"},
%%                                      {ssl_ca_certificate_file, "ssl/www_swiftp_org.ca-bundle"},
%%                                      {server_name, "testserver"}, 
%%                                      {socket_type, ssl},
%%                                      {erl_script_alias, {"/web_int", [web_int]}},
%%                                      {error_log, "error.log"},
%%                                      {security_log, "security.log"},
%%                                      {transfer_log, "transfer.log"} ]),
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
    log(info, "Application stop() called~n", []).

device_session_create() ->
    {ok, _Pid} = supervisor:start_child(device_session_sup, []).

client_session_create() ->
    {ok, _Pid} = supervisor:start_child(client_session_sup, []).

log(Level, Format, Args) ->
    log:log(Level, ?MODULE, Format, Args).

