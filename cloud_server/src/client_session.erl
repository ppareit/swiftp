-module(client_session).
-behavior(gen_fsm).
-export([code_change/4, handle_event/3, handle_info/3, handle_sync_event/4, init/1,
         terminate/3]).
-import(log, [log/2, log/3]).
-import(json_eep, [term_to_json/1, json_to_term/1]).
-compile(export_all).   % Because exporting each state function is a pain.

-record(state, {devicesocket, clientsocket, devicerow, authfails, listenref, username}).

-define(MAXAUTHFAILS, 3).
-define(AUTH_TIMEOUT, 10000).

%TODO: Finish state machine (all states)
%TODO: add state timeouts

% TODO: clean this up (remove it?)
% The job of a "matcher" is to: 
% 1. take an incoming connection from an FTP client
% 2. Pretend to be an FTP server
% 3. When the client sends "USER prefix_username", take the prefix and resolve
%    it to an existing PID
% 4. Pass the client socket along with the username to the PID, which will
%    proxy between the socket representing the FTP client and the socket
%    representing the device running SwiFTP server.

start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

init([]) ->
    {ok, state_fresh_start, []}.

state_fresh_start({socket, ClientSocket}, _ ) ->
    % Receive TCP traffic as messages, unpackaged, in binary form
    ok = inet:setopts(ClientSocket, [{active, once}, {packet, 0}, binary]),
    ok = gen_tcp:send(ClientSocket, <<"220 SwiFTP cloud proxy ready\r\n">>),
    {next_state, 
     state_waiting_login, 
     #state{devicesocket=none, 
            clientsocket=ClientSocket,
            devicerow=none,
            authfails=0,
            listenref=none,
            username=none},
     ?AUTH_TIMEOUT}.

% Called by tcp_listener. After we have spawned a client session and are
% waiting in state fresh_start, the tcp_listener will call this function
% using the Pid of the spawned thread to give it the accepted socket.
set_socket(Pid, Socket) ->
    gen_fsm:send_event(Pid, {socket, Socket}).

% Called when the device connects to the listening socket that we set up.
handle_info({inet_async, ListenSocket, Ref, {ok, DeviceSocket}},
            StateName, StateData = #state{listenref=Ref} ) ->
    Opts = [binary, {active, once}, {packet, 0}],
    ok = inet:setopts(DeviceSocket, Opts),
    gen_tcp:close(ListenSocket),
    ?MODULE:StateName({devicesocket, DeviceSocket}, StateData);

% Take TCP data as an Erlang message and dispatch it to the current state function                   
handle_info({tcp, Socket, Data}, StateName, StateData) ->
    % Now that we've received the latest TCP message, enable receipt
    % of the next message. (It's actually received by the gen_fsm and
    % forwarded to this function.
    inet:setopts(Socket, [{active, once}]),
    % The following call will return a {next_state, ...} tuple as required.
    ?MODULE:StateName({tcp, Socket, Data}, StateData);

% If any open socket is closed, close all sockets and terminate this process
handle_info({tcp_closed, Socket}, 
            _StateName,
            #state{clientsocket=ClientSocket,
                   devicesocket=DeviceSocket} = StateData) ->
    case Socket of
        ClientSocket -> log(info, "Client socket closed, terminating~n", []);
        DeviceSocket -> log(info, "Device socket closed, terminating~n", [])
    end,
    gen_tcp:close(ClientSocket),
    gen_tcp:close(DeviceSocket),
    {stop, normal, StateData};
handle_info(Info, StateName, StateData) ->
    log(warn, "Unrecognized info in device_session: ~p~n", [Info]),
    {noreply, StateName, StateData}.

%% start(Socket) ->
%%     gen_tcp:controlling_process(Socket, self()),
%%     gen_tcp:send(Socket, <<"220 SwiFTP cloud proxy ready\r\n">>),
%%     try get_parse_login(Socket) of
%%         {Prefix, Username} ->
%%             case session_registry:lookup(Prefix) of
%%                 Pid when is_pid(Pid) ->
%%                     log(debug, "Looked up ~p: ~p~n", [Prefix, Pid]),
%%                     Pid ! {clientsocket, Socket, binary_to_list(Username)},
%%                     gen_tcp:controlling_process(Socket, Pid);
%%                 Other ->
%%                     log(info, "Prefix pid lookup: ~p, err: ~p~n", [Prefix, Other]),
%%                     gen_tcp:send(Socket, <<"530 Bad credentials, check prefix ",
%%                                            "and username\r\n">>),
%%                     gen_tcp:close(Socket)
%% 
%%             end;
%%         _ -> log(info, "Client closed without ever sending valid USER~n", [])
%%     catch
%%         X : Y -> 
%%             log("Matcher exception ~p:~p~n", [X, Y]),
%%             gen_tcp:close(Socket)
%%     end,
%%     log(info, "Matcher exiting~n", []).

%% get_parse_login(Socket) ->
%%     receive
%%         %{tcp, Socket, <<"USER ", P1, P2, P3, P4, P5, P6, "_", Username/binary>> } ->
%%         {tcp, Socket, <<"USER ", PrefixBin:6/binary-unit:8, "_", Username/binary>>} ->
%%             Prefix = binary_to_list(PrefixBin),
%%             log(debug, "Matched with ~p/~p~n", [Prefix, Username]),
%%             {Prefix, Username};
%%         {tcp_closed, Socket} ->
%%             tcp_closed;
%%         Data ->
%%             log(debug, "Didn't match~n", []),
%%             case Data of
%%                 <<"USER ", _/binary>> ->
%%                     gen_tcp:send(Socket, <<"530 Check your username prefix\r\n">>);
%%                 _ ->                                         
%%                     gen_tcp:send(Socket, <<"500 Login with USER first\r\n">>)
%%             end,
%%             get_parse_login(Socket)
%%     end.

state_waiting_login({tcp, ClientSocket, Data}, State = #state{clientsocket = ClientSocket}) ->
    FailResponse = <<"530 Check your login credentials\r\n">>,
    case Data of
         <<"USER ", PrefixBin:6/binary-unit:8, "_", Username/binary>> ->
            Prefix = binary_to_list(PrefixBin),
            log(debug, "Parsed user/prefix ~p/~p~n", [Username, Prefix]),
            case session_registry:lookup(Prefix) of
                CmdSessionPid when is_pid(CmdSessionPid) ->
                    % There's an open command session that matches that prefix. Open
                    % a listening socket and inform the command session that it's ready.
                    Opts = [binary, {packet, 0}, {reuseaddr, true},
                            {keepalive, true}, {backlog, 30}, {active, false}],
                    case gen_tcp:listen(0, Opts) of
                        {ok, ListenSocket} ->
                            % Start non-blocking accept
                            {ok, Ref} = prim_inet:async_accept(ListenSocket, -1),
                            ListenPort = inet:port(ListenSocket),
                            gen_fsm:send_event(CmdSessionPid, {control_waiting, ListenPort}),
                            {next_state, 
                             state_listening, 
                             State#state{listenref=Ref, username=Username}};
                        {error, Reason} ->
                            gen_tcp:send(ClientSocket, <<"500 Internal err in listener\r\n">>),
                            log(warn, "Client session listen fail: ~p~n", [Reason]),
                            {stop, listen_fail, State}
                    end;
                _ ->
                    log(info, "Failed user/prefix lookup ~p/~p~n", [Username, Prefix]),
                    gen_tcp:send(ClientSocket, FailResponse),
                    handle_auth_fail(State)
            end;
        Other ->
            log(info, "Got something instead of client login: ~p~n", [Other]),
            gen_tcp:send(ClientSocket, FailResponse),
            handle_auth_fail(State)
    end;
state_waiting_login(timeout, State) ->
    {stop, timeout, State}.
                    
%% @spec handle_auth_fail(State) -> {next_state, state_waiting_login, NewState} 
%%                                | {stop, too_many_auth_fails, NewState}
%% The return value is suitable to be used as a gen_fsm event return value.
%% Should be called when either
%%   1. The client supplied a bad user or prefix value
%%   2. The client sent something else when a login was expected
handle_auth_fail(State) ->
    AuthFails = State#state.authfails + 1,
    NewState = State#state{authfails=AuthFails},
    if
        AuthFails >= ?MAXAUTHFAILS -> 
            {stop, too_many_auth_fails, NewState};
        true ->
            {next_state, state_waiting_login, State}
    end.

% We've told the device that there's a control client waiting, and
% now we're waiting to receive a connection from it. We'll receive
% it indirectly via a {inet_async, ...} message to handle_info, which
% will get forwarded to here.
state_listening({devicesocket, DeviceSocket}, StateData) ->
    log(debug, "Received device socket~n", []),
    % The client already sent us its username earlier, when it sent the prefix.
    % Now we should forward it on to the device. It already has trailing \n.
    Username = StateData#state.username,
    UserCmd = <<"USER ", Username>>,
    gen_tcp:send(DeviceSocket, UserCmd),
    {next_state, state_device_connected, 
        StateData#state{devicesocket=DeviceSocket, listenref=none}}.

%% % The device has connected but not yet authenticated.
%% state_device_connected({tcp, DeviceSocket, Data}, StateData) ->
%%     JsonTerm = json_to_term(Data),
%%     case util:get_json_value(JsonTerm, <<"action">>) of
%%         <<"authenticate">> ->
%%             {WhetherAuth, Response, DeviceRow} = util:authenticate_json(JsonTerm),
%%             gen_tcp:send(DeviceSocket, Response),
%%             case WhetherAuth of
%%                 true ->
%%                     log(debug, "Device authenticated~n", []),
%%                     NewStateData = StateData#state{devicerow=DeviceRow},
%%                     {next_state, state_proxying, NewStateData};
%%                 false ->
%%                     log(warn, "Device failed auth: ~n", []),
%%                     {stop, failed_device_auth, StateData}
%%             end;
%%         X ->
%%             log(warn, "Device sent something other than auth: ~p~n,", [X]),
%%             {stop, failed_device_auth, StateData}
%%     end.
       
state_proxying({tcp, Socket, Data}, StateData = #state{devicesocket=DeviceSocket,
                                                       clientsocket=ClientSocket}) ->
    case Socket of
        DeviceSocket ->
            gen_tcp:send(ClientSocket, Data);
        ClientSocket ->
            gen_tcp:send(DeviceSocket, Data)
    end,
    {next_state, state_proxying, StateData}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
    {next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
    ok.

