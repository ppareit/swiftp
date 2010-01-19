-module(client_session).
-include("records.hrl").
-behavior(gen_fsm).
-export([code_change/4, handle_event/3, handle_info/3, handle_sync_event/4, init/1,
         terminate/3]).
-import(json_eep, [term_to_json/1, json_to_term/1]).
-compile(export_all).   % Because exporting each state function is a pain.

-record(state, {devicesocket, clientsocket, listensocket, devicerow, authfails, 
                listenref, username, datafordevice}).

-define(MAXAUTHFAILS, 3).
-define(AUTH_TIMEOUT, 120000). % 2 minutes to login after connecting
-define(MAX_QUEUED_DATA, 1024).
-define(GENERAL_TIMEOUT, 30*60*1000). % Most states time out after 30 min

%TODO: Finish state machine (all states)
%TODO: add state timeouts

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
            username=none,
            datafordevice=none},
     ?AUTH_TIMEOUT}.

% Called by tcp_listener. After we have spawned a client session and are
% waiting in state fresh_start, the tcp_listener will call this function
% using the Pid of the spawned thread to give it the accepted socket.
set_socket(Pid, Socket) ->
    gen_fsm:send_event(Pid, {socket, Socket}).

% Called when the device connects to the listening socket that we set up.
handle_info({inet_async, ListenSocket, Ref, {ok, DeviceSocket}},
            StateName, StateData = #state{listenref=Ref, listensocket=ListenSocket} ) ->
    log(debug, "handle_info has devicesocket ~p~n", [DeviceSocket]),
    set_sockopt(ListenSocket, DeviceSocket),
    inet:setopts(DeviceSocket, [{active, once}]),
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

handle_info({inet_async, ListSock, Ref, {ok, DeviceSocket}}, StateName,
            StateData = #state{listenref=Ref}) ->
    ok = inet:setopts(DeviceSocket, [{active, once}, {packet, 0}, binary]),
    % The following line will return a {next_state, ..} tuple, which is what we want.
    NextStateTuple = ?MODULE:StateName({devicesocket, DeviceSocket}, StateData),
    gen_tcp:close(ListSock),  % We don't need the listener socket anymore
    NextStateTuple;
    

% If any open socket is closed, close all sockets and terminate this process
% It's unclear what the difference is between tcp_close and tcp_closed, we handle
% them identically.
handle_info({tcp_close, Socket}, StateName, StateData) -> 
    handle_info({tcp_closed, Socket}, StateName, StateData);
handle_info({tcp_closed, _Socket}, _StateName, StateData) ->
    log(info, "Socket closed, terminating~n", []),
    {stop, normal, StateData};
handle_info({tcp_error, _Socket, Reason}, _StateName, StateData) ->
    log(info, "TCP socket error, exiting normally: ~p~n", [Reason]),
    {stop, normal, StateData};
handle_info(Info, StateName, StateData) ->
    log(warn, "Unrecognized info in device_session: ~p~n", [Info]),
    {noreply, StateName, StateData}.

state_waiting_login({tcp, ClientSocket, Data}, State = #state{clientsocket = ClientSocket}) ->
    case Data of
         <<"USER ", PrefixBin:5/binary-unit:8, "_", Username/binary>> ->
            Prefix = binary_to_list(PrefixBin),
            log(debug, "Parsed user/prefix ~p/~p~n", [Username, Prefix]),
            case session_registry:lookup(Prefix) of
                CmdSessionPid when is_pid(CmdSessionPid) ->
                    % There's an open command session that matches that prefix. Open
                    % a listening socket and inform the command session that it's ready.
                    Opts = [binary, {packet, 0}, {reuseaddr, true},
                            {keepalive, true}, {backlog, 1}, {active, false}],
                    case gen_tcp:listen(0, Opts) of
                        {ok, ListenSocket} ->
                            % Start non-blocking accept
                            {ok, Ref} = prim_inet:async_accept(ListenSocket, -1),
                            {ok, ListenPort} = inet:port(ListenSocket),
                            gen_fsm:send_event(CmdSessionPid, {control_waiting, ListenPort}),
                            {next_state, 
                             state_listening, 
                             State#state{listenref=Ref, 
                                         listensocket=ListenSocket,
                                         username=Username},
                             ?GENERAL_TIMEOUT};
                        {error, Reason} ->
                            gen_tcp:send(ClientSocket, <<"500 Internal err in listener\r\n">>),
                            log(warn, "Client session listen fail: ~p~n", [Reason]),
                            {stop, listen_fail, State}
                    end;
                Other ->
                    log(warn, "Failed prefix lookup ~p: ~p~n", [Prefix, Other]),
                    Response = <<"530 Prefix not valid, make sure your Android device has signed in\r\n">>,
                    gen_tcp:send(ClientSocket, Response),
                    handle_auth_fail(State)
            end;
        % Some clients try to login as "anonymous" first, then ask the user for credentials
        % only if the anonymous login fails.
        <<"USER ", _/binary>> ->
            gen_tcp:send(ClientSocket, <<"331 Send password\r\n">>),
            log(info, "No prefix_ in username, waiting to reject password~n", []),
            {next_state, state_waiting_to_reject_pass, State, ?GENERAL_TIMEOUT};
        <<"QUIT", _/binary>> ->
            gen_tcp:send(ClientSocket, <<"221 Goodbye\r\n">>),
            log(info, "Client sent QUIT before logging in~n", []),
            {stop, normal, State};
        Other ->
            log(info, "Got something instead of client login: ~p~n", [Other]),
            gen_tcp:send(ClientSocket, <<"530 Login before using that command\r\n">>),
            {next_state, state_waiting_login, State, ?GENERAL_TIMEOUT}
    end;
state_waiting_login(timeout, State) ->
    {stop, timeout, State}.

login_fail_response() -> <<"530 Check login credentials. Include the prefix_string.\r\n">>.

% FTP servers shouldn't reject a login until after USER and PASS have both been received.
% If we're in this state, we already know that we've received USER but not yet PASS.
% So we're just waiting politely for the FTP client to send PASS so we can reject the
% login attempt with code 530. We halt if we get anything besides PASS.
state_waiting_to_reject_pass({tcp, ClientSocket, Data}, State) ->
    case Data of
        <<"PASS ", _/binary>> ->
            gen_tcp:send(ClientSocket, login_fail_response()),
            log(debug, "Rejected PASS as planned, waiting for actual login~n", []),
            {next_state, state_waiting_login, State, ?GENERAL_TIMEOUT};
        _ ->
            gen_tcp:send(ClientSocket, <<"530 Login with USER and PASS">>),
            log(info, "Waiting to reject PASS but got ~p~n", [Data]),
            {stop, normal, State}
    end.
                    
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
            {next_state, state_waiting_to_reject_pass, State, ?GENERAL_TIMEOUT}
    end.

% We've told the device that there's a control client waiting, and
% now we're waiting to receive a connection from it. We'll receive
% it indirectly via a {inet_async, ...} message to handle_info, which
% will get forwarded to here.
state_listening({devicesocket, DeviceSocket}, StateData) ->
    log(debug, "client_session received device socket~n", []),
    {next_state, state_device_connected, 
     StateData#state{devicesocket=DeviceSocket, listenref=none},
     ?GENERAL_TIMEOUT}.

state_device_connected({tcp, DeviceSocket, Data}, 
                       StateData = #state{devicesocket=DeviceSocket}) ->
    log(debug, "Got data from device in state_device_connected: ~p~n", [Data]),
    Json = json_to_term(binary_to_list(Data)),
    case util:get_json_value(Json, <<"action">>) of
        % Obsolete, there's no authentication anymore
        %<<"authenticate">> ->
        %    {WhetherAuth, Response, DeviceRow} = util:authenticate_json(Json),
        %    gen_tcp:send(DeviceSocket, Response),
        %    case WhetherAuth of
        %        true ->
        %            AndroidId = DeviceRow#device.android_id,
        %            log(info, "client_session device authed: ~p~n", [AndroidId]),
        %            % The client already sent us its username earlier, when it 
        %            % sent the prefix. Now we should forward it on to the device. 
        %            % It already has trailing \r\n.
        %            Username = StateData#state.username,
        %            UserString = <<"USER ">>,
        %            log(debug, "Username is ~p~n", [Username]),
        %            log(debug, "UserString is ~p~n", [UserString]),
        %            UserCmd = <<UserString/binary, Username/binary>>,
        %            log(debug, "2~n", []),
        %            log(debug, "Sending line: ~p to ~p~n", [UserCmd, DeviceSocket]),
        %            apply(gen_tcp, send, [DeviceSocket, UserCmd]),
        %            log(debug, "3~n", []),
        %            % If any data was queued up from the client to the device,
        %            % it should be sent now.
        %            case StateData#state.datafordevice of
        %                none -> ok;
        %                Data -> gen_tcp:send(DeviceSocket, Data)
        %            end,
        %            {next_state, state_proxying, 
        %             StateData#state{devicerow=DeviceRow,datafordevice=none}};
        %        false ->
        %            log(info, "client_session device failed auth~n", []),
        %            {stop, authenticate_failed, StateData}
        %    end;
        <<"login">> ->
            {Ok, Response, DeviceRow} = util:login_noauth_json(Json),
            gen_tcp:send(DeviceSocket, Response),
            case Ok of
                true ->
                    AndroidId = DeviceRow#device.android_id,
                    log(info, "client_session device authed: ~p~n", [AndroidId]),
                    % The client already sent us its username earlier, when it 
                    % sent the prefix. Now we should forward it on to the device. 
                    % It already has trailing \r\n.
                    Username = StateData#state.username,
                    UserString = <<"USER ">>,
                    %log(debug, "Username is ~p~n", [Username]),
                    %log(debug, "UserString is ~p~n", [UserString]),
                    UserCmd = <<UserString/binary, Username/binary>>,
                    %log(debug, "Sending line: ~p to ~p~n", [UserCmd, DeviceSocket]),
                    apply(gen_tcp, send, [DeviceSocket, UserCmd]),
                    % If any data was queued up from the client to the device,
                    % it should be sent now.
                    case StateData#state.datafordevice of
                        none -> ok;
                        Data -> gen_tcp:send(DeviceSocket, Data)
                    end,
                    {next_state, state_proxying, 
                     StateData#state{devicerow=DeviceRow,datafordevice=none},
                     ?GENERAL_TIMEOUT};
                false ->
                    log(warn, "Ill-formed login~n", []),
                    {stop, normal, StateData}
            end;
        OtherAction ->
            log(info, "Expected authentication but got action: ~p~n", [OtherAction]),
            {stop, no_authenticate, StateData}
    end;

% If we receive data from the client while both sockets are connected but the 
% device hasn't authenticated, we should queue up the data. We will send it
% after the device authenticates.
state_device_connected({tcp, ClientSocket, Data}, 
                       StateData = #state{clientsocket=ClientSocket}) ->
    log(debug, "Got data from client in state_device_connected~n", []),
    QueuedData = StateData#state.datafordevice,
    if 
        size(QueuedData) + size(Data) > ?MAX_QUEUED_DATA ->
            {stop, too_much_queued_data, StateData};
        true ->
            NextStateData = StateData#state{datafordevice= <<QueuedData, Data>>},
            {next_state, state_device_connected, NextStateData, ?GENERAL_TIMEOUT}
    end.
              

state_proxying({tcp, Socket, Data}, StateData = #state{devicesocket=DeviceSocket,
                                                       clientsocket=ClientSocket}) ->
    case Socket of
        DeviceSocket ->
            %log(debug, "Proxying device to client~n", []),
            gen_tcp:send(ClientSocket, Data);
        ClientSocket ->
            %log(debug, "Proxying client to device~n", []),
            gen_tcp:send(DeviceSocket, Data)
    end,
    {next_state, state_proxying, StateData, ?GENERAL_TIMEOUT}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData, ?GENERAL_TIMEOUT}.

handle_sync_event(_Event, _From, StateName, StateData) ->
    {next_state, StateName, StateData, ?GENERAL_TIMEOUT}.

terminate(_Reason, _StateName, _StateData) ->
    ok.

% Copied from tcp_listener to set internal tcp state correctly and copy socket options.
set_sockopt(ListSock, CliSocket) ->
    true = inet_db:register_socket(CliSocket, inet_tcp),
    case prim_inet:getopts(ListSock, [active, nodelay, keepalive, delay_send, priority, tos]) of
    {ok, Opts} ->
        case prim_inet:setopts(CliSocket, Opts) of
        ok    -> ok;
        Error -> gen_tcp:close(CliSocket), Error
        end;
    Error ->
        gen_tcp:close(CliSocket), Error
    end.

log(Level, Format, Args) ->
    log:log(Level, ?MODULE, Format, Args).
