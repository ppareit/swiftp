-module(device_session).
-include("records.hrl").
-compile(export_all).  % Exporting each state function would be a real pain.
-export([code_change/4, handle_event/3, handle_info/3, handle_sync_event/4, init/1,
         terminate/3]).
-behavior(gen_fsm).
-import(log, [log/3]).
-import(rand, [random_alnum/1]).
-import(json_eep, [json_to_term/1, term_to_json/1]).

-record(state, {devicesocket, clientsocket, listenref, devicerow}).

-define(AUTH_TIMEOUT, 10000).

start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

init([]) ->
    {ok, state_fresh_start, []}.

state_fresh_start({socket, Socket}, _ ) ->
    % Receive TCP traffic as messages, unpackaged, in binary form
    ok = inet:setopts(Socket, [{active, once}, {packet, 0}, binary]),
    log(info, "New device socket received~n", []),
    {next_state, 
     state_have_socket, 
     #state{devicesocket=Socket, 
            clientsocket=none,
            devicerow=none,
            listenref=none},
     ?AUTH_TIMEOUT}.

% Called by tcp_listener. After we have spawned a device session and are
% waiting in state fresh_start, the tcp_listener will call this function
% using the Pid of the spawned thread to give it the accepted socket.
set_socket(Pid, Socket) ->
    gen_fsm:send_event(Pid, {socket, Socket}).

%% This state occurs after we have an open TCP socket to the device but it has not
%% yet authenticated.
state_have_socket({tcp, DeviceSocket, Data}, StateData = #state{devicesocket=DeviceSocket}) ->
    Json = json_to_term(binary_to_list(Data)),
    case util:get_json_value(Json, <<"action">>) of
        <<"authenticate">> ->
            {WhetherAuth, Response, DeviceRow} = util:authenticate_json(Json),
            gen_tcp:send(DeviceSocket, Response),
            case WhetherAuth of
                true ->
                    AndroidId = DeviceRow#device.android_id,
                    log(info, "Device authenticated: ~p~n", [AndroidId]),
                    {next_state, state_authenticated, StateData#state{devicerow=DeviceRow}};
                false ->
                    log(info, "Device failed authentication~n", []),
                    {stop, authenticate_failed, StateData}
            end;
        <<"create_account">> ->
            log(debug, "Creating account~n", []),
            {WhetherAuth, Response, DeviceRow} = util:create_account_json(Json),
            gen_tcp:send(DeviceSocket, Response),
            case WhetherAuth of
                true -> 
                    AndroidId = DeviceRow#device.android_id,
                    log(info, "Created account for: ~p~n", [AndroidId]),
                    {next_state, state_authenticated, StateData#state{devicerow=DeviceRow}};
                false -> 
                    log(warn, "Account create failed~n ", []),
                    {stop, account_create_failed, StateData}
            end;
        OtherAction ->
            log(info, "Expected authentication but got action: ~p~n", [OtherAction]),
            {stop, no_authenticate, StateData}
    end;
state_have_socket(timeout, State) ->
    log(info, "Timing out unauthenticated device session~n", []),
    {stop, timeout, State}.

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
%%     log(info, "Command thread started~n", []),
%%     gen_tcp:controlling_process(Socket, self()), % assign socket control to me
%%     Prefix = get_unused_prefix(),
%%     try auth_or_create_account(Socket) of
%%         false ->
%%             log(info, "Authenticate failed~n", []);
%%         true -> 
%%             log(info, "Authenticated successfully~n", []),
%%             % There is a little race condition here, between creating and registering
%%             % the new prefix, someone else may have registered it. Unlikely, though.
%%             case session_registry:add(Prefix, self()) of
%%                 success ->
%%                     main_loop(Socket, Prefix);
%%                 X ->
%%                     log(error, "Session add error: ~p~n", [X])
%%             end,
%%             main_loop(Socket, Prefix)
%%     %catch
%%     %    X : Y -> log(warn, "Exception in command session, ~p:~p~n", [X, Y])
%%     after
%%         gen_tcp:close(Socket),
%%         session_registry:remove(Prefix)
%%     end,
%%     log(info, "Command session thread exiting~n", []).
    
% We enter this loop after the device has authenticated or created an account
%% main_loop(Socket, Prefix) ->
%%     receive
%%         {tcp, Socket, Data} ->
%%             log(debug, "Json data to parser: ~p~n", [Data]),
%%             {JsonPropList} = json_to_term(binary_to_list(Data)),
%%             case proplists:get_value(<<"action">>, JsonPropList) of
%%                 <<"start_command_session">> ->
%%                     PrefixBin = list_to_binary(Prefix),
%%                     gen_tcp:send(Socket, 
%%                                  term_to_json({[{<<"prefix">>, PrefixBin}]})),
%%                     command_session(Socket);
%%                 <<"data_pasv_listen">> ->
%%                     data_pasv_listen(Socket);
%%                 <<"data_port_connect">> ->
%%                     data_port_connect(Socket, JsonPropList)
%%             end;
%%         {tcp_closed, Socket} ->
%%             ok
%%         %{clientsocket, ClientSocket, Username} -> 
%%         %    gen_tcp:controlling_process(ClientSocket, self()),
%%         %    gen_tcp:send(DeviceSocket, "USER " ++ Username),
%%         %    case proxy_loop(DeviceSocket, ClientSocket) of
%%         %        quit ->
%%         %            gen_tcp:close(DeviceSocket);
%%         %        dont_quit -> 
%%         %            wait_loop(DeviceSocket)
%%         %    end
%%     end.

% Can transition to states state_cmd_session, state_pasv_listen
state_authenticated({tcp, DeviceSocket, Data}, StateData= #state{devicesocket=DeviceSocket}) ->
    JsonTerm = json_to_term(binary_to_list(Data)),
    case util:get_json_value(JsonTerm, <<"action">>) of
        <<"start_command_session">> ->
            log(debug, "Starting command session~n", []),
            {next_state, state_cmd_session, StateData};
        <<"data_pasv_listen">> ->
            log(debug, "Starting pasv listen~n", []),
            process_flag(trap_exit, true),
            Opts = [binary, {packet, 0}, {reuseaddr, true},
                    {keepalive, true}, {backlog, 30}, {active, false}],
            case gen_tcp:listen(0, Opts) of
                {ok, ListenSocket} ->
                    % Send JSON with port number on successful listen
                    {ok, Port} = inet:port(ListenSocket),
                    ResponseObj = term_to_json({[{<<"port">>, Port}]}),
                    gen_tcp:send(DeviceSocket, ResponseObj),
                    % In StateData, the clientsocket is the listener
                    {next_state, state_pasv_listen, 
                        StateData#state{clientsocket=ListenSocket}};
                {error, Reason} ->
                    % Send error object if listen fails
                    log(warn, "device_session pasv_listen failed~n", []),
                    ResponseTerm = {[{<<"error_code">>, 0},
                                     {<<"error_string">>, <<"Listen failed">>}]},
                    ResponseObj = term_to_json(ResponseTerm),
                    gen_tcp:send(DeviceSocket, ResponseObj),
                    {stop, Reason, StateData}
            end;
        <<"data_port_connect">> ->
            Address = util:get_json_value(JsonTerm, <<"address">>),
            Port = util:get_json_value(JsonTerm, <<"port">>),
            case util:open_data_port(Address, Port) of
                {ok, ClientSocket} ->
                    gen_tcp:send(DeviceSocket, term_to_json({[]})),
                    {next_state, state_proxying, 
                        StateData#state{clientsocket=ClientSocket}};
                _ ->
                    log(info, "Failed connecting port to ~p:~p~n", [Address,
                                                                    Port]),
                    ResponseTerm = {[{<<"error_code">>, 0},
                                     {<<"error_string">>, <<"Connect failed">>}]},
                    gen_tcp:send(DeviceSocket, term_to_json(ResponseTerm)),
                    {stop, data_port_connect_failed, StateData}
            end
    end.

state_pasv_listen({tcp, DeviceSocket, Data}, 
                  StateData = #state{devicesocket=DeviceSocket,
                                     clientsocket=ListenSocket}) ->
    JsonTerm = json_to_term(Data),
    case util:get_json_value(JsonTerm, <<"action">>) of
        <<"data_pasv_accept">> ->
            case gen_tcp:accept(ListenSocket, 10000) of  % timeout 10 sec
                {ok, DeviceSocket} ->
                    NewStateData = StateData#state{devicesocket=DeviceSocket},
                    log(debug, "Device session accepted client socket ok~n", []),
                    gen_tcp:send(DeviceSocket, term_to_json({[]})),
                    {next_state, state_proxying, NewStateData};
                {error, Reason} ->
                    log(warn, "Device session pasv accept failed: ~p~n", [Reason]),
                    {stop, failed_pasv_accept, StateData}
            end;
        X ->
            log(warn, "Expected pasv_accept but got: ~p~n", [X])
    end.

state_cmd_session({tcp, DeviceSocket, _Data}, 
                  StateData = #state{devicesocket=DeviceSocket}) ->
    % This is noop for now. Will be implemented later.
    ResponseTerm = {[{<<"error_code">>, 0}, {<<"Command session not implemented">>}]},
    ResponseJson = term_to_json(ResponseTerm),
    gen_tcp:send(DeviceSocket, ResponseJson),
    {nextstate, state_cmd_session, StateData}.

state_proxying({tcp, Socket, Data}, StateData = #state{devicesocket=DeviceSocket,
                                                       clientsocket=ClientSocket}) ->
    case Socket of
        DeviceSocket ->
            gen_tcp:send(ClientSocket, Data);
        ClientSocket ->
            gen_tcp:send(DeviceSocket, Data)
    end,
    {next_state, state_proxying, StateData}.

%% command_session(Socket) ->
%%     receive
%%         {tcp, Socket, Data} ->
%%             log(info, "Received unexpected data in command session: ~p~n", [Data]),
%%             command_session(Socket);
%%         {tcp_closed, Socket} ->
%%             log(info, "Command session closed~n", []);
%%         {control_connection_waiting, Port} ->
%%             PortBinary = list_to_binary(integer_to_list(Port)),
%%             Json = term_to_json({[{<<"action">>, <<"control_connection_waiting">>},
%%                                   {<<"port">>, PortBinary}]}),
%%             gen_tcp:send(Socket, Json),
%%             log(debug, "Told client about waiting control connection on ~p~n", [Port]),
%%             command_session(Socket)
%%     end.

%% data_pasv_listen(Socket) ->
%%     case util:tcp_listen(0) of
%%         {error, Reason} ->
%%             log(warn, "Couldn't open pasv socket: ~p~n", [Reason]),
%%             Json = term_to_json({[{<<"error_code">>, 0},
%%                                   {<<"error_string">>, <<"Couldn't open port">>}]}),
%%             gen_tcp:send(Socket, Json);
%%         {ok, NewSocket} ->
%%             {ok, Port} = inet:port(NewSocket),
%%             PortBinary = list_to_binary(integer_to_list(Port)),
%%             log(debug, "Opened pasv socket on port: ~p~n", [Port]),
%%             Json = term_to_json({[{<<"port">>, PortBinary}]}),
%%             gen_tcp:send(Socket, Json),
%%             wait_for_pasv_accept(Socket, NewSocket),
%%             % When we return here, the proxying is over, and it's time to clean up
%%             gen_tcp:close(NewSocket)
%%     end.
        
%% wait_for_pasv_accept(DeviceSocket, ListenSocket) ->
%%     receive
%%         {tcp, DeviceSocket, Data} ->
%%             {JsonPropList} = json_to_term(Data),
%%             case proplists:get_value(<<"action">>, JsonPropList) of
%%                 <<"data_pasv_accept">> ->
%%                     case gen_tcp:accept(ListenSocket) of
%%                         {ok, ClientSocket} ->
%%                             log(info, "pasv accept OK, proxying~n", []),
%%                             util:proxy_until_close(ClientSocket, DeviceSocket, 30000);
%%                         X ->
%%                             log(warn, "pasv accept error: ~p~n", [X])
%%                     end;
%%                 Other ->
%%                     log(error, "Got unexpected request waiting for pasv_accept: ~p~n", 
%%                         [Other])
%%             end
%%     after 120000 ->
%%         % Require data_pasv_accept within 2 minutes of data_pasv_listen 
%%         log(warn, "Timed out between data_pasv_listen and data_pasv_accept~n", [])
%%     end.
                    
%% data_port_connect(DeviceSocket, JsonPropList) ->
%%     ErrorJson = term_to_json({[{<<"error_code">>, 9},
%%                                {<<"error_string">>, <<"Missing argument">>}]}),
%%     case proplists:get_value(<<"address">>, JsonPropList) of
%%         undefined ->
%%             gen_tcp:send(DeviceSocket, ErrorJson);
%%         Address ->
%%             case proplists:get_value(<<"port">>, JsonPropList) of
%%                 undefined ->
%%                     gen_tcp:send(DeviceSocket, ErrorJson);
%%                 Port ->
%%                     log(debug, "Opening outbound socket to ~p:~p~n", [Address, Port]),
%%                     log(warn, "Not really~n", [])
%%             end
%%     end.

% This is the loop we're in when both connections have been established 
%% proxy_loop(DevSocket, CliSocket) -> 
%%     receive
%%         {tcp, DevSocket, Data} -> 
%%             gen_tcp:send(CliSocket, Data);
%%         {tcp, CliSocket, Data} ->
%%             gen_tcp:send(DevSocket, Data);
%%         {tcp_closed, Socket} -> 
%%             gen_tcp:close(CliSocket),
%%             case Socket of 
%%                 DevSocket -> 
%%                     log(info, "Device socket closed~n", []),
%%                     quit;
%%                 CliSocket -> 
%%                     log(info, "Client socket closed~n", []),
%%                     dont_quit
%%             end;
%%         _ -> 
%%             ignore
%%      end.
    

%% % Repeated generate prefix strings until we find an unused one
%% get_unused_prefix() -> get_unused_prefix(50). % Try max 50 times
%% -spec get_unused_prefix(integer()) -> string().
%% get_unused_prefix(0) -> throw("Could not find unused prefix");
%% get_unused_prefix(TriesLeft) ->
%%     Candidate = random_alnum(6),
%%     case session_registry:lookup(Candidate) of
%%         undefined -> Candidate;
%%         _ -> get_unused_prefix(TriesLeft-1)
%%     end.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
    {next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
    ok.
