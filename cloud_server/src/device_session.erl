-module(device_session).
-include("records.hrl").
-compile(export_all).  % Exporting each state function would be a real pain.
-export([code_change/4, handle_event/3, handle_info/3, handle_sync_event/4, init/1,
         terminate/3]).
-behavior(gen_fsm).
-import(rand, [random_alnum/1]).
-import(json_eep, [json_to_term/1, term_to_json/1]).

-record(state, {devicesocket, clientsocket, listenref, devicerow, sessionbytes}).

-define(AUTH_TIMEOUT, 10000).
-define(GENERAL_TIMEOUT, 30*60*1000). % Most states timeout after 30 minutes
start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),  % we need terminate() to be called on exit
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
            listenref=none,
            sessionbytes=0},
     ?AUTH_TIMEOUT}.

% Called by tcp_listener. After we have spawned a device session and are
% waiting in state fresh_start, the tcp_listener will call this function
% using the Pid of the spawned thread to give it the accepted socket.
set_socket(Pid, Socket) ->
    gen_fsm:send_event(Pid, {socket, Socket}).

state_have_socket({tcp, DeviceSocket, Data}, StateData = #state{devicesocket=DeviceSocket}) ->
    Json = json_to_term(binary_to_list(Data)),
    case util:get_json_value(Json, <<"action">>) of
        <<"login">> ->
            {Ok, Response, DeviceRow} = util:login_noauth_json(Json),
            gen_tcp:send(DeviceSocket, Response),
            case Ok of
                true ->
                    {next_state, state_authenticated, StateData#state{devicerow=DeviceRow},
                     ?GENERAL_TIMEOUT};
                false ->
                    log(warn, "Ill-formed login~n", []),
                    {stop, normal, StateData, ?GENERAL_TIMEOUT}
            end;
%        <<"authenticate">> ->
%            {WhetherAuth, Response, DeviceRow} = util:authenticate_json(Json),
%            gen_tcp:send(DeviceSocket, Response),
%            case WhetherAuth of
%                true ->
%                    AndroidId = DeviceRow#device.android_id,
%                    BytesUsed = DeviceRow#device.totalbytes,
%                    log(info, "Device authenticated: ~p~n", [AndroidId]),
%                    {next_state, state_authenticated, StateData#state{devicerow=DeviceRow}};
%                false ->
%                    log(info, "Device failed authentication~n", []),
%                    {stop, normal, StateData}
%            end;
%        <<"create_account">> ->
%            log(debug, "Creating account~n", []),
%            {WhetherAuth, Response, DeviceRow} = util:create_account_json(Json),
%            gen_tcp:send(DeviceSocket, Response),
%            case WhetherAuth of
%                true -> 
%                    AndroidId = DeviceRow#device.android_id,
%                    log(info, "Created account for: ~p~n", [AndroidId]),
%                    {next_state, state_authenticated, StateData#state{devicerow=DeviceRow}};
%                false -> 
%                    log(warn, "Account create failed~n ", []),
%                    {stop, normal, StateData}
%            end;
        OtherAction ->
            %log(info, "Expected login but got action: ~p~n", [OtherAction]),
            {stop, normal, StateData}
    end;
state_have_socket(timeout, State) ->
    log(info, "Timing out unauthenticated device session~n", []),
    {stop, normal, State}.

% Take TCP data as an Erlang message and dispatch it to the current state function                   
handle_info({tcp, Socket, Data}, StateName, StateData) ->
    % Now that we've received the latest TCP message, enable receipt
    % of the next message. (It's actually received by the gen_fsm and
    % forwarded to this function.
    inet:setopts(Socket, [{active, once}]),
    % The following call will return a {next_state, ...} tuple as required.
    ?MODULE:StateName({tcp, Socket, Data}, StateData);
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


state_authenticated({tcp, DeviceSocket, Data}, StateData= #state{devicesocket=DeviceSocket}) ->
    JsonTerm = json_to_term(binary_to_list(Data)),
    AndroidId = (StateData#state.devicerow)#device.android_id,
    case util:get_json_value(JsonTerm, <<"action">>) of
        <<"start_command_session">> ->
            log(debug, "Starting command session~n", []),
            % The response to start_command_session contains the session prefix
            DeviceRow = StateData#state.devicerow,
            Prefix = DeviceRow#device.prefix,
            PrefixBin = list_to_binary(Prefix),
            ResponseTerm = {[{<<"prefix">>, PrefixBin}]},
            gen_tcp:send(DeviceSocket, term_to_json(ResponseTerm)),
            session_registry:add(Prefix, self()),
            do_queued_actions(DeviceSocket, DeviceRow),
            {next_state, state_cmd_session, StateData, ?GENERAL_TIMEOUT};
        <<"data_pasv_listen">> ->
            log(debug, "Starting pasv listen~n", []),
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
                     StateData#state{clientsocket=ListenSocket},
                     ?GENERAL_TIMEOUT};
                {error, Reason} ->
                    % Send error object if listen fails
                    log(warn, "device_session pasv_listen failed~n", []),
                    ResponseJson = util:json_err_obj(0, "Listen failed"),
                    gen_tcp:send(DeviceSocket, ResponseJson),
                    {stop, Reason, StateData}
            end;
        <<"data_port_connect">> ->
            Address = binary_to_list(util:get_json_value(JsonTerm, <<"address">>)),
            Port = util:get_json_value(JsonTerm, <<"port">>),
            log(debug, "Connecting PORT to ~p:~p~n", [Address, Port]),
            case util:open_data_port(Address, Port) of
                {ok, ClientSocket} ->
                    gen_tcp:send(DeviceSocket, term_to_json({[]})),
                    {next_state, state_proxying, 
                     StateData#state{clientsocket=ClientSocket},
                     ?GENERAL_TIMEOUT};
                _ ->
                    log(info, "Failed connecting port to ~p:~p~n", [Address, Port]),
                    ResponseJson = util:json_err_obj(14, "Connect failed"),
                    gen_tcp:send(DeviceSocket, ResponseJson),
                    {stop, data_port_connect_failed, StateData}
            end
    end.

state_pasv_listen({tcp, DeviceSocket, Data}, 
                  StateData = #state{devicesocket=DeviceSocket,
                                     clientsocket=ListenSocket}) ->
    JsonTerm = json_to_term(binary_to_list(Data)),
    case util:get_json_value(JsonTerm, <<"action">>) of
        <<"data_pasv_accept">> ->
            case gen_tcp:accept(ListenSocket, 10000) of  % timeout 10 sec
                {ok, ClientSocket} ->
                    NewStateData = StateData#state{clientsocket=ClientSocket},
                    log(debug, "Device session accepted client socket ok~n", []),
                    gen_tcp:send(DeviceSocket, term_to_json({[]})),
                    inet:setopts(ClientSocket, [{active, once}, {packet, 0}, binary]),
                    {next_state, state_proxying, NewStateData, ?GENERAL_TIMEOUT};
                {error, Reason} ->
                    log(warn, "Device session pasv accept failed: ~p~n", [Reason]),
                    {stop, failed_pasv_accept, StateData}
            end;
        X ->
            log(warn, "Expected pasv_accept but got: ~p~n", [X])
    end.

state_cmd_session({tcp, DeviceSocket, Data}, 
                  StateData = #state{devicesocket=DeviceSocket, devicerow=DeviceRow}) ->
    JsonTerm = json_to_term(binary_to_list(Data)),
    case util:get_json_value(JsonTerm, <<"action">>) of
        <<"finished">> ->
            log(debug, "Device sent action \"finished\", closing~n", []),
            gen_tcp:close(DeviceSocket),
            {stop, normal, StateData};
        <<"noop">> ->
            log(debug, "Noop received~n", []),
            gen_tcp:send(DeviceSocket, term_to_json({[]})),
            {next_state, state_cmd_session, StateData, ?GENERAL_TIMEOUT};        
        Cmd ->
            log(debug, "Unimplemented command: ~p~n", [Cmd]),
            ResponseJson = util:json_err_obj(0, "Command not implemented~n"),
            gen_tcp:send(DeviceSocket, ResponseJson),
            {next_state, state_cmd_session, StateData, ?GENERAL_TIMEOUT}
    end;

state_cmd_session({control_waiting, Port}, StateData) when is_integer(Port) ->
    % An FTP client has initiated a connection. We now must tell the device to
    % connect on the given port to establish an FTP control connection.
    DeviceSocket = StateData#state.devicesocket,
    AndroidId = (StateData#state.devicerow)#device.android_id,
    log(info, "Informing device ~p of control conn on port ~p~n", [AndroidId, Port]),
    Term = {[{<<"action">>, <<"control_connection_waiting">>},
             {<<"port">>, Port}]},
    gen_tcp:send(DeviceSocket, term_to_json(Term)),
    {next_state, state_cmd_session, StateData, ?GENERAL_TIMEOUT}.

state_proxying({tcp, Socket, Data}, StateData = #state{devicesocket=DeviceSocket,
                                                       clientsocket=ClientSocket}) ->
    case Socket of
        DeviceSocket ->
            %log(debug, "Proxying a packet from device to client~n", []),
            gen_tcp:send(ClientSocket, Data);
        ClientSocket ->
            %log(debug, "Proxying a packet from client to device~n", []),
            gen_tcp:send(DeviceSocket, Data)
    end,
    PacketBytes = size(Data),
    OldSessionBytes = StateData#state.sessionbytes,
    NewStateData = StateData#state{sessionbytes = OldSessionBytes+PacketBytes},
    {next_state, state_proxying, NewStateData, ?GENERAL_TIMEOUT}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData, ?GENERAL_TIMEOUT}.

handle_sync_event(_Event, _From, StateName, StateData) ->
    {next_state, StateName, StateData, ?GENERAL_TIMEOUT}.

terminate(_Reason, StateName, StateData) ->
    %gen_tcp:close(StateData#state.devicesocket),
    %gen_tcp:close(StateData#state.clientsocket),
    DeviceRow = StateData#state.devicerow,
    case StateName of
        state_cmd_session ->
            % If this was a command session, and not an file data session, then
            % remove it from the active session registry
            Prefix = DeviceRow#device.prefix,
            session_registry:remove_self(Prefix),
            ok;
        _OtherState -> ok
    end.
    
% There may be some actions queued up for this device in its device row, stored as
% a list in the #device{} queued_actions field. The actions
% should be performed as soon as is practical. The queued actions takes the
% form of a function, whose input is a #device{} tuple, and returns either
% the atom none, or a list to send to the device.
do_queued_actions(Socket, DeviceRow) ->
    AndroidId = DeviceRow#device.android_id,
    ActionList = DeviceRow#device.queued_actions,
    case ActionList of
        [Action|Rest] ->
            log(debug, "Performing queued action for ~p~n", [AndroidId]), 
            NewDeviceRow = DeviceRow#device{queued_actions=Rest},
            case Action(NewDeviceRow) of
                Data when is_list(Data) ->
                    gen_tcp:send(Socket, Data);
                none -> ok;
                _ ->
                    log(warn, "Queued action returned something unexpected~n", [])
            end,
            do_queued_actions(Socket, NewDeviceRow); % recursive handle remaining actions
        _ ->
            log(debug, "No queued actions for ~p~n", [AndroidId]),
            ok
        
    end.

% These three functions are helpers for queueing up news messages for delivery to a client
% at the next available opportunity.
make_news_fun(Str) ->
    fun(_DeviceRow) -> make_news_obj(Str) end.
make_news_obj(Str) ->
    term_to_json({[{<<"action">>,<<"message">>}, {<<"text">>,list_to_binary(Str)}]}).
-spec enqueue_news(AndroidId::string(), String::string()) -> ok | error.
enqueue_news(AndroidId, String) ->
    Device = db:get_device_row(AndroidId),
    case Device of
        _X when is_record(Device, device) ->
            Queue=[make_news_fun(String)],
            db:write_device_row(Device#device{queued_actions=Queue}),
            ok;
        _ ->
            log(warn, "Failed to enqueue news, invalid device row~n", []),
            error
    end.

log(Level, Format, Args) ->
    log:log(Level, ?MODULE, Format, Args).
