-module(device_session).
-export([start/1]).
-import(log, [log/2, log/3]).
-import(gen_tcp, [recv/2]).
-import(security, [auth_or_create_account/1]).
-import(rand, [random_alnum/1]).
-import(json_eep, [json_to_term/1, term_to_json/1]).

-record(state, {device_id, session_bytes, total_bytes, quota}).

start(Socket) ->
    log(info, "Command thread started~n", []),
    gen_tcp:controlling_process(Socket, self()), % assign socket control to me
    Prefix = get_unused_prefix(),
    try auth_or_create_account(Socket) of
        false ->
            log(info, "Authenticate failed~n", []);
        true -> 
            log(info, "Authenticated successfully~n", []),
            % There is a little race condition here, between creating and registering
            % the new prefix, someone else may have registered it. Unlikely, though.
            case session_registry:add(Prefix, self()) of
                success ->
                    main_loop(Socket, Prefix);
                X ->
                    log(error, "Session add error: ~p~n", [X])
            end,
            main_loop(Socket, Prefix)
    %catch
    %    X : Y -> log(warn, "Exception in command session, ~p:~p~n", [X, Y])
    after
        gen_tcp:close(Socket),
        session_registry:remove(Prefix)
    end,
    log(info, "Command session thread exiting~n", []).
    
% We enter this loop after the device has authenticated or created an account
main_loop(Socket, Prefix) ->
    receive
        {tcp, Socket, Data} ->
            log(debug, "Json data to parser: ~p~n", [Data]),
            {JsonPropList} = json_to_term(binary_to_list(Data)),
            case proplists:get_value(<<"action">>, JsonPropList) of
                <<"start_command_session">> ->
                    PrefixBin = list_to_binary(Prefix),
                    gen_tcp:send(Socket, 
                                 term_to_json({[{<<"prefix">>, PrefixBin}]})),
                    command_session(Socket);
                <<"data_pasv_listen">> ->
                    data_pasv_listen(Socket);
                <<"data_port_connect">> ->
                    data_port_connect(Socket, JsonPropList)
            end;
        {tcp_closed, Socket} ->
            ok
        %{clientsocket, ClientSocket, Username} -> 
        %    gen_tcp:controlling_process(ClientSocket, self()),
        %    gen_tcp:send(DeviceSocket, "USER " ++ Username),
        %    case proxy_loop(DeviceSocket, ClientSocket) of
        %        quit ->
        %            gen_tcp:close(DeviceSocket);
        %        dont_quit -> 
        %            wait_loop(DeviceSocket)
        %    end
    end.

command_session(Socket) ->
    receive
        {tcp, Socket, Data} ->
            log(info, "Received unexpected data in command session: ~p~n", [Data]),
            command_session(Socket);
        {tcp_closed, Socket} ->
            log(info, "Command session closed~n", []);
        {control_connection_waiting, Port} ->
            PortString = integer_to_list(Port),
            Json = term_to_json({[{<<"action">>, <<"control_connection_waiting">>},
                                  {<<"port">>, <<PortString>>}]}),
            gen_tcp:send(Socket, Json),
            log(debug, "Told client about waiting control connection on ~p~n", [Port]),
            command_session(Socket)
    end.

data_pasv_listen(Socket) ->
    case util:tcp_listen(0) of
        {error, Reason} ->
            log(warn, "Couldn't open pasv socket: ~p~n", [Reason]),
            Json = term_to_json({[{<<"errorCode">>, 0},
                                  {<<"errorString">>, <<"Couldn't open port">>}]}),
            gen_tcp:send(Socket, Json);
        {ok, NewSocket} ->
            {ok, Port} = inet:port(NewSocket),
            PortString = integer_to_list(Port),
            log(debug, "Opened pasv socket on port: ~p~n", [Port]),
            Json = term_to_json({[{<<"port">>, <<PortString>>}]}),
            gen_tcp:send(Socket, Json),
            wait_for_pasv_accept(Socket, NewSocket),
            % When we return here, the proxying is over, and it's time to clean up
            gen_tcp:close(NewSocket)
    end.
        
wait_for_pasv_accept(DeviceSocket, ListenSocket) ->
    receive
        {tcp, DeviceSocket, Data} ->
            {JsonPropList} = json_to_term(Data),
            case proplists:get_value(<<"action">>, JsonPropList) of
                <<"data_pasv_accept">> ->
                    case gen_tcp:accept(ListenSocket) of
                        {ok, ClientSocket} ->
                            log(info, "pasv accept OK, proxying~n", []),
                            util:proxy_until_close(ClientSocket, DeviceSocket, 30000);
                        X ->
                            log(warn, "pasv accept error: ~p~n", [X])
                    end;
                Other ->
                    log("Got unexpected request waiting for pasv_accept: ~p~n", 
                        [Other])
            end
    after 120000 ->
        % Require data_pasv_accept within 2 minutes of data_pasv_listen 
        log(warn, "Timed out between data_pasv_listen and data_pasv_accept~n", [])
    end.
                    
data_port_connect(DeviceSocket, JsonPropList) ->
    ErrorJson = term_to_json({[{<<"errorCode">>, 9},
                               {<<"errorString">>, <<"Missing argument">>}]}),
    case proplists:get_value(<<"address">>, JsonPropList) of
        undefined ->
            gen_tcp:send(DeviceSocket, ErrorJson);
        Address ->
            case proplists:get_value(<<"port">>, JsonPropList) of
                undefined ->
                    gen_tcp:send(DeviceSocket, ErrorJson);
                Port ->
                    log(debug, "Opening outbound socket to ~p:~p~n", [Address, Port]),
                    log(warn, "Not really~n", [])
            end
    end.

% This is the loop we're in when both connections have been established 
proxy_loop(DevSocket, CliSocket) -> 
    receive
        {tcp, DevSocket, Data} -> 
            gen_tcp:send(CliSocket, Data);
        {tcp, CliSocket, Data} ->
            gen_tcp:send(DevSocket, Data);
        {tcp_closed, Socket} -> 
            gen_tcp:close(CliSocket),
            case Socket of 
                DevSocket -> 
                    log(info, "Device socket closed~n", []),
                    quit;
                CliSocket -> 
                    log(info, "Client socket closed~n", []),
                    dont_quit
            end;
        _ -> 
            ignore
     end.
    

% Repeated generate prefix strings until we find an unused one
get_unused_prefix() -> get_unused_prefix(50). % Try max 50 times
get_unused_prefix(0) -> throw("Could not find unused prefix");
get_unused_prefix(TriesLeft) ->
    Candidate = random_alnum(6),
    case session_registry:lookup(Candidate) of
        undefined -> Candidate;
        _ -> get_unused_prefix(TriesLeft-1)
    end.
