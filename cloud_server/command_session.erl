-module(command_session).
-export([start/1]).
-import(log, [log/2, log/3]).
-import(gen_tcp, [recv/2]).
-import(security, [auth_or_create_account/1]).
-import(rand, [random_alnum/1]).

-record(state, {device_id, session_bytes, total_bytes, quota}).

start(Socket) ->
    log(info, "Command thread started~n", []),
    gen_tcp:controlling_process(Socket, self()),
    Prefix = get_unused_prefix(),
    log(info, "Here1~n",[]),
    try auth_or_create_account(Socket) of
        false ->
            log(info, "Authenticate failed~n", []);
        true -> 
            log(info, "Authenticated successfully~n", []),
            % There is a little race condition here, between creating and registering
            % the new prefix, someone else may have registered it. Unlikely, though.
            case session_registry:add(Prefix, self()) of
                success ->
                    main_loop(Socket);
                X ->
                    log(error, "Session add error: ~p~n", [X])
            end
    %catch
    %    X : Y -> log(info, "Exception in command session, ~p:~p~n", [X, Y])
    after
        gen_tcp:close(Socket),
        session_registry:remove(Prefix)
    end,
    log(info, "Command session thread exiting~n", []).
    
% This is the loop we're in while the device is connected, but the FTP client
% has not yet connected
main_loop(DeviceSocket) ->
    receive
        {tcp, DeviceSocket, Data} ->
            handle_data(Data),
            main_loop(DeviceSocket); 
        {tcp_closed, DeviceSocket} ->
            gen_tcp:close(DeviceSocket)
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
    
handle_data(Data) ->
    log(debug, "Dropping incoming data: ~p~n", [Data]).
    

% Repeated generate prefix strings until we find an unused one
get_unused_prefix() -> get_unused_prefix(50). % Try max 50 times
get_unused_prefix(0) -> throw("Could not find unused prefix");
get_unused_prefix(TriesLeft) ->
    Candidate = random_alnum(6),
    case session_registry:lookup(Candidate) of
        undefined -> Candidate;
        _ -> get_unused_prefix(TriesLeft-1)
    end.
