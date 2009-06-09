-module(session).
-export([start/1, random_alnum/1]).
-import(log, [log/2, log/3]).
-import(gen_tcp, [recv/2]).

-record(State, {device_id, session_bytes, total_bytes, quota}).

start(Socket) ->
    log(info, "Session thread started~n"),
    gen_tcp:controlling_process(Socket, self()),
    Prefix = get_unused_prefix(),
    try session_registry:add(Prefix, self()) of
        success ->
            % Hand-craft some simple JSON
            gen_tcp:send(Socket, "{\"options\": { \"prefix\": \"" ++ Prefix ++ "\"}}\r\n"),
            wait_loop(Socket);
        X ->
            log(error, "Session add error: ~p~n", [X])
    catch
        X : Y -> log(info, "Exception in session, ~p:~p~n", [X, Y])
    after
        gen_tcp:close(Socket),
        session_registry:remove(Prefix)
    end,
    log(info, "Session thread exiting~n").
    
% This is the loop we're in while the device is connected, but the FTP client
% has not yet connected
wait_loop(DeviceSocket) ->
    receive
        {tcp, DeviceSocket, Data} ->
            handle_data(Data),
            wait_loop(DeviceSocket); 
        {tcp_closed, DeviceSocket} ->
            gen_tcp:close(DeviceSocket);
        {clientsocket, ClientSocket, Username} -> 
            gen_tcp:controlling_process(ClientSocket, self()),
            gen_tcp:send(DeviceSocket, "USER " ++ Username),
            case proxy_loop(DeviceSocket, ClientSocket) of
                quit ->
                    gen_tcp:close(DeviceSocket);
                dont_quit -> 
                    wait_loop(DeviceSocket)
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
                    log(info, "Device socket closed~n"),
                    quit;
                CliSocket -> 
                    log(info, "Client socket closed~n"),
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
            
    
% Generate a random alphanumeric string of the given length
random_alnum(Length) -> random_alnum(Length, []).
random_alnum(0, Accum) -> Accum;
random_alnum(Length, Accum) ->
    % There are 26+26+10=62 possible alphanumeric characters
    R = case rand:rand(62) of
        X when 1 =< X, X =< 10 -> 
            % Range 1 to 10 will be treated as digits
            % since ASCII integers start at 48, we add 48-1=44
            X+47;
        X when 11 =< X, X =< 36 ->
            % Range 11 to 36 will be treated as upper case letters
            % upper case letters start at ASCII 65, so add 65-11=54
            X+54;
        X when 37 =< X, X =< 62 ->
            % Range 37 to 62 will be treated as lower case letters
            % lower case letters start at ASCII 97, so add 97-37=60
            X+60
    end,
    random_alnum(Length-1, [R|Accum]).
