-module(connection_matcher).

-import(log, [log/2, log/3]).
-export([start/1]).

% The job of a "matcher" is to: 
% 1. take an incoming connection from an FTP client
% 2. Pretend to be an FTP server
% 3. When the client sends "USER prefix_username", take the prefix and resolve
%    it to an existing PID
% 4. Pass the client socket along with the username to the PID, which will
%    proxy between the socket representing the FTP client and the socket
%    representing the device running SwiFTP server.

start(Socket) ->
    gen_tcp:controlling_process(Socket, self()),
    gen_tcp:send(Socket, <<"220 SwiFTP cloud proxy ready\r\n">>),
    try get_parse_login(Socket) of
        {Prefix, Username} ->
            case session_registry:lookup(Prefix) of
                Pid when is_pid(Pid) ->
                    log(debug, "Looked up ~p: ~p~n", [Prefix, Pid]),
                    Pid ! {clientsocket, Socket, binary_to_list(Username)},
                    gen_tcp:controlling_process(Socket, Pid);
                Other ->
                    log(info, "Prefix pid lookup: ~p, err: ~p~n", [Prefix, Other]),
                    gen_tcp:send(Socket, <<"530 Bad credentials, check prefix ",
                                           "and username\r\n">>),
                    gen_tcp:close(Socket)

            end;
        _ -> log(info, "Client closed without ever sending valid USER~n")
    catch
        X : Y -> 
            log("Matcher exception ~p:~p~n", [X, Y]),
            gen_tcp:close(Socket)
    end,
    log(info, "Matcher exiting~n").

get_parse_login(Socket) ->
    receive
        %{tcp, Socket, <<"USER ", P1, P2, P3, P4, P5, P6, "_", Username/binary>> } ->
        {tcp, Socket, <<"USER ", PrefixBin:6/binary-unit:8, "_", Username/binary>>} ->
            Prefix = binary_to_list(PrefixBin),
            log(debug, "Matched with ~p/~p~n", [Prefix, Username]),
            {Prefix, Username};
        {tcp_closed, Socket} ->
            tcp_closed;
        Data ->
            log(debug, "Didn't match~n"),
            case Data of
                <<"USER ", _/binary>> ->
                    gen_tcp:send(Socket, <<"530 Check your username prefix\r\n">>);
                _ ->                                         
                    gen_tcp:send(Socket, <<"500 Login with USER first\r\n">>)
            end,
            get_parse_login(Socket)
    end.

