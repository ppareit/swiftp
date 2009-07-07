-module(util).
-import(log, [log/3]).

-compile(export_all).

% Start listening on the given port and return the opened socket. Return
% conventions are the same as for gen_tcp:listen: either {ok, Socket} or
% {error, Reason}. Listening on port 0 will pick a random port.
tcp_listen(Port) -> 
    gen_tcp:listen(Port, [binary,
                          {reuseaddr, true}, 
                          {active, true}]).

% Given two sockets, pass data between them until one of them is closed,
% or Timeout milliseconds pass without data being sent.
proxy_until_close(Socket1, Socket2, Timeout) ->
    receive
        {tcp, Socket, Data} ->
            case Socket of
                Socket1 ->
                    gen_tcp:send(Socket2, Data);
                Socket2 ->
                    gen_tcp:send(Socket1, Data)
            end;
        {tcp_closed, _} ->
            ok
    after Timeout ->
        log(info, "proxy_until_close returning due to timeout~n", [])
    end.
