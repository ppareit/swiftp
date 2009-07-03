-module(log).
-export([log/3]).

% A wrapper for log() providing a default log level of "info"
%log(Message) -> log(info, Message).

% A wrapper for log() providing a default empty argument list
%log(Level, Message) -> log(Level, Message, []).

log(_Level, Message, Args) when is_atom(_Level), is_list(Message), is_list(Args) ->
    io:format(Message, Args).
    

