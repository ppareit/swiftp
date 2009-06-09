-module(log).
-export([log/1, log/2, log/3]).

% A wrapper for log() providing a default log level of "info"
log(Message) -> log(info, Message).

% A wrapper for log() providing a default empty argument list
log(Level, Message) -> log(Level, Message, []).

log(_Level, Message, Args) ->
    io:format(Message, Args).
    

