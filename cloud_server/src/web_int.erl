-module(web_int).
-export([test/3]).

test(SessionId, _Env, _Input) ->
    mod_esi:deliver(SessionId, "I am some dynamic content").

    