-module(session_registry).
-export([start/0, add/2, remove/1, lookup/1, dump_state/0]).
-import(log, [log/3]).

start() ->
    log(info, "Session registry thread active~n", []),
    register(registry_process, self()),
    main_loop([]),
    log(info, "Session registry quitting~n", []).

% The external interface to other modules
add(Prefix, Pid) when is_list(Prefix), is_pid(Pid) ->
    log(debug, "session_registry:add for ~p, ~p~n", [Prefix, Pid]),
    registry_process ! {self(), {add, {Prefix, Pid}}},
    wait_for_response().

remove(Prefix) when is_list(Prefix) ->
    log(debug, "session_registry:remove for ~p~n", [Prefix]),
    registry_process ! {self(), {remove, Prefix}},
    wait_for_response().
    
lookup(Prefix) when is_list(Prefix) ->
    log(debug, "session_registry:lookup for ~p~n", [Prefix]),
    registry_process ! {self(), {lookup, Prefix}},
    wait_for_response().

dump_state() ->
    registry_process ! {self(), {dump_state}},
    wait_for_response().

wait_for_response() ->
    receive
        {registry, X} -> X
    after 5000 -> % wait 5 sec before failing
        {error, timeout}
    end.
    
main_loop(State) ->
    receive
        {quit, Reason} ->
            log(info, "Quitting due to: ~p~n", [Reason]);
        {From, Operation} ->
            {Response, NewState} = case Operation of
                {dump_state} ->
                    {{registry, State}, State};
                {add, Mapping} ->
                    add_internal(State, Mapping);
                {lookup, Prefix} ->
                    lookup_internal(State, Prefix);
                {remove, Prefix} ->
                    remove_internal(State, Prefix);
                Other ->
                    log(error, "Unsupported registry request: ~p~n", [Other]),
                    {{registry, unsupported}, State}
            end,
            From ! Response,
            main_loop(NewState);
        Message ->
            log(error, "Unsupported registry message: ~p~n", [Message]),
            main_loop(State)
    end.

add_internal(State, Mapping) ->
    log(debug, "session_registry:add_internal for ~p~n", [Mapping]),
    {Prefix, _} = Mapping,
    case proplists:get_value(Prefix, State) of
        undefined ->
            {{registry, success}, [Mapping|State]};
        _ ->
            {{registry, already_present}, State}
    end.
    
% Returns undefined if the Prefix is not present
lookup_internal(State, Prefix) ->
    log(debug, "session_registry:lookup_internal for ~p~n", [Prefix]),
    {{registry, proplists:get_value(Prefix, State)}, State}.
    
remove_internal(State, Prefix) ->
    log(debug, "session_registry:remove_internal for ~p~n", [Prefix]),
    case proplists:get_value(Prefix, State) of
        undefined ->
            {{registry, not_present}, State};
        _ ->
            {{registry, success}, proplists:delete(Prefix, State)}
    end.

