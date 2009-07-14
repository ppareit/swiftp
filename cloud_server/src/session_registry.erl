-module(session_registry).
-behavior(gen_server).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).
-export([start_link/0, lookup/1, remove/1, add/2]).

-import(log, [log/3]).
-import(json_eep, [json_to_term/1, term_to_json/1, get_state/0]).

% TODO: implement a "clean/0" function that scans the table and removes stale entries

%% The external interface to other modules
-spec add(Prefix :: string(), Pid :: pid()) -> ok | already_exists | error.
add(Prefix, Pid) when is_list(Prefix), is_pid(Pid) ->
    log(debug, "session_registry:add for ~p, ~p~n", [Prefix, Pid]),
    gen_server:call(session_registry, {add, {Prefix, Pid}}).

-spec remove(Prefix :: string()) -> ok.
remove(Prefix) when is_list(Prefix) ->
    log(debug, "session_registry:remove for ~p~n", [Prefix]),
    gen_server:call(session_registry, {remove, Prefix}).
    
-spec lookup(Prefix :: string()) -> pid() | none. 
lookup(Prefix) when is_list(Prefix) ->
    log(debug, "session_registry:lookup for ~p~n", [Prefix]),
    gen_server:call(session_registry, {lookup, Prefix}).

%% dump_state() ->
%%     gen_server:call(session_registry, {remove, Prefix}).
%%     wait_for_response().


start_link() ->
    gen_server:start_link({local, session_registry}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    {ok, ets:new(registry_ets, [set])}.  % Unordered, key must be unique         

handle_call({Action, Param}, _From, State = {EtsTable}) ->
    case Action of
        add ->
            {reply, 
             add_internal(EtsTable, Param), % Param has form {Prefix, Pid}
             State};
        lookup ->
            {reply, 
             lookup_internal(EtsTable, Param), % Param has form string()
             State};
        remove ->
            remove_internal(EtsTable, Param), % Param has form string()
            {reply,
             ok,
             State}
    end.
            

add_internal(EtsTable, Param = {Prefix, Pid}) when is_list(Prefix), is_pid(Pid) ->
    ets:insert(EtsTable, Param).

lookup_internal(EtsTable, Prefix) when is_list(Prefix) ->
    case ets:lookup(EtsTable, Prefix) of
        [{Prefix, Pid}] when is_pid(Pid) ->
            Pid;
        _ ->
            none
    end.

remove_internal(EtsTable, Prefix) ->
    ets:delete(EtsTable, Prefix).

terminate(Reason, _State) ->
    case Reason of
        normal -> ok;
        shutdown -> ok;
        {shutdown, _} -> ok;
        _ ->
            % If this thread dies,  kill all device sessions,
            % since all prefix-to-process mappings will be lost.
            % It would be nice to not kill processes that are in mid-transfer, but
            % that require some re-architecting.
            log(error, "Session registry terminated due to: ~p~n", [Reason]),
            log(error, "Restarting device session supervisor to reconnect devices~n", []),
            supervisor:restart_child(swiftp_proxy, device_session_sup),
            ok
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

%% start() ->
%%     log(info, "Session registry thread active~n", []),
%%     register(registry_process, self()),
%%     main_loop([]),
%%     log(info, "Session registry quitting~n", []).
%% 
%% 
%% wait_for_response() ->
%%     receive
%%         {registry, X} -> X
%%     after 5000 -> % wait 5 sec before failing
%%         {error, timeout}
%%     end.
%%     
%% main_loop(State) ->
%%     receive
%%         {quit, Reason} ->
%%             log(info, "Quitting due to: ~p~n", [Reason]);
%%         {From, Operation} ->
%%             {Response, NewState} = case Operation of
%%                 {dump_state} ->
%%                     {{registry, State}, State};
%%                 {add, Mapping} ->
%%                     add_internal(State, Mapping);
%%                 {lookup, Prefix} ->
%%                     lookup_internal(State, Prefix);
%%                 {remove, Prefix} ->
%%                     remove_internal(State, Prefix);
%%                 Other ->
%%                     log(error, "Unsupported registry request: ~p~n", [Other]),
%%                     {{registry, unsupported}, State}
%%             end,
%%             From ! Response,
%%             main_loop(NewState);
%%         Message ->
%%             log(error, "Unsupported registry message: ~p~n", [Message]),
%%             main_loop(State)
%%     end.
%% 
%% add_internal(State, Mapping) ->
%%     log(debug, "session_registry:add_internal for ~p~n", [Mapping]),
%%     {Prefix, _} = Mapping,
%%     case proplists:get_value(Prefix, State) of
%%         undefined ->
%%             {{registry, success}, [Mapping|State]};
%%         _ ->
%%             {{registry, already_present}, State}
%%     end.
%%     
%% % Returns undefined if the Prefix is not present
%% lookup_internal(State, Prefix) ->
%%     log(debug, "session_registry:lookup_internal for ~p~n", [Prefix]),
%%     {{registry, proplists:get_value(Prefix, State)}, State}.
%%     
%% remove_internal(State, Prefix) ->
%%     log(debug, "session_registry:remove_internal for ~p~n", [Prefix]),
%%     case proplists:get_value(Prefix, State) of
%%         undefined ->
%%             {{registry, not_present}, State};
%%         _ ->
%%             {{registry, success}, proplists:delete(Prefix, State)}
%%     end.
%% 
