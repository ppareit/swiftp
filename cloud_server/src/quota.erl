-module(quota).
-behavior(gen_server).
-include("records.hrl").
-export([start_link/0, init/1, terminate/2, code_change/3, handle_call/3,
         handle_info/2, handle_cast/2]).
-export([session_starting/3, check/1, closed_session/1, note_usage/2,
         report_usage/1, initial_quota/0, disable_device/1, increase_quota/2,
         grant_infinite_quota/1, infinite_quota_constant/0]).

-record(entry, {android_id, used, quota}).

-define(INITIAL_QUOTA, 10000000).
-define(QUOTA_DISABLED, 0).
-define(QUOTA_INFINITE, -1).  % Must be kept in sync with web services


% Every time a data socket closes, it reports the number of bytes to this
% module using report_usage. This module keeps a running total of the bytes
% used in an ETS table.
%
% When a command session session is closed, device_session will call
% closed_session(), which will flush the quota statistics from the ETS
% table to the database device table, and remove the ETS row.
%
% The reason for this design is to minimize the number of database
% operations in favor of local ETS operations. The database should
% only be touched when a device command session is started or ended.


initial_quota() ->
    ?INITIAL_QUOTA.

infinite_quota_constant() ->
    ?QUOTA_INFINITE.

start_link() ->
    gen_server:start_link({local, quota_proc}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    EtsTable = ets:new(quota_ets, [set, {keypos, 2}]), % keypos 2 since using record
    {ok, EtsTable}.

handle_call({session_starting, AndroidId, BytesUsed, Quota}, _From, State) ->
    EtsTable = State,
    % If there's another session open for the given AndroidId, then use
    % the higher of the two byte counts.
    StartByteCount = case ets:lookup(EtsTable, AndroidId) of
        [#entry{used=ExistingCount, _ = _}] -> util:maxint(ExistingCount, BytesUsed);
        _                                   -> BytesUsed
    end,
    Row = #entry{android_id=AndroidId, used=StartByteCount, quota=Quota},
    log(debug, "Starting quota tracking session for ~p~n", [AndroidId]),
    ets:insert(EtsTable, Row),
    {reply, ok, EtsTable};
handle_call({check, AndroidId}, _From, State) ->
    EtsTable = State,
    case ets:lookup(EtsTable, AndroidId) of
        [Entry = #entry{used=Used,quota=Quota}] ->
            if
                Quota == ?QUOTA_INFINITE ->
                    % A quota of 1 is a special value meaning "unlimited"
                    {reply, under_quota, EtsTable};
                Used =< Quota ->
                    {reply, under_quota, EtsTable};
                true ->
                    % We are over quota. We'll check the database to be sure that
                    % the user hasn't recently increased their quota.
                    #device{quota=DbQuota} = db:get_device_row_dirty(AndroidId),
                    if
                        Used > DbQuota ->
                            log(info, "Client is over quota: ~p~n", [AndroidId]),
                            {reply, over_quota, EtsTable};
                        true ->
                            % After reading the database, the quota is OK.
                            % We update the ETS table to have the new quota.
                            NewEntry = Entry#entry{quota=DbQuota},
                            ets:insert(EtsTable, NewEntry),
                            {reply, under_quota, EtsTable}
                    end
            end;
        _ ->
            {reply, error, EtsTable}
    end;
handle_call({note_usage, AndroidId, SessionBytes}, _From, State) ->
    EtsTable = State,
    case ets:lookup(EtsTable, AndroidId) of
        [Entry = #entry{used=OldCount}] ->
            NewEntry = Entry#entry{used=OldCount+SessionBytes},
            ets:insert(EtsTable, NewEntry);
        _ ->
            log(warn, "Can't note quota data, android_id not in ETS~n", [])
    end,
    {reply, ok, EtsTable};
            
            
handle_call({closed_session, AndroidId}, _From, State) ->
    % Persist the quota usage stats to the database.
    EtsTable = State,
    case ets:lookup(EtsTable, AndroidId) of
        [Entry] ->
            persist_entry(Entry),
            ets:delete(EtsTable, Entry);    
        _ ->
            log(warn, "Couldn't persist non-existent ETS quota row~n", [])
    end,
    {reply, ok, EtsTable};
handle_call({grant_infinite_quota, AndroidId}, _From, State) ->
    % The call to db:set_device_quota() returns ok | error
    {reply, db:set_device_quota(AndroidId, ?QUOTA_INFINITE), State};
handle_call({report_usage, AndroidId}, _From, State) ->
    % Lookup the quota data for the given device in the ETS table if present,
    % otherwise check the database.
    EtsTable = State,
    case ets:lookup(EtsTable, AndroidId) of
        [Entry] -> 
            {reply, {Entry#entry.used, Entry#entry.quota}, EtsTable};
        _ -> 
            case db:get_device_row_dirty(AndroidId) of
                Device when is_record(Device, device) ->
                    {reply, {Device#device.totalbytes, Device#device.quota}, EtsTable};
                _ ->
                    {reply, not_present, EtsTable}
            end
    end;
handle_call({increase_quota, AndroidId, AddQuota}, _From, State) ->
    Reply = db:add_to_quota(AndroidId, AddQuota),  % either ok | error
    {reply, Reply, State};
handle_call({disable_device, AndroidId}, _From, State) ->
    Reply = db:set_device_quota(AndroidId, 0),
    {reply, Reply, State};
handle_call({persist_all}, _From, State) ->
    EtsTable = State,
    log(info, "Persisting all quota entries~n", []),
    ets:foldl(fun persist_entry/1, [], EtsTable),
    log(info, "Finished persisting all quota entries~n", []),
    {reply, ok, EtsTable}.

persist_entry(#entry{android_id=AndroidId, used=Used}) ->
    db:update_quota_usage(AndroidId, Used),
    ok.

terminate(_Args, _State) -> ok.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_cast(_Request, State) -> {noreply, State}.


% Interface to other modules:

-spec session_starting(AndroidId::string(), 
                       BytesUsed::integer(),
                       Quota::integer()) -> ok.
session_starting(AndroidId, BytesUsed, Quota) 
 when is_list(AndroidId), is_integer(BytesUsed), is_integer(Quota) ->
    gen_server:call(quota_proc, {session_starting, AndroidId, BytesUsed, Quota}).

-spec check(AndroidId::string()) -> over_quota | under_quota.
check(AndroidId) when is_list(AndroidId) ->
    case gen_server:call(quota_proc, {check, AndroidId}) of
        over_quota -> over_quota;
        under_quota ->  under_quota;
        % In the case of some internal error, the graceful behavior is to
        % allow all file transfers
        _ -> under_quota
    end.

-spec closed_session(AndroidId::string()) -> ok.
closed_session(AndroidId) when is_list(AndroidId) ->
    gen_server:call(quota_proc, {closed_session, AndroidId}).

-spec note_usage(AndroidId::string(), NumBytes::integer()) -> ok.
note_usage(AndroidId, NumBytes) when is_list(AndroidId), is_integer(NumBytes) ->
    gen_server:call(quota_proc, {note_usage, AndroidId, NumBytes}).

-spec report_usage(AndroidId::string()) -> 
    {Used::integer(), Quota::integer()} | not_present.
report_usage(AndroidId) when is_list(AndroidId) ->
    gen_server:call(quota_proc, {report_usage, AndroidId}).

-spec increase_quota(AndroidId::string(), AddQuota::integer()) -> ok | error.
increase_quota(AndroidId, AddQuota) ->    
    gen_server:call(quota_proc, {increase_quota, AndroidId, AddQuota}).

-spec disable_device(AndroidId :: string()) -> ok | error.
disable_device(AndroidId) ->
    gen_server:call(quota_proc, {disable_device, AndroidId}).

-spec grant_infinite_quota(AndroidId :: string()) -> ok | error.
grant_infinite_quota(AndroidId) ->
    gen_server:call(quota_proc, {grant_infinite_quota, AndroidId}).

log(Level, Format, Args) ->
    log:log(Level, ?MODULE, Format, Args).

