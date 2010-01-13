-module(db).
-include("records.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([create_master_schema/0, join_db/1,
         reuse_schema/0, all_devices/0, clear_devices/0, create_or_login/1]).
-export([make_prefix_lookup_q/1, get_device_row_dirty/1,
         check_android_id_exists/1, get_device_row/1,
         write_device_row/1]).
-import(json_eep, [term_to_json/1, json_to_term/1]).

% Creates schema, should only be called on initial master node.
% Returns ok or throws something.
create_master_schema() ->
    ok = mnesia:delete_schema([node()]),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    ok = create_tables().

% The master list of tables and attributes
-spec table_list() -> list(tuple()).
table_list() ->
    [{device, [{attributes, record_info(fields, device)},
               {disc_copies, [node()]},
               {record_name, device}]}].
               
-spec create_tables() -> ok | {error, Reason :: atom()}.
create_tables() ->
    create_table_list(table_list()).
-spec create_table_list(ToCreate :: list()) -> ok | {error, Reason :: atom()}.
create_table_list([]) ->
    ok;
create_table_list([{Name, Attributes} | Rest]) ->
    case mnesia:create_table(Name, Attributes) of
        {atomic, ok} ->
            log(info, "Created table ~p OK~n", [Name]),
            create_table_list(Rest);
        {aborted, Reason} ->
            log(error, "Failed to create table ~p due to: ~p~n", [Name, Reason]),
            {error, Reason}
    end.

% Tries to ping the node we are attempting to join to make sure it is up.
% Simply wraps net_adm:ping with a wrapper that will log the results.
-spec ping_master(atom()) -> pong | pang.
ping_master(Target) ->
    case Response = net_adm:ping(Target) of
        pong ->
            log(debug, "DB master responded, proceeding~n", []);
        pang ->
            log(error, "DB master is not responding~n", [])
    end,
    Response.

-spec reuse_schema() -> ok.
reuse_schema() ->
    log(info, "Using on-disk schema for mnesia start~n", []),
    log(debug, "Starting mnesia...~n", []),
    ok = mnesia:start(),
    TablePropList = table_list(),
    TableNames = proplists:get_keys(TablePropList),
    % We want to make sure all necessary tables is in our schema.
    % mnesia:table_info will throw an exception if the table isn't present.
    F = fun(Name) -> mnesia:table_info(Name, all) end,
    lists:foreach(F, TableNames), % might throw exception if table isn't here
    ok.

-spec join_db(atom()) -> ok.
join_db(Target) ->
    log(info, "Joining to master ~p~n", [Target]),
    pong = ping_master(Target),
    log(debug, "Deleting schema...~n", []),
    ok = mnesia:delete_schema([node()]),
    log(debug, "Mnesia starting...~n", []),
    ok = mnesia:start(),
    MyNode = node(),
    % TODO: Not sure why this step is necessary...
    log(debug, "RPC adding this node to schema table copies..~n", []),
    rpc:call(Target, mnesia, add_table_copy, [schema, MyNode, ram_copies]),
    % Add this node to the mnesia cluster
    log(debug, "Adding self to mnesia cluster~n", []),
    {ok, _NodeList} = rpc:call(Target, mnesia, change_config, [extra_db_nodes, [MyNode]]),
    % Persist the schema table to disk
    log(debug, "Changing schema table to disc_copies...~n", []),
    mnesia:change_table_copy_type(schema, MyNode, disc_copies),
    % Run mnesia:add_table_copy for all tables to be replicated on this node
    TableNames = proplists:get_keys(table_list()),
    F = fun(TableName) -> 
            log:log(debug, dbfun, "Replicating table ~p on this node~n", [TableName]),
            %{atomic, ok} = mnesia:add_table_copy(TableName, MyNode, disc_copies)
            mnesia:add_table_copy(TableName, MyNode, disc_copies)
    end,
    lists:map(F, TableNames),
    log(info, "DB init completed.~n", []),
    ok.

% Does a dirty read of the device table and returns the row, if found.
-spec get_device_row_dirty(AndroidId::string()) -> #device{} | no_exist.
get_device_row_dirty(AndroidId) ->
    %log(debug, "Getting device row dirtily for ~p~n", [AndroidId]),
    case mnesia:dirty_read(device, AndroidId) of 
        [Row] -> Row;
        _ -> no_exist
    end.

-spec get_device_row(AndroidId::string()) -> #device{} | no_exist.
get_device_row(AndroidId) ->
    Q = fun() ->
        qlc:e(qlc:q([Row || Row <- mnesia:table(device),
                            Row#device.android_id == AndroidId]))
    end,
    case mnesia:transaction(Q) of
        {atomic, [Row]} -> Row;
        _ -> no_exist
    end.

-spec create_or_login(AndroidId :: string()) -> 
    {ok, #device{}} | error.
create_or_login(AndroidId) ->
    QueryFun =  fun() ->
        case qlc:e(qlc:q([R || R <- mnesia:table(device),
                                    R#device.android_id == AndroidId])) of
            [] -> % This android_id doesn't have a prefix yet                
                Prefix = intrans_find_unused_prefix(),
                NowTime = erlang:localtime(),
                DeviceRow = #device{android_id = AndroidId,
                                    prefix = Prefix,
                                    creation_time = NowTime,
                                    last_login = NowTime},
                case mnesia:write(DeviceRow) of
                    ok -> DeviceRow;
                    _  -> mnesia:abort(write_failed)
                end;
            [DeviceRow] -> % This android_id exists in the DB
                DeviceRow;
            _Other ->  % Shouldn't have duplicates in the DB, but let's handle it
                mnesia:abort(duplicate_device_row)
        end
    end,

    case mnesia:transaction(QueryFun) of
        {atomic, DeviceRow} ->
            {ok, DeviceRow};
        {aborted, Reason} ->
            log(warn, "Couldn't add device row for ~p: ~p~n", [AndroidId, Reason]),
            error;
        _Other ->
            log(warn, "Error adding device row for ~p~n", [AndroidId]),
            error
    end.            

% This is obsolete now that there is no authentication and no secret data.
%-spec authenticate(AndroidId :: string(), Secret :: string()) -> {true, #device{}}
%                                                               | false
%                                                               | error.
%authenticate(AndroidId, Secret) ->
%    log(debug, "Trying to auth with ~p/~p~n", [AndroidId, Secret]),
%    QueryFun = fun() ->
%        % TODO: update last login time
%        qlc:e(qlc:q([Row || Row <- mnesia:table(device),
%                            AndroidId == Row#device.android_id,
%                            Secret == Row#device.secret]))
%    end,
%    case mnesia:transaction(QueryFun) of
%        {atomic, [DeviceRow]} ->
%            log(debug, "Authenticated android_id ~p~n", [AndroidId]),
%            {true, DeviceRow};
%        {atomic, _ }->
%            log(info, "Query success but no row for ~p~n", [AndroidId]),
%            false;
%        Err ->
%            log(error, "DB error looking up ~p: ~p~n", [AndroidId, Err]),
%            error
%    end.
        
    
% This is obsolete now that there is no authentication and no secret data.
% Unpack and execute an incoming JSON "create_account" action
% Returns a response {ok, Row#device} on success, {error, Reason} on failure
%-spec create_account(AndroidId :: string()) -> {ok, #device{}} | {error, Reason :: atom()}.
%create_account(AndroidId) ->
%    NowTime = erlang:localtime(),
%    DeviceRow = #device{android_id = AndroidId,
%                        secret = generate_secret(),
%                        prefix = none,  % will be filled in later when writing db
%                        creation_time = NowTime,
%                        last_login = NowTime,
%                        totalbytes=0},
%    AddFun = make_user_add_fun(DeviceRow),
%    case mnesia:transaction(AddFun) of
%        {atomic, CompleteRow} when is_record(CompleteRow, device) ->
%            log(info, "Added device ~p to database~n", [AndroidId]),
%            {ok, CompleteRow}; % Now the prefix has been set
%        {aborted, already_exists} ->
%            log(info, "Tried to create account for existing android_id ~p~n", [AndroidId]),
%            {error, already_exists};
%        ErrReason ->
%            log(error, "Failed adding device due to ~p~n", [ErrReason]),
%            {error, database_failed}
%    end.

% This function returns a function that can be passed to mnesia:transaction 
% that will add a new user to the database.
-spec make_user_add_fun(Row :: #device{}) -> fun().
make_user_add_fun(Row) ->
    fun() ->
            % Ensure there's not already a row/account for this android_id
            case qlc:e(qlc:q([R || R <- mnesia:table(device),
                                   R#device.android_id == Row#device.android_id])) of
                [] ->                      
                    % The row to be added doesn't have a prefix yet
                    Prefix = intrans_find_unused_prefix(),
                    CompleteRow = Row#device{prefix = Prefix},
                    case mnesia:write(CompleteRow) of
                        ok -> CompleteRow;
                        _ ->  mnesia:abort(write_failed)
                    end;
                _Other ->
                    mnesia:abort(already_exists)
            end
    end.
            
% Generates an unused device prefix string. Can only be called inside an
% mnesia transaction.
-spec intrans_find_unused_prefix() -> string().
intrans_find_unused_prefix() ->
    Prefix = rand:random_alpha(5),
    % See if prefix already exists in the database
    Query = make_prefix_lookup_q(Prefix),
    case qlc:e(Query) of
        [] ->
            % The prefix is unused, so use it
            Prefix;
        [_|_] ->
            % Prefix is used, try a different one
            log(debug, "Prefix ~p is used, trying a different one~n", [Prefix]),
            intrans_find_unused_prefix()
    % We deliberately ignore the case {error, ...} , which will
    % result in an exception being thrown
    end.

%% A reusable tidbit that returns a device row based on the prefix.
make_prefix_lookup_q(Prefix) ->
    qlc:q([X || X <- mnesia:table(device),
                    Prefix == X#device.prefix]).
    


%% fake_device_row() ->
%%     #device{android_id=none, 
%%             prefix=none, 
%%             secret=none, 
%%             creation_time=none, 
%%             last_login=none}.
    
-spec generate_secret() -> string().
generate_secret() ->
    rand:random_alnum(16).

all_devices() ->
    QueryFun = fun() ->
        qlc:e(qlc:q([D || D <- mnesia:table(device)]))
    end,
    case mnesia:transaction(QueryFun) of
        {atomic, Devices} ->
            Devices;
        X -> X
    end.

clear_devices() ->
    log(info, "Clearing device table~n", []),
    mnesia:clear_table(device).


-spec check_android_id_exists(AndroidId :: string()) -> exists | no_exists.
check_android_id_exists(AndroidId) ->
    Fun = fun() ->
        case qlc:e(qlc:q([D || D <- mnesia:table(device),
                               D#device.android_id == AndroidId])) of
            [_Row] ->
                exists;
            _ ->
                no_exists
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, exists} ->
            exists;
        _ ->
            no_exists
    end.
                                        
write_device_row(Row) when is_record(Row, device) ->
    Fun = fun() ->
        mnesia:write(Row)
    end,
    mnesia:transaction(Fun).


log(Level, Format, Args) ->
    log:log(Level, ?MODULE, Format, Args).

