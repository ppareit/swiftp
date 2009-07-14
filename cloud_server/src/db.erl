-module(db).
-include("records.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([create_master_schema/0, join_db/1, authenticate/2, create_account/1,
         reuse_schema/0, all_devices/0]).
-export([find_unused_prefix/0, make_prefix_lookup_q/1]).
-import(log, [log/3]).
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
            log:log(debug, "Replicating table ~p on this node~n", [TableName]),
            %{atomic, ok} = mnesia:add_table_copy(TableName, MyNode, disc_copies)
            mnesia:add_table_copy(TableName, MyNode, disc_copies)
    end,
    lists:map(F, TableNames),
    log(info, "DB init completed.~n", []),
    ok.
    

% Unpack and execute an incoming JSON "authenticate" action
-spec authenticate(AndroidId :: string(), Secret :: string()) -> {true, #device{}}
                                                               | false
                                                               | error.
authenticate(AndroidId, Secret) ->
    QueryFun = fun() ->
        % TODO: update last login time
        qlc:e(qlc:q([Row || Row <- mnesia:table(device),
                            AndroidId == Row#device.android_id,
                            Secret == Row#device.secret]))
    end,
    case mnesia:transaction(QueryFun) of
        {atomic, [DeviceRow]} ->
            log(debug, "Authenticated android_id ~p~n", [AndroidId]),
            {true, DeviceRow};
        {atomic, _ }->
            log(info, "Query success but no row for ~p~n", [AndroidId]),
            false;
        Err ->
            log(error, "DB error looking up ~p: ~p~n", [AndroidId, Err]),
            error
    end.
        
    
% Unpack and execute an incoming JSON "create_account" action
% Returns a response {ok, Row#device} on success, {error, Reason} on failure
-spec create_account(AndroidId :: string()) -> {ok, #device{}} | {error, Reason :: atom()}.
create_account(AndroidId) ->
    NowTime = erlang:localtime(),
    DeviceRow = #device{android_id = AndroidId,
                        secret = generate_secret(),
                        prefix = none,  % will be filled in later when writing db
                        creation_time = NowTime,
                        last_login = NowTime},
    AddFun = make_user_add_fun(DeviceRow),
    case mnesia:transaction(AddFun) of
        {atomic, CompleteRow} when is_record(CompleteRow, device) ->
            log(info, "Added device ~p to database~n", [AndroidId]),
            {ok, CompleteRow}; % Now the prefix has been set
%%             % The fun() picked a prefix that we don't know. So now we query
%%             % the database to get the new row.
%%             QueryFun = fun() -> qlc:e(qlc:q([X || X <- mnesia:table(device),
%%                                                   AndroidId = X#device.android_id]))
%%                     end,
%%             case mnesia:transaction(QueryFun) of
%%                 {atomic, [Row]} -> 
%%                     {ok, Row};
%%                 _ ->
%%                         log(error, "Lookup after add failed~n", []),
%%                         {error, database_failed}
%%             end;
        ErrReason ->
            log(error, "Failed adding device due to ~p~n", [ErrReason]),
            {error, database_failed}
    end.

% This function returns a function that can be passed to mnesia:transaction 
% that will add a new user to the database.
-spec make_user_add_fun(Row :: #device{}) -> fun().
make_user_add_fun(Row) ->
    fun() ->
            % The row doesn't have a prefix yet
            Prefix = db:find_unused_prefix(),
            CompleteRow = Row#device{prefix = Prefix},
            case mnesia:write(CompleteRow) of
                ok -> CompleteRow;
                _ ->
                    mnesia:abort("Failed adding new row")
            end
    end.
            
% Generates an unused device prefix string. Can only be called inside an
% mnesia transaction.
-spec find_unused_prefix() -> string().
find_unused_prefix() ->
    Prefix = rand:random_alnum(6),
    % See if prefix already exists in the database
    Query = make_prefix_lookup_q(Prefix),
    case qlc:e(Query) of
        [] ->
            % The prefix is unused, so use it
            Prefix;
        [_|_] ->
            % Prefix is used, try a different one
            log(debug, "Prefix ~p is used, trying a different one~n", [Prefix]),
            find_unused_prefix()
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
             
             