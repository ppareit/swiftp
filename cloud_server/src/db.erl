-module(db).
-include("records.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([create_master_schema/0, join_db/1, authenticate/2, create_account/1,
         reuse_schema/0, all_devices/0, clear_devices/0]).
-export([make_prefix_lookup_q/1, get_device_row_dirty/1, update_quota_usage/2, 
         add_to_quota/2, check_android_id_exists/1,insert_order_row/6, 
         look_up_device_by_order/1, order_charged/4, order_state_change/3, 
         set_device_quota/2, write_device_row/1, get_all_orders/0,
         mark_order_applied/1, check_order_applied/1]).
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
               {record_name, device}]},
     {order, [{attributes, record_info(fields, order)},
                 {disc_copies, [node()]},
                 {record_name, order}]}].

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

-spec authenticate(AndroidId :: string(), Secret :: string()) -> {true, #device{}}
                                                               | false
                                                               | error.
authenticate(AndroidId, Secret) ->
    log(debug, "Trying to auth with ~p/~p~n", [AndroidId, Secret]),
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
                        last_login = NowTime,
                        quota=quota:initial_quota(),
                        totalbytes=0},
    AddFun = make_user_add_fun(DeviceRow),
    case mnesia:transaction(AddFun) of
        {atomic, CompleteRow} when is_record(CompleteRow, device) ->
            log(info, "Added device ~p to database~n", [AndroidId]),
            {ok, CompleteRow}; % Now the prefix has been set
        {aborted, already_exists} ->
            log(info, "Tried to create account for existing android_id ~p~n", [AndroidId]),
            {error, already_exists};
        ErrReason ->
            log(error, "Failed adding device due to ~p~n", [ErrReason]),
            {error, database_failed}
    end.

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

-spec update_quota_usage(AndroidId::string(), TotalBytes::integer()) -> ok.
update_quota_usage(AndroidId, TotalBytes) ->
    Fun = fun() ->
        case qlc:e(qlc:q([X || X <- mnesia:table(device),
                               AndroidId == X#device.android_id])) of
            [Row] ->
                if
                    TotalBytes >= Row#device.totalbytes ->
                        NewRow = Row#device{totalbytes=TotalBytes},
                        mnesia:write(NewRow),
                        ok;
                    true ->
                        % If the DB already has a higher usage count, leave it
                        % This shouldn't happen
                        wont_decrease
                end;
            _ ->
                % We failed to find the DB row, not much we can do here
                no_row
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} ->
            ok;
        {atomic, Reason} ->
            log(warn, "Failed updating quota for ~p: ~p~n", [AndroidId, Reason]);
        {aborted, Reason} ->
            log(warn, "Quota DB transaction failed: ~p~n", [Reason])
    end,
    ok.

clear_devices() ->
    log(info, "Clearing device table~n", []),
    mnesia:clear_table(device).
             
insert_order_row(OrderNum, AndroidId, BuyerId, OrderTotal, TimeStamp, TransferCredit) ->
    % Make sure we have a device with the given Android ID
    case get_device_row_dirty(AndroidId) of
        no_exist ->
            bad_android_id;
        _ ->
            Row = #order{order_num=OrderNum, android_id=AndroidId, buyer_id=BuyerId,
                        order_total=OrderTotal, timestamp=TimeStamp, state="",
                        transfer_credit=TransferCredit, total_charged="0"},
            Fun = fun() -> mnesia:write(Row) end,
            case mnesia:transaction(Fun) of
                {atomic, ok} ->
                    ok;
                {aborted, Reason} ->
                    log(error, "Failed to insert new order row: ~p~n", [Reason]),
                    failed
            end
    end.

order_state_change(OrderNum, OrderState, TimeStamp) ->
    Fun = fun() ->
        case qlc:e(qlc:q([R || R <- mnesia:table(order),
                               R#order.order_num == OrderNum])) of
            [Row] ->
                NewRow = Row#order{state=OrderState, timestamp=TimeStamp},
                mnesia:write(NewRow);
            _ ->
                no_row
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} ->
            ok;
        {atomic, no_row} ->
            log(warn, "Couldn't update state of nonexistent order row~n", []),
            failed;
        {aborted, Reason} ->
            log(warn, "Order row state change failed", [Reason]),
            failed
    end.

add_to_quota(AndroidId, AddBytes) ->
    Fun = fun() ->
        case qlc:e(qlc:q([D || D <- mnesia:table(device),
                               D#device.android_id == AndroidId])) of
            [OldDevice] ->
                NewQuota = OldDevice#device.quota + AddBytes,
                NewDevice = OldDevice#device{quota=NewQuota}, 
                mnesia:write(NewDevice);
            _ ->
                mnesia:abort(failed)
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, _} ->
            ok;
        _ ->
            error
    end.

order_charged(OrderNum, _LatestAmt, TotalAmt, TimeStamp) ->
    Fun = fun() ->
        case qlc:e(qlc:q([R || R <- mnesia:table(order),
                               R#order.order_num == OrderNum])) of
            [OldOrder] ->
                NewOrder = OldOrder#order{total_charged=TotalAmt, timestamp=TimeStamp},
                mnesia:write(NewOrder),
                if 
                    TotalAmt >= NewOrder#order.order_total ->
                        % Charging has completed for this order, grant the quota credit
                        % By adding transfer_credit gigabytes to their quota
                        TransferCredit = NewOrder#order.transfer_credit,
                        AndroidId = NewOrder#order.android_id,
                        {payment_complete, AndroidId, TransferCredit};
                    true ->
                        ok
                end
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} ->
            ok;
        {atomic, T = {payment_complete, _, _} } ->
            T;
        _ ->
            failed
    end.
                

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

look_up_device_by_order(OrderNum) ->
    Fun = fun() ->
        case qlc:e(qlc:q([O || O <- mnesia:table(order),
                               O#order.order_num == OrderNum])) of
            [OrderRow] ->
                OrderRow#order.android_id;
            _ ->
                mnesia:abort(failed)
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, AndroidId} ->
            AndroidId;
        _ ->
            error
    end.
        
set_device_quota(AndroidId, Value) ->
    Fun = fun() ->
        case qlc:e(qlc:q([D || D <- mnesia:table(device),
                               D#device.android_id == AndroidId])) of
            [OldDevice] ->
                NewDevice = OldDevice#device{quota=Value},
                mnesia:write(NewDevice);
            _ ->
                mnesia:abort(failed)
        end
    end,
    case mnesia:transaction(Fun) of
        {atomic, _ } ->
            ok;
        _ ->
            error
    end.

% Mostly for debugging purposes, can be called from console
get_all_orders() ->
    F = fun() -> 
        qlc:e(qlc:q([R || R <- mnesia:table(order)]))
    end,
    mnesia:transaction(F).

-spec mark_order_applied(OrderNum :: string()) -> ok | error.
mark_order_applied(OrderNum) ->
    % An order is "applied" if the transfer credits have been added to the device quota.
    F = fun() ->
        case qlc:e(qlc:q([O || O <- mnesia:table(order),
                               O#order.order_num == OrderNum])) of
            [Order] ->
                NewOrder = Order#order{applied = true},
                mnesia:write(NewOrder);
            _ ->
                mnesia:abort(failed)
        end
    end,
    case mnesia:transaction(F) of
        {atomic, _} -> 
            ok;
        _  -> 
            log(warn, "Error marking order applied: ~p~n", [OrderNum]),
            error
    end.
    
-spec check_order_applied(OrderNum::string()) -> true | false | not_present | error.
check_order_applied(OrderNum) ->
    F = fun() ->
        case qlc:e(qlc:q([O || O <- mnesia:table(order),
                               O#order.order_num == OrderNum])) of
            [Order] ->
                if
                    % If this order has applied==true, return true
                    Order#order.applied ->
                        true;
                    % Otherwise return false
                    true ->
                        false
                end;
            _ ->
                not_present
        end
    end,
    case mnesia:transaction(F) of
        {atomic, X} ->
            X;
        _ ->
            error
    end.
    

log(Level, Format, Args) ->
    log:log(Level, ?MODULE, Format, Args).

