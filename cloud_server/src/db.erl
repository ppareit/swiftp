-module(db).
-include("records.hrl").
-export([create_master_schema/0, join_db/1]).
-import(log, [log/3]).

% Creates schema, should only be called on initial master node.
% Returns ok or throws something.
create_master_schema() ->
    ok = mnesia:delete_schema([node()]),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    ok = create_tables().

% The master list of tables and attributes
table_list() ->
    [{device, [{attributes, record_info(fields, device)},
               {disc_copies, [node()]},
               {record_name, device}]}].

create_tables() ->
    create_table_list(table_list()).
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
%% @spec ping_master() -> pong | pang
ping_master(Target) ->
    case Response = net_adm:ping(Target) of
        pong ->
            log(debug, "DB master responded, proceeding~n", []);
        pang ->
            log(error, "DB master is not responding~n", [])
    end,
    Response.

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
    

