-module(db).
-include("records.hrl").
-export([create_master_schema/0, join_db/1]).
-import(log, [log/3]).

% Creates schema, should only be called on initial master node.
% Returns either {error, Reason} or ok.
create_master_schema() ->
    ok = mnesia:delete_schema([node()]),
    case mnesia:create_schema([node()]) of
        Error = {error, Reason} ->
            log(error, "Master schema creation failed due to: ~p~n", [Reason]),
            Error;
        ok ->
            ok = mnesia:start(),
            create_tables()
    end.

create_tables() ->
    TableList = [{scratch, [{attributes, record_info(fields, scratch)}]},
                  {device, [{attributes, record_info(fields, device)}]}],
    create_table_list(TableList).
create_table_list([]) ->
    ok;
create_table_list([{Name, Attributes} | Rest]) ->
    case mnesia:create_table(Name, Attributes) of
        {atomic, ok} ->
            create_table_list(Rest);
        {aborted, Reason} ->
            log(error, "Failed to create table ~p due to: ~p~n", [Name, Reason]),
            {error, Reason}
    end.

join_db(Target) ->
    ok = mnesia:delete_schema([node()]),
    case rpc:call(Target, server, ping, []) of
        {badrpc, X} ->
            log(error, "Couldn't connect to node ~p due to ~p~n", [Target, X]);
        ok ->
            MyNode = node(),
            % Request that the remote node add us to it's mnesia cluster
            case rpc:call(Target, mnesia, change_config, [extra_db_nodes, [MyNode]]) of
                Error = {error, Reason} ->
                    log(error, "Failed joining mnesia due to: ~p~n", [Reason]),
                    Error;
                {ok, _NodeList} ->
                    % TODO: convert from ram copies to disc copies
                    test_database()
            end
    end.

% Tries to insert a record into a scratch table as a test of the database
test_database() ->
    {error, "test_database not implemented"}.    
