-module(db).
-include("records.hrl").
-export([create_master_schema/0, join_db/1]).
-import(log, [log/3]).

% Creates schema, should only be called on initial master node.
% Returns either {error, Reason} or ok.
create_master_schema() ->
    case mnesia:create_schema([node()]) of
        Error = {error, Reason} ->
            log(error, "Master schema creation failed due to: ~p~n", [Reason]),
            Error;
        ok ->
            case mnesia:create_table(scratch, 
                                     [{attributes, record_info(fields, scratch)}]) of
                {aborted, Reason} ->
                    log(error, "Scratch table create failed due to: ~p~n", [Reason]),
                    {error, Reason};
                {atomic, ok} ->
                    case mnesia:create_table(device, 
                            [{attributes, record_info(fields, device)}]) of
                        {aborted, Reason} ->
                            log(error, "Device table create failed due to: ~p~n", [Reason]),
                            {error, Reason};
                        {atomic, ok} ->
                            ok
                    end
            end
    end.

join_db(Target) ->
    ok = mnesia:delete_schema([node()]),
    case rpc:call(Target, server, ping) of
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
