-module(swiftp_proxy.security).
-export([auth_or_create_account/1]).
-import(swiftp_proxy.log, [log/2, log/3]).
-import(swiftp_proxy.rand, [random_alnum/1]).
-import(jsonparse.json_eep, [term_to_json/1, json_to_term/1]).

auth_or_create_account(Socket) ->
    receive
        {tcp, Socket, Data} ->
            Json = json_to_term(binary_to_list(Data)),
            log(debug, "parsed json: ~p~n", [Json]),
            {JsonKeyVals} = Json,  % unpack the outer tuple, ending up with
                                   % a list of key/value tuples
            log(debug, "JsonKeyVals: ~p~n", [JsonKeyVals]),
            case proplists:get_value(<<"action">>, JsonKeyVals) of
                <<"authenticate">> ->
                    {WhetherSuccess, Response} = authenticate_json(Json),
                    gen_tcp:send(Socket, Response),
                    WhetherSuccess;
                <<"create_account">> ->
                    {WhetherSuccess, Response} = create_account_json(Json),
                    gen_tcp:send(Socket, Response),
                    WhetherSuccess;
                OtherAction ->
                    log(info, "Expected authentication but got action: ~p~n", [OtherAction])
            end;
        {tcp_closed, Socket} ->
            log(info, "Authenticate got tcp_closed~p~n", []),
            false
    after 5000 ->
        log(info, "Authenticate timed out~n", []),
        false
    end.

% Unpack and execute an incoming JSON "authenticate" action
authenticate_json({JsonKeyValList}) ->
    case proplists:get_value(<<"android_id">>, JsonKeyValList) of
        undefined -> 
            log(info, "authenticate request didn't contain android_id~n", []),
            {false, term_to_json({[{9, <<"android_id not present">>}]})};
        AndroidId -> 
            log(info, "Stub authenticate_json always allowing. android_id ~p~n", 
                [AndroidId]),
            {true, term_to_json({[]})}  % empty object returned on successful auth
    end.
    
% Unpack and execute an incoming JSON "create_account" action
% Returns a response {true, JsonString} on success, {false, JsonString} on failure
create_account_json({JsonKeyValList}) ->
    case proplists:get_value(<<"android_id">>, JsonKeyValList) of
        undefined ->
            log(info, "create_account didn't contain android_id~n", []),
            {false, term_to_json({[{9, "android_id not present"}]})};
        AndroidId ->
            Secret = list_to_binary(generate_secret(AndroidId)),
            {true, term_to_json({[{"secret", Secret}]})}
    end.
    
    
generate_secret(_AndroidId)->
    log(info, "Stub generate_secret not actually persisting~n", []),
    random_alnum(16).
    
