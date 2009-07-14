-module(util).
-include("records.hrl").
-import(log, [log/3]).
-import(json_eep, [json_to_term/1, term_to_json/1]).

-compile(export_all).

% Start listening on the given port and return the opened socket. Return
% conventions are the same as for gen_tcp:listen: either {ok, Socket} or
% {error, Reason}. Listening on port 0 will pick a random port.
tcp_listen(Port) -> 
    gen_tcp:listen(Port, [binary,
                          {reuseaddr, true}, 
                          {active, true}]).

% Given two sockets, pass data between them until one of them is closed,
% or Timeout milliseconds pass without data being sent.
%% proxy_until_close(Socket1, Socket2, Timeout) ->
%%     receive
%%         {tcp, Socket, Data} ->
%%             case Socket of
%%                 Socket1 ->
%%                     gen_tcp:send(Socket2, Data);
%%                 Socket2 ->
%%                     gen_tcp:send(Socket1, Data)
%%             end;
%%         {tcp_closed, _} ->
%%             ok
%%     after Timeout ->
%%         log(info, "proxy_until_close returning due to timeout~n", [])
%%     end.

-spec get_json_value(Object :: json_term(), Key :: binary()) -> any().
get_json_value({JsonKeyVals}, Key) ->
    Value = proplists:get_value(Key, JsonKeyVals),
%%     log(debug, "get_json_value: Looking up ~p in ~p gave ~p~n", [Key,
%%                                                                  JsonKeyVals,
%%                                                                  Value]),
    Value.
    

%% @spec open_data_port(Address, Port) -> {ok, Socket} | {error, Reason}
%%              Address = string() | atom() | ip_address()
%%              Port = 0..65535
open_data_port(Address, Port) ->
    gen_tcp:connect(Address, Port, [binary, 
                                    {packet, 0}, 
                                    {active, once}]).

% Unpack a JSON request object requesting account creation, and pass on
% the unpacked arguments to the "db" module.
-spec create_account_json(json_term()) -> 
        {true, json_binary(), #device{}} | {false, json_binary(), none}.
create_account_json({JsonKeyValList}) ->
    case proplists:get_value(<<"android_id">>, JsonKeyValList) of
        undefined ->
            log(info, "create_account didn't contain android_id~n", []),
            {false, 
             json_err_obj(9, "android_id not present"),
             none};
        AndroidIdBin ->
            AndroidId = binary_to_list(AndroidIdBin),
            true = is_list(AndroidId),
            true = length(AndroidId) > 1,
            case db:create_account(AndroidId) of
                {ok, DeviceRow} ->
                    SecretAsBinary = DeviceRow#device.secret,
                    {true, term_to_json({[{<<"secret">>, SecretAsBinary}]}), DeviceRow};
                {error, _} ->
                    % The failure and reason will have been logged already by the db module
                    {false, json_err_obj(0, "Database failure"), none}
            end
    end.
    
-spec authenticate_json(json_term()) -> 
                {true, json_binary(), #device{}} | {false, json_binary(), none}.
authenticate_json({JsonKeyValList}) ->
    case proplists:get_value(<<"android_id">>, JsonKeyValList) of
        undefined ->
            log(info, "authenticate request didn't contain android_id~n", []),
            {false, json_err_obj(9, "android_id not present"), none};
        AndroidIdBin ->
            AndroidId = binary_to_list(AndroidIdBin),
            case proplists:get_value(<<"secret">>, JsonKeyValList) of
                undefined ->
                    log(info, "authenticate request didn't contain secret~n", []),
                    {false, json_err_obj(9, "secret not present"), none};
                SecretBin ->
                    Secret = binary_to_list(SecretBin),
                    case db:authenticate(AndroidId, Secret) of
                        {true, DeviceRow} ->
                            % We return empty json object on success
                            {true, term_to_json({[]}), DeviceRow};
                        false ->
                            {false, json_err_obj(11, "Invalid authentication attempt"), none};
                        error ->
                            {false, json_err_obj(0, "Database failure"), none}
                    end
            end
    end.
                        

%% Creates a frequently-used type of JSON object to report an error back to the
%% client, having the keys "error_code" and "error_string".
-spec json_err_obj(Code :: integer(), String :: string()) -> json_binary().
json_err_obj(Code, String) when is_integer(Code) and is_list(String) ->
    term_to_json({[{<<"error_code">>, Code}, 
                   {<<"error_string">>, list_to_binary(String)}]}).