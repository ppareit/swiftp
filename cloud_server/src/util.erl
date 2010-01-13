-module(util).
-include("records.hrl").
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
% A shorter way to extract a value from a json term.
% The Object argument should have been returned from json_to_term().
get_json_value({JsonKeyVals}, Key) ->
    proplists:get_value(Key, JsonKeyVals).

-spec get_json_string(Json :: json_term(), Key :: binary()) -> any().
% A convenience function to call get_json_value using the given arguments
% and convert the return value to a string before returning it to the caller.
get_json_string(Json, Key) ->
    %log(debug, "Getting key ~p from ~p~n", [Key, Json]),
    binary_to_list(get_json_value(Json, Key)).


%% @spec open_data_port(Address, Port) -> {ok, Socket} | {error, Reason}
%%              Address = string() | atom() | ip_address()
%%              Port = 0..65535
open_data_port(Address, Port) ->
    gen_tcp:connect(Address, Port, [binary, 
                                    {packet, 0}, 
                                    {active, once}]).

% This is obsolete. Now we use login_noauth_json instead.
% Unpack a JSON request object requesting account creation, and pass on
% the unpacked arguments to the "db" module.
%-spec create_account_json(json_term()) -> 
%        {true, json_binary(), #device{}} | {false, json_binary(), none}.
%create_account_json({JsonKeyValList}) ->
%    case proplists:get_value(<<"android_id">>, JsonKeyValList) of
%        undefined ->
%            log(info, "create_account didn't contain android_id~n", []),
%            {false, 
%             json_err_obj(9, "android_id not present"),
%             none};
%        AndroidIdBin ->
%            AndroidId = binary_to_list(AndroidIdBin),
%            true = is_list(AndroidId),
%            true = length(AndroidId) > 1,
%            case db:create_account(AndroidId) of
%                {ok, DeviceRow} ->
%                    SecretAsBinary = list_to_binary(DeviceRow#device.secret),
%                    {true, term_to_json({[{<<"secret">>, SecretAsBinary}]}), DeviceRow};
%                {error, _} ->
%                    % The failure and reason will have been logged already by the db module
%                    Response = json_err_obj(11, "Invalid creation attempt or DB failure"),
%                    {false, Response, none}
%            end
%    end.
    
%% This is now obsolete. There is no authentication, and no secret data.
%-spec authenticate_json(json_term()) -> 
%                {true, json_binary(), #device{}} | {false, json_binary(), none}.
%authenticate_json({JsonKeyValList}) ->
%    case proplists:get_value(<<"android_id">>, JsonKeyValList) of
%        undefined ->
%            log(info, "authenticate request didn't contain android_id~n", []),
%            {false, json_err_obj(9, "android_id not present"), none};
%        AndroidIdBin ->
%            AndroidId = binary_to_list(AndroidIdBin),
%            case proplists:get_value(<<"secret">>, JsonKeyValList) of
%                undefined ->
%                    log(info, "authenticate request didn't contain secret~n", []),
%                    {false, json_err_obj(9, "secret not present"), none};
%                SecretBin ->
%                    Secret = binary_to_list(SecretBin),
%                    case db:authenticate(AndroidId, Secret) of
%                        {true, DeviceRow} ->
%                            % We return empty json object on success
%                            {true, term_to_json({[]}), DeviceRow};
%                        false ->
%                            log(info, "Invalid credentials in auth attempt~n", []),
%                            {false, json_err_obj(11, "Invalid authentication attempt"), none};
%                        error ->
%                            log(info, "DB error in auth attempt~n", []),
%                            {false, json_err_obj(0, "Database failure"), none}
%                    end
%            end
%    end.

%% As of January 2010, we don't do authentication any more. The client sends 
%% us an Android ID, and we send back a prefix, as long as their request is
%% well-formed.
-spec login_noauth_json(json_term()) -> 
    {true, json_binary(), #device{}} | {false, json_binary(), none}.
login_noauth_json({JsonKeyValList}) ->
    case proplists:get_value(<<"android_id">>, JsonKeyValList) of
        undefined ->
            log(info, "login request malformed (no android_id!)~n", []),
            {false, json_err_obj(9, <<"android_id not present">>), none};
        AndroidIdBin ->
            AndroidId = binary_to_list(AndroidIdBin),
            case db:create_or_login(AndroidId) of
                {ok, DeviceRow} ->
                    log(info, "AndroidId ~p logged in~n", [AndroidId]),
                    {true, term_to_json({[]}), DeviceRow};
                error ->
                    {false, json_err_obj(9, <<"Check AndroidId">>), none}
            end
    end.


%% Creates a frequently-used type of JSON object to report an error back to the
%% client, having the keys "error_code" and "error_string".
-spec json_err_obj(Code :: integer(), String :: string()) -> json_binary().
json_err_obj(Code, String) when is_integer(Code) and is_list(String) ->
    term_to_json({[{<<"error_code">>, Code}, 
                   {<<"error_string">>, list_to_binary(String)}]}).

-spec maxint(X::integer(), Y::integer()) -> integer().
maxint(X, Y) ->
    if 
        X > Y -> X;
        true  -> Y
    end.

log(Level, Format, Args) ->
    log:log(Level, ?MODULE, Format, Args).

