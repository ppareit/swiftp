-record(device, {android_id, prefix, secret, creation_time, last_login}).
-type json_term()  ::  {[any()]}.  % Approximately, TODO: be more specific 

%-type json_binary() :: binary().
-type json_binary() :: [byte()].