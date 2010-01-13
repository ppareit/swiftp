-record(device, {android_id, prefix, creation_time, last_login, queued_actions}).
%-record(order, {order_num, android_id, buyer_id, order_total, timestamp, state, 
%                total_charged, transfer_credit, manual_intervention, applied}).

-type json_term()  ::  {[any()]}.  % Approximately, TODO: be more specific 

%-type json_binary() :: binary().
-type json_binary() :: [byte()].
