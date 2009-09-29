-module(checkout_session).
-include("records.hrl").
-behavior(gen_fsm).

-import(json_eep, [term_to_json/1, json_to_term/1]).
-compile(export_all).   % Because exporting each state function is a pain.
-export([code_change/4, handle_event/3, handle_info/3, handle_sync_event/4, init/1,
         terminate/3]).

% This module implements a TCP server that handles quota upgrade purchases. Users
% will buy upgrades using Google Checkout, which will call our Apache+mod_python
% server, which will in turn create a TCP connection here to:
%  - update the quota information in the database
%  - keep track of order id's and status

-record(state, {clientsocket}).
-define(SOCKET_TIMEOUT, 5000).
-define(CMD_TIMEOUT, 5000).
-define(BYTES_PER_GIG, 1073741824).

start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

init(_) ->
    {ok, state_fresh_start, [], ?SOCKET_TIMEOUT}.

% Called by tcp_listener. After we have spawned a device session and are
% waiting in state fresh_start, the tcp_listener will call this function
% using the Pid of the spawned thread to give it the accepted socket.
set_socket(Pid, Socket) ->
    gen_fsm:send_event(Pid, {socket, Socket}).


state_fresh_start({socket, ClientSocket}, _ ) ->
    % Receive TCP traffic as messages, unpackaged, in binary form
    ok = inet:setopts(ClientSocket, [{active, once}, {packet, 0}, binary]),
    {next_state, 
     state_waiting_command, 
     #state{clientsocket=ClientSocket}, 
     ?CMD_TIMEOUT}.

state_waiting_command({tcp, ClientSocket, Data}, State) ->
    %log(debug, "Got checkout command ~p~n", [Data]),
    Json = json_to_term(binary_to_list(Data)),
    Return = case util:get_json_value(Json, <<"event">>) of
        <<"check-android_id">> ->
            AndroidId = util:get_json_string(Json, <<"android_id">>),
            check_android_id(AndroidId);
        <<"new-order-notification">> ->
            OrderNum = util:get_json_string(Json, <<"google-order-number">>),
            AndroidId = util:get_json_string(Json, <<"android_id">>),
            BuyerId = util:get_json_string(Json, <<"buyer-id">>),
            OrderTotal = util:get_json_string(Json, <<"order-total">>),
            TimeStamp = util:get_json_string(Json, <<"timestamp">>),
            TransferCredit = util:get_json_value(Json, <<"transfer-credit">>),
            true = is_integer(TransferCredit),
            new_order(OrderNum, AndroidId, BuyerId, OrderTotal, TimeStamp, TransferCredit);
        <<"order-state-change-notification">> ->
            OrderNum = util:get_json_string(Json, <<"google-order-number">>),
            OrderState = util:get_json_string(Json, <<"new-financial-order-state">>),
            TimeStamp = util:get_json_string(Json, <<"timestamp">>),
            order_state_change(OrderNum, OrderState, TimeStamp);
        <<"charge-amount-notification">> ->
            OrderNum = util:get_json_string(Json, <<"google-order-number">>),
            LatestAmt = util:get_json_string(Json, <<"latest-charge-amount">>),
            TotalAmt = util:get_json_string(Json, <<"total-charge-amount">>),
            TimeStamp = util:get_json_string(Json, <<"timestamp">>),
            order_charged(OrderNum, LatestAmt, TotalAmt, TimeStamp);
        <<"chargeback-amount-notification">> ->
            OrderNum = util:get_json_string(Json, <<"google-order-number">>),
            LatestAmt = util:get_json_string(Json, <<"latest-chargeback-amount">>),
            TotalAmt = util:get_json_string(Json, <<"total-chargeback-amount">>),
            TimeStamp = util:get_json_string(Json, <<"timestamp">>),
            order_charged_back(OrderNum, LatestAmt, TotalAmt, TimeStamp);
        Other ->
            log(warn, "Unrecognized checkout message ~p~n", Other)
    end,
    gen_tcp:send(ClientSocket, term_to_json(Return)),
    gen_tcp:close(ClientSocket),
    {stop, normal, State}.
            
-spec new_order(OrderNum::string(), AndroidId::string(), BuyerId :: string(), 
        OrderTotal::string(), TimeStamp::string(), TransferCredit::integer()) -> {list()}.
new_order(OrderNum, AndroidId, BuyerId, OrderTotal, TimeStamp, TransferCredit)
  when is_integer(TransferCredit) ->
    case db:insert_order_row(OrderNum, AndroidId, BuyerId, OrderTotal, 
                             TimeStamp, TransferCredit) of
        ok ->
            log(info, "New order notification processed OK~n", []),
            {[]}; % empty JSON object means success
        bad_android_id ->
            log(warn, "Failed new order processing, nonexistent android_id~n", []),
            {[{<<"error_code">>, 10}, {<<"error_string">>, <<"Nonexistent android_id">>}]};
        failed ->
            {[{<<"error_code">>, 1}, {<<"error_string">>, <<"Failed order row insertion">>}]}
    end.
        
order_state_change(OrderNum, OrderState, TimeStamp) ->
    case db:order_state_change(OrderNum, OrderState, TimeStamp) of
        ok ->
            log(info, "Success changing order ~p to state ~p~n", [OrderNum, OrderState]),
            {[]}; % empty JSON object indicates success
        failed ->
            log(warn, "Failed changing order ~p to state ~p~n", [OrderNum, OrderState]),
            {[{<<"error_code">>, 1}, {<<"error_string">>, <<"Couldn't update order row">>}]}
    end.
        
order_charged(OrderNum, LatestAmt, TotalAmt, TimeStamp) ->
    % There's an unlikely race condition here, where a single order may have its quota
    % applied twice, if there are two quick successive calls to this function. This
    % is not a problem worth solving.
    case db:order_charged(OrderNum, LatestAmt, TotalAmt, TimeStamp) of
        {payment_complete, AndroidId, TransferGigs} ->
            InfinityValue = quota:infinite_quota_constant(),
            AlreadyApplied = db:check_order_applied(OrderNum),
            Success = if 
                AlreadyApplied ->
                    % For a single order, only grant the quota increase once
                    log(warn, "Won't grant quota increase twice for order ~p~n", [OrderNum]),
                    ok;
                TransferGigs == InfinityValue ->
                    % There's a special value meaning to grant infinite quota to this device
                    case quota:grant_infinite_quota(AndroidId) of
                        ok -> db:mark_order_applied(OrderNum);
                        _  -> ok
                    end;
                true ->
                    case quota:increase_quota(AndroidId, TransferGigs*?BYTES_PER_GIG) of
                        ok -> db:mark_order_applied(OrderNum);
                        _  -> ok
                    end
            end,
            case Success of 
                ok ->
                    log(info, "Charged order ~p, successfully increased quota for ~p by ~p GB~n", 
                        [OrderNum, AndroidId, TransferGigs]),
                    {[]};
                error ->
                    log(warn, "Error increasing quota after payment~n", []),
                        {[{<<"error_code">>, 1}, {<<"error_string">>, <<"DB error">>}]}
            end;
        ok ->
            log(warn, "Charge successful on order ~p for incomplete amount~n", [OrderNum]),
            {[]};
        failed ->
            log(warn, "Couldn't note charge on order ~p~n", [OrderNum]),
            {[{<<"error_code">>, 1}, {<<"error_string">>, 
                                      <<"Couldn't process order charge status">>}]}
    end.

order_charged_back(OrderNum, _LatestAmt, _TotalAmt, _TimeStamp) ->
    log(warn, "Chargeback on order ~p~n", [OrderNum]),
    case db:look_up_device_by_order(OrderNum) of
        AndroidId when is_list(AndroidId) ->
            quota:disable_device(AndroidId),
            {[]};
        _ ->
            log(warn, "Error looking up android_id in chargeback processing", []),
            {[{<<"error_code">>, 1}, {<<"error_string">>, <<"Failed lookup">>}]}
    end.

-spec check_android_id(Android :: string()) -> {list()}.
check_android_id(AndroidId) ->
    case db:check_android_id_exists(AndroidId) of % returns exists | no_exists | error
        exists ->
            {[{<<"existence">>, <<"true">>}]};
        no_exists ->
            {[{<<"existence">>, <<"false">>}]}
    end.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_sync_event(_Event, _From, StateName, StateData) ->
    {next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
    ok.

log(Level, Format, Args) ->
    log:log(Level, ?MODULE, Format, Args).

% Take TCP data as an Erlang message and dispatch it to the current state function                   
handle_info({tcp, Socket, Data}, StateName, StateData) ->
    % Now that we've received the latest TCP message, enable receipt
    % of the next message. (It's actually received by the gen_fsm and
    % forwarded to this function.
    inet:setopts(Socket, [{active, once}]),
    % The following call will return a {next_state, ...} tuple as required.
    ?MODULE:StateName({tcp, Socket, Data}, StateData);
% If any open socket is closed, close all sockets and terminate this process
% It's unclear what the difference is between tcp_close and tcp_closed, we handle
% them identically.
handle_info({tcp_close, Socket}, StateName, StateData) -> 
    handle_info({tcp_closed, Socket}, StateName, StateData);
handle_info({tcp_closed, _Socket}, _StateName, StateData) ->
    log(info, "Socket closed, terminating~n", []),
    {stop, normal, StateData};
handle_info({tcp_error, _Socket, Reason}, _StateName, StateData) ->
    log(info, "TCP socket error, exiting normally: ~p~n", [Reason]),
    {stop, normal, StateData};
handle_info(Info, StateName, StateData) ->
    log(warn, "Unrecognized info in device_session: ~p~Dn", [Info]),
    {noreply, StateName, StateData}.
