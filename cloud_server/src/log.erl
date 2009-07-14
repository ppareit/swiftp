-module(log).
-behavior(gen_server).
-export([start_link/3, log/3, init/1, terminate/2, code_change/3, handle_call/3,
         handle_info/2, handle_cast/2]).

-record(state, {fh, directory, file, threshold}).
-record(log_msg, {pid, level, time, message, args}).

% TODO: implement logger process at each node that stores threshold, to eliminate
% extraneous inter-node traffic

start_link(Directory, File, Threshold) ->
    gen_server:start_link({global, logger}, 
                          ?MODULE, 
                          [Directory, File, Threshold],
                          []).

init([Directory, File, Threshold]) ->
    case file:open(Directory ++ "/" ++ File, [append]) of
        {ok, Fh} ->
            State = #state{fh=Fh, 
                           directory=Directory, 
                           file=File, 
                           threshold=Threshold},
            log(info, "Logger running!~n", []),
            {ok, State};
        X ->
            io:format("log:init() failed to open log file: ~p~n", [X]),
            {stop, "log file open failure"}
    end.

%% @spec main_loop(Fh::iodevice(), Directory::string(), File::string(), Threshold::atom())
%%                 -> ok
%% main_loop(Fh, Directory, File, Threshold) ->
%%     receive
%%         {Pid, log_message, MsgLevel, Timestamp, Message, Args} ->
%%             case numeric_level(MsgLevel) >= numeric_level(Threshold) of 
%%                 true ->
%%                     % TODO: use delayed write for performance
%%                     {{Year,Month,Day},{Hour,Minute,Second}} = Timestamp,
%%                     io:format(Fh, "~5p|~7p|~p/~p/~p,~2p:~2p:~2p|" ++ Message, 
%%                               [MsgLevel | [Pid | 
%%                               [ Year | [Month | [Day | [Hour | [Minute | [Second | 
%%                               Args]]]]]]]] ),
%%                     main_loop(Fh, Directory, File, Threshold);
%%                 false ->
%%                     io:format("Main loop, level ~p not >= ~p~n", [MsgLevel, Threshold]),
%%                     % Don't log, because the log level is insufficient
%%                     main_loop(Fh, Directory, File, Threshold)
%%             end;
%%         {_Pid, change_file, NewDirectory, NewFile} ->
%%             file:close(Fh),
%%             logger_thread_start(NewDirectory, NewFile, Threshold);
%%         {_Pid, change_threshold, NewThreshold} ->
%%             log(info, "Changing logging threshold to: ~p~n", [NewThreshold]),
%%             main_loop(Fh, Directory, File, NewThreshold);
%%         {Pid, quit} ->
%%             log(info, "Exiting because pid ~p is taking over logging duties.~n", [Pid]),
%%             ok;
%%         Other ->
%%             io:format("Unexpected message to logger: ~p~n", [Other]),
%%             main_loop(Fh, Directory, File, Threshold)
%%     end.

handle_cast(#log_msg{pid=Pid, level=Level, time=Time, message=Message, args=Args},
            State = #state{fh=Fh, threshold=Threshold}) ->
    case numeric_level(Level) >= numeric_level(Threshold) of 
        true ->
            % TODO: use delayed write for performance
            {{Year,Month,Day},{Hour,Minute,Second}} = Time,
            io:format(Fh, "~5p|~7p|~p/~p/~p,~2p:~2p:~2p|" ++ Message, 
                      [Level | [Pid | 
                      [ Year | [Month | [Day | [Hour | [Minute | [Second | 
                      Args]]]]]]]] );
        false -> 
            % Don't log, because the log level is insufficient
            ok
    end,
    {noreply, State}.


% Send a "cast" to the globally registered logger process if it exists,
% otherwise print to standard output.
log(Level, Message, Args) ->
    case global:whereis_name(logger) of
        undefined ->
            io:format("No logger for msg: ", []),
            io:format(Message, Args),
            {error, "No logger registered"};
        _X ->
            Timestamp = erlang:localtime(),
            gen_server:cast({global, logger}, #log_msg{level=Level, 
                                                       time=Timestamp, 
                                                       message=Message, 
                                                       args=Args})
    end.

-spec numeric_level(atom()) -> integer().
numeric_level(error) -> 5;
numeric_level(warn)  -> 4;
numeric_level(info)  -> 3;
numeric_level(debug) -> 2;
numeric_level(trace) -> 1.


% Callbacks for gen_event, which lets us log events originating within the Erlang runtime
terminate(_Args, _State) -> ok.
handle_call(_Request, _From, State) ->  {noreply, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

