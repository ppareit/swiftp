-module(swiftp_proxy.log).
-export([start/4, log/3, logger_thread_start/3]).

% A wrapper for log() providing a default log level of "info"
%log(Message) -> log(info, Message).

% A wrapper for log() providing a default empty argument list
%log(Level, Message) -> log(Level, Message, []).

%% @spec start(_Directory::string(), _Filename::string(), _Threshold::atom(), int())
%%             -> atom().
start(_Directory, _Filename, _Threshold, 0) ->
    out_of_tries;
%% @spec start(Directory::string(), Filename::string(), Threshold::atom(), Tries::int()) 
%%             -> ok
start(Directory, Filename, Threshold, Tries) ->
    process_flag(trap_exit, true),
    ChildPid = spawn_link(?MODULE, logger_thread_start, [Directory, Filename, Threshold]),
    timer:sleep(10000),  % Only retry every 10 seconds
    receive
        {'EXIT', ChildPid, normal} ->
            ok;
        {'EXIT', ChildPid, _Reason} ->
            start(Directory, Filename, Threshold, Tries - 1)
    end.
        
%% @spec logger_thread_start(Directory::string(), File::string(), Threshold::atom())
%%                           -> none()
logger_thread_start(Directory, File, Threshold) ->
    become_cluster_logger(),
    case file:open(Directory ++ "/" ++ File, [append]) of
        {ok, Fh} ->
            io:format("Log file open~n", []),
            main_loop(Fh, Directory, File, Threshold);
        X ->
            io:format("Failed opening log file: ~p~n", [X]),
            exit(file_open_failure)
    end.

%% @spec main_loop(Fh::iodevice(), Directory::string(), File::string(), Threshold::atom())
%%                 -> ok
main_loop(Fh, Directory, File, Threshold) ->
    receive
        {Pid, log_message, MsgLevel, Timestamp, Message, Args} ->
            case numeric_level(MsgLevel) >= numeric_level(Threshold) of 
                true ->
                    % TODO: use delayed write for performance
                    {{Year,Month,Day},{Hour,Minute,Second}} = Timestamp,
                    io:format(Fh, "~5p|~7p|~p/~p/~p,~2p:~2p:~2p|" ++ Message, 
                              [MsgLevel | [Pid | 
                              [ Year | [Month | [Day | [Hour | [Minute | [Second | 
                              Args]]]]]]]] ),
                    main_loop(Fh, Directory, File, Threshold);
                false ->
                    io:format("Main loop, level ~p not >= ~p~n", [MsgLevel, Threshold]),
                    % Don't log, because the log level is insufficient
                    main_loop(Fh, Directory, File, Threshold)
            end;
        {_Pid, change_file, NewDirectory, NewFile} ->
            file:close(Fh),
            logger_thread_start(NewDirectory, NewFile, Threshold);
        {_Pid, change_threshold, NewThreshold} ->
            log(info, "Changing logging threshold to: ~p~n", [NewThreshold]),
            main_loop(Fh, Directory, File, NewThreshold);
        {Pid, quit} ->
            log(info, "Exiting because pid ~p is taking over logging duties.~n", [Pid]),
            ok;
        Other ->
            io:format("Unexpected message to logger: ~p~n", [Other]),
            main_loop(Fh, Directory, File, Threshold)
    end.

%% @spec become_cluster_logger() -> yes | no
become_cluster_logger() ->
    MyPid = self(),
    case global:whereis_name(logger) of
        undefined -> ok;
        MyPid -> ok;
        OtherPid when is_pid(OtherPid) ->
            % Tell the existing logger process to quit, if it exists
            OtherPid ! {self(), quit}
    end,
    MyPid = self(),
    ChooseSelfResolver = fun() -> MyPid end,
    case Success = global:register_name(logger, self(), ChooseSelfResolver) of
        yes ->
            log(info, "I (~p) took over logger duties.~n", [node()]);
        no ->
            log(error, "I (~p) failed to take over logger duties.~n", [node()])
    end,
    Success.

%% @spec log(Level::atom(), Message::string(), Args::list() -> ok | {error, Reason}
log(Level, Message, Args) when is_atom(Level), is_list(Message), is_list(Args) ->
    case global:whereis_name(logger) of
        undefined ->
            io:format("No logger for msg: ~p~n", [Message]),
            {error, "No logger registered"};
        LoggerPid ->
            %{{Year,Month,Day},{Hours,Minutes,Seconds}} = erlang:localtime(),
            %Timestamp = lists:flatten(io_lib:format("~p/~p/~p,~p:~p:~p",
            %                    [Year, Month, Day, Hours, Minutes, Seconds])),
            Timestamp = erlang:localtime(),
            LoggerPid ! {self(), log_message, Level, Timestamp, Message, Args},
            ok
    end.
    
%% @spec numeric_level(atom()) -> int()
numeric_level(error) -> 5;
numeric_level(warn)  -> 4;
numeric_level(info)  -> 3;
numeric_level(debug) -> 2;
numeric_level(trace) -> 1.
