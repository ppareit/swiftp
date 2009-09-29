-module(log).
-behavior(gen_server).
-export([start_link/3, log/4, init/1, terminate/2, code_change/3, handle_call/3,
         handle_info/2, handle_cast/2]).
-record(state, {fh, directory, file, threshold}).
-record(log_msg, {pid, level, module, time, message, args}).

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
            log(info, ?MODULE, "Logger running!~n", []),
            {ok, State};
        X ->
            io:format("log:init() failed to open log file: ~p~n", [X]),
            {stop, "log file open failure"}
    end.

handle_cast(#log_msg{pid=Pid, module=Module, level=Level, time=Time, 
                     message=Message, args=Args},
            State = #state{fh=Fh, threshold=Threshold}) ->
    case numeric_level(Level) >= numeric_level(Threshold) of 
        true ->
            ModuleString = atom_to_list(Module),
            {{Year,Month,Day},{Hour,Minute,Second}} = Time,
            io:format(Fh, "~5p|~7p|~p/~p/~p,~2p:~2p:~2p|" ++ ModuleString 
                     ++ "|" ++ Message, 
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
log(Level, Module, Message, Args) ->
    case global:whereis_name(logger) of
        undefined ->
            io:format("No logger for msg: ", []),
            io:format(Message, Args),
            {error, "No logger registered"};
        _X ->
            Timestamp = erlang:localtime(),
            gen_server:cast({global, logger}, #log_msg{level=Level,
                                                       module=Module, 
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


terminate(_Args, _State) -> ok.
handle_call(_Request, _From, State) ->  {noreply, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

