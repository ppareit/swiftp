%% A gen_server that can provide random strings.
%%
%% This is a separate process because the state of the randomizer seems to
%% be maintained on a per-process basis. That means that either:
%%
%%  a) each process wanting a random number has to seed the randomizer
%%  b) we seed the randomizer in a designated process, and ask it for random numbers.
%%
%% This module implements approach (b), which seems cleanest.

-module(rand).
-behavior(gen_server).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).
-export([start_link/0, random_alnum/1]).

start_link() ->
    gen_server:start_link({local, random_thread}, ?MODULE, [], []).

%% The interface to other modules:
% This is our interface to callers who want a random string.
random_alnum(Length) ->
    gen_server:call(random_thread, {random_alnum, Length}).

init(_) ->
    seed_randomizer(),
    {ok, []}.

handle_call({random_alnum, Length}, _From, State) ->
    {reply, random_alnum_internal(Length), State}.
    
% Generate a random string of digits+lowercase of the given length
% TODO: this could be much more efficient
random_alnum_internal(Length) -> random_alnum_internal(Length, []).
random_alnum_internal(0, Accum) -> Accum;
random_alnum_internal(Length, Accum) ->
    % There are 26+26+10=62 possible alphanumeric characters
    R = case random:uniform(36) of
        X when 1 =< X, X =< 10 -> 
            % Range 1 to 10 will be treated as digits
            % since ASCII integers start at 48, we add 48-1=44
            X+47;
        X when 11 =< X, X =< 36 ->
            % Range 37 to 62 will be treated as lower case letters
            % lower case letters start at ASCII 97, so add 97-11=86
            X+86
    end,
    random_alnum_internal(Length-1, [R|Accum]).

% Seed the randomizer with the current time
seed_randomizer() ->
    {X, Y, Z} = now(),
    random:seed(X, Y, Z).

terminate(_Args, _State) -> ok.
handle_cast(_Request, State) ->  {noreply, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

log(Level, Format, Args) ->
    log:log(Level, ?MODULE, Format, Args).
