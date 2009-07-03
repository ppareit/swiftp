-module(rand).
-import(log, [log/2, log/3]).
-export([start/0, rand/1, random_alnum/1]).

% This is our interface to callers who want a random number.
rand(Modulus) ->
    random_thread ! {self(), Modulus},
    receive
        R -> R
    end.

% Generate a random alphanumeric string of the given length
% TODO: this could be much more efficient
random_alnum(Length) -> random_alnum(Length, []).
random_alnum(0, Accum) -> Accum;
random_alnum(Length, Accum) ->
    % There are 26+26+10=62 possible alphanumeric characters
    R = case rand:rand(62) of
        X when 1 =< X, X =< 10 -> 
            % Range 1 to 10 will be treated as digits
            % since ASCII integers start at 48, we add 48-1=44
            X+47;
        X when 11 =< X, X =< 36 ->
            % Range 11 to 36 will be treated as upper case letters
            % upper case letters start at ASCII 65, so add 65-11=54
            X+54;
        X when 37 =< X, X =< 62 ->
            % Range 37 to 62 will be treated as lower case letters
            % lower case letters start at ASCII 97, so add 97-37=60
            X+60
    end,
    random_alnum(Length-1, [R|Accum]).

start()->
    seed_randomizer(),
    main_loop().

main_loop() ->
    try
        receive
            {From, Modulus} -> 
                From ! random:uniform(Modulus);
            _ -> 
                ignore
        end,
        main_loop()
    catch
        X : Y -> log(error, "Exception in random_thread ~p/~p", [X, Y]),
        main_loop()
    after
        main_loop()
    end.
    
% Seed the randomizer with the current time
seed_randomizer() ->
    {X, Y, Z} = now(),
    random:seed(X, Y, Z).
