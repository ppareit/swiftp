-module(rand).
-import(log, [log/2, log/3]).
-export([start/0, rand/1]).

% This is our interface to callers who want a random number.
rand(Modulus) ->
    random_thread ! {self(), Modulus},
    receive
        R -> R
    end.

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
