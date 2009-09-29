%%% -------------------------------------------------------------------
%%% Description : Implements a simple_one_for_one supervisor that spawns 
%%%               checkout session threads.
%%% -------------------------------------------------------------------
-module(checkout_session_sup).

-behaviour(supervisor).

-export([
	 init/1
        ]).

-define(MAX_RESTART, 100).
-define(MAX_TIME, 1).  % Never give up, keep restarting children forever

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================



%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init([]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              {   undefined,                        % Id       = internal id
                  {checkout_session,start_link, []},  % StartFun = {M, F, A}
                  temporary,                        % Restart  = permanent | transient | temporary
                  2000,                             % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                           % Type     = worker | supervisor
                  []                                % Modules  = [Module] | dynamic
              }
            ]
        }
    }.
