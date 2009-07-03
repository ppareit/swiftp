%% Copyright (c) 2008 Robert Virding. All rights reserved.

%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:

%% 1. Redistributions of source code must retain the above copyright
%%    notice, this list of conditions and the following disclaimer.
%% 2. Redistributions in binary form must reproduce the above copyright
%%    notice, this list of conditions and the following disclaimer in the
%%    documentation and/or other materials provided with the distribution.

%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%% FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%% COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.

%%% File    : leex.erl
%%% Author  : Robert Virding (robert.virding@telia.com)
%%% Purpose : A Lexical Analyser Generator for Erlang.
%%%
%%% Most of the algorithms used here are taken pretty much as
%%% described in the "Dragon Book" by Aho, Sethi and Ullman. Some
%%% completing details were taken from "Compiler Design in C" by
%%% Hollub.

-module(leex).

-export([file/1,file/2,format_error/1]).

-import(lists, [member/2,reverse/1,seq/2,sort/1,
		keymember/3,keysearch/3,keysort/2,
		map/2,foldl/3,foldr/3,foreach/2]).
-import(string, [substr/2,substr/3,span/2,tokens/2]).
-import(ordsets, [is_element/2,add_element/2,union/2,subtract/2]).

%% -compile([export_all]).

-record(leex, {xfile=[],			%Xrl file
	       efile=[],			%Erl file
	       ifile=[],			%Include file
	       gfile=[],			%Graph file
	       module,				%Module name
	       opts=[],				%Options
	       errors=[],
	       warnings=[],
	       xport=none,			%Xrl file port
	       eport=none,			%Erl file port
	       iport=none,			%Include file port
	       gport=none			%Graph file port
	      }).

-record(nfa_state, {no,edges=[],accept=noaccept}).
-record(dfa_state, {no,nfa=[],trans=[],accept=noaccept}).

%% file(File) -> ok | error.
%% file(File, Options) -> ok | error.

file(File) -> file(File, []).

file(File, Opts) ->
    St0 = #leex{},
    St1 = filenames(File, Opts, St0),		%Get all the filenames
    try
	case parse_file(St1) of
	    {ok,REAs,Actions,Code} ->
		{NFA,NF} = build_combined_nfa(REAs),
		io:fwrite("NFA contains ~w states, ", [size(NFA)]),
		{DFA0,DF0} = build_dfa(NFA, NF),
		io:fwrite("DFA contains ~w states, ", [length(DFA0)]),
		{DFA,DF} = minimise_dfa(DFA0, DF0),
		io:fwrite("minimised to ~w states.~n", [length(DFA)]),
		%% io:fwrite("~p\n", [{NF,NFA}]),
		%% io:fwrite("~p\n", [{DF,DFA}]),
		Ret = out_file(St1, DFA, DF, Actions, Code),
		case member(dfa_graph, Opts) of
		    true -> out_dfa_graph(St1, DFA, DF);
		    false -> ok
		end,
		Ret;				%The important return!
	    {error,PError} ->
		io:put_chars([$\n,gcc_error(St1#leex.xfile, PError),$\n]),
		error
	end
    catch
	%% This is not used yet!
	throw: {leex_error,Error,St2} ->
	    close_files(St2),
	    io:put_chars([$\n,gcc_error(St2#leex.xfile, Error),$\n]),
	    error
    end.

%% filenames(File, Options, State) -> State.
%%  The default output dir is the current directory unless an
%%  explicit one has been given in the options.

filenames(File, Opts, St0) ->
    Dir = filename:dirname(File),
    Base = filename:basename(File, ".xrl"),
    Xfile = filename:join(Dir, Base ++ ".xrl"),
    Efile = Base ++ ".erl",
    Gfile = Base ++ ".dot",
    Ifile = "leexinc.hrl",
    Module = list_to_atom(Base),
    St1 = St0#leex{xfile=Xfile,
		   ifile=Ifile,
		   opts=Opts,
		   module=Module},
    %% Test for explicit out dir.
    case keysearch(outdir, 1, Opts) of
	{value,{outdir,D}} ->
	    St1#leex{efile=filename:join(D, Efile),
		     gfile=filename:join(D, Gfile)};
	_ ->
	    St1#leex{efile=Efile,gfile=Gfile}
    end.

close_files(St) ->
    MaybeClose = fun (none) -> ok; (Port) -> file:close(Port) end,
    MaybeClose(St#leex.xport),
    MaybeClose(St#leex.eport),
    MaybeClose(St#leex.iport),
    MaybeClose(St#leex.gport).

format_error({open,F}) -> ["error opening ",io_lib:write_string(F)];
format_error(missing_defs) -> "missing definitions";
format_error(missing_rules) -> "missing rules";
format_error(bad_rule) -> "bad rule";
format_error({regexp,E})-> ["bad regexp `",regexp:format_error(E),"'"];
format_error({after_regexp,S}) ->
    ["bad code after regexp ",io_lib:write_string(S)].

gcc_error(File, {Line,Mod,Error})                 ->
    io_lib:format("~s:~w: ~s", [File,Line,apply(Mod, format_error, [Error])]);
gcc_error(File, {Mod,Error})                      ->
    io_lib:format("~s: ~s", [File,apply(Mod, format_error, [Error])]).

%% parse_file(State) -> {ok,[REA],[Action],Code} | {error,Error}
%%  when
%%      REA = {RegExp,ActionNo};
%%      Action = {ActionNo,ActionString};
%%      Code = [char()].
%%
%%  Read and parse the file Xfile.
%%  After each section of the file has been parsed we directly call the
%%  next section. This is done when we detect a line we don't recognise
%%  in the current section. The file format is very simple and Erlang
%%  token based, we allow empty lines and Erlang style comments.

parse_file(St0) ->
    case file:open(St0#leex.xfile, [read]) of
	{ok,Xfile} ->
	    St1 = St0#leex{xport=Xfile},
	    io:fwrite("Parsing file ~s, ", [St1#leex.xfile]),
	    case parse_head(Xfile) of
		{ok,REAs,Actions,Code} ->
		    io:fwrite("contained ~w rules.~n", [length(REAs)]),
		    file:close(Xfile),
		    {ok,REAs,Actions,Code};
		Error ->
		    file:close(Xfile),
		    Error
	    end;
	{error,_} ->
	    {error,{none,leex,{open,St0#leex.xfile}}}
    end.

%% parse_head(File)
%%  Parse the head of the file.

parse_head(Ifile) -> parse_defs(Ifile, nextline(Ifile, 0)).

%% parse_defs(File, Line)
%%  Parse the macro definition section of a file. Allow no definitions.

parse_defs(Ifile, {ok,"Definitions." ++ _,L}) ->
    parse_defs(Ifile, nextline(Ifile, L), []);
parse_defs(_, {ok,_,L}) ->
    {error,{L,leex,missing_defs}};
parse_defs(_, {eof,L}) ->
    {error,{L,leex,missing_defs}}.

parse_defs(Ifile, {ok,Chars,L}, Ms) ->
    case tokens(Chars, " \t\n") of		%Also strips \n from eol!
	[Name,"=",Def] ->
	    parse_defs(Ifile, nextline(Ifile, L), [{Name,Def}|Ms]);
	_ ->					%Anything else
	    parse_rules(Ifile, {ok,Chars,L}, Ms)
    end;
parse_defs(Ifile, Line, Ms) ->
    parse_rules(Ifile, Line, Ms).

%% parse_rules(File, Line, Macros)
%%  Parse the RE rules section of the file. This must exist.

parse_rules(Ifile, {ok,"Rules." ++ _Rest,L}, Ms) ->
    parse_rules(Ifile, nextline(Ifile, L), Ms, [], [], 0);
parse_rules(_, {ok,_,L}, _) ->
    {error,{L,leex,missing_rules}};
parse_rules(_, {eof,L}, _) ->
    {error,{L,leex,missing_rules}}.

%% parse_rules(File, Result, Macros, RegExpActions, Actions, Acount) ->
%%      {ok,RegExpActions,Actions} | {error,E}.

parse_rules(Ifile, NextLine, Ms, REAs, As, N) ->
    case NextLine of
	{ok,"Erlang code." ++ _Rest,L} ->
	    %% Must be careful to put rules in correct order!
	    parse_code(Ifile, L, reverse(REAs), reverse(As));
	{ok,Chars,L0} ->
	    %%io:fwrite("~w: ~p~n", [L0,Chars]),
	    case collect_rule(Ifile, Chars, L0) of
		{ok,Re,Atoks,L1} ->
		    case parse_rule(Re, L0, Atoks, Ms, N) of
			{ok,REA,A} ->
			    parse_rules(Ifile, nextline(Ifile, L1), Ms,
					[REA|REAs], [A|As], N+1);
			{error,E} -> {error,E}
		    end;
		{error,E} -> {error,E}
	    end;
	{eof,_} ->
	    %% Must be careful to put rules in correct order!
	    {ok,reverse(REAs),reverse(As),[]}
    end.

%% collect_rule(File, Line, Lineno) ->
%%      {ok,RegExp,ActionTokens,NewLineno} | {error,E}.
%% Collect a complete rule by reading lines until the the regexp and
%% action has been read. Keep track of line number.

collect_rule(Ifile, Chars, L0) ->
    {match,St,Len} = regexp:first_match(Chars, "[^ \t]+"),
    %%io:fwrite("RE = ~p~n", [substr(Chars, St, Len)]),
    case collect_action(Ifile, substr(Chars, St+Len), L0, []) of
	{ok,[{':',_}|Toks],L1} -> {ok,substr(Chars, St, Len),Toks,L1};
	{ok,_,_} -> {error,{L0,leex,bad_rule}};
	{eof,L1} -> {error,{L1,leex,bad_rule}};
	{error,E,_} -> {error,E}
    end.

collect_action(Ifile, Chars, L0, Cont0) ->
    case erl_scan:tokens(Cont0, Chars, L0) of
	{done,{ok,Toks,_},_} -> {ok,Toks,L0};
	{done,{eof,_},_} -> {eof,L0};
	{done,{error,E,_},_} -> {error,E,L0};
	{more,Cont1} ->
	    collect_action(Ifile, io:get_line(Ifile, leex), L0+1, Cont1)
    end.

%% parse_rule(RegExpString, RegExpLine, ActionTokens, Macros, Counter)
%%  Parse one regexp after performing macro substition.

parse_rule(S, Line, [{dot,_}], Ms, N) ->
    case parse_rule_regexp(S, Ms) of
	{ok,R} ->
	    {ok,{R,N},{N,empty_action}};
	{error,E} ->
	    {error,{Line,leex,{regexp,E}}}
    end;
parse_rule(S, Line, Atoks, Ms, N) ->
    case parse_rule_regexp(S, Ms) of
	{ok,R} ->
	    case erl_parse:parse_exprs(Atoks) of
		{ok,Aes} ->
		    %% Check for token variables.
		    TokenChars = var_used('TokenChars', Atoks),
		    TokenLen = var_used('TokenLen', Atoks),
		    TokenLine = var_used('TokenLine', Atoks),
		    {ok,{R,N},{N,Aes,TokenChars,TokenLen,TokenLine}};
		{error,_} ->
		    {error,{Line,leex,{after_regexp,S}}}
	    end;
	{error,E} ->
	    {error,{Line,leex,{regexp,E}}}
    end.

var_used(Name, Toks) ->
    case keysearch(Name, 3, Toks) of
	{value,{var,_,Name}} -> true;
	_ -> false
    end.

%% parse_rule_regexp(RegExpString, Macros) -> {ok,RegExp} | {error,Error}.
%% Substitute in macros and parse RegExpString. Cannot use regexp:gsub
%% here as it uses info in replace string (&).

parse_rule_regexp(RE0, [{M,Exp}|Ms]) ->
    case regexp:matches(RE0, "{" ++ M ++ "}") of
	{match,Mats} ->
	    RE1 = sub_repl(Mats, Exp, RE0, 1),
	    parse_rule_regexp(RE1, Ms);
	{error,_} ->
	    parse_rule_regexp(RE0, Ms)
    end;
parse_rule_regexp(RE, []) ->
    %% io:fwrite("RE = ~p~n", [RE]),
    regexp:parse(RE).

sub_repl([{St,L}|Ss], Rep, S, Pos) ->
    Rs = sub_repl(Ss, Rep, S, St+L),
    substr(S, Pos, St-Pos) ++ Rep ++ Rs;
sub_repl([], _Rep, S, Pos) -> substr(S, Pos).

%% parse_code(File, Line, REAs, Actions)
%%  Parse the code section of the file.

parse_code(Ifile, _, REAs, As) ->
    {ok,REAs,As,io:get_chars(Ifile, leex, 102400)}.

%% nextline(InputFile, PrevLineNo) -> {ok,Chars,LineNo} | {eof,LineNo}.
%%  Get the next line skipping comment lines and blank lines.

nextline(Ifile, L) ->
    case io:get_line(Ifile, leex) of
	eof -> {eof,L};
	Chars ->
	    case substr(Chars, span(Chars, " \t\n")+1) of
		[$%|_Rest] -> nextline(Ifile, L+1);
		[] -> nextline(Ifile, L+1);
		_Other -> {ok,Chars,L+1}
	    end
    end.

%% build_combined_nfa(RegExpActionList) -> {NFA,FirstState}.
%%  Build the combined NFA using Thompson's construction straight out
%%  of the book. Build the separate NFAs in the same order as the
%%  rules so that the accepting have ascending states have ascending
%%  state numbers.  Start numbering the states from 1 as we put the
%%  states in a tuple with the state number as the index.

build_combined_nfa(REAs) ->
    {NFA0,Firsts,Free} = build_nfa_list(REAs, [], [], 1),
    F = #nfa_state{no=Free,edges=epsilon_trans(Firsts)},
    {list_to_tuple(keysort(#nfa_state.no, [F|NFA0])),Free}.

build_nfa_list([{RE,Action}|REAs], NFA0, Firsts, Free0) ->
    {NFA1,Free1,First} = build_nfa(RE, Free0, Action),
    build_nfa_list(REAs, NFA1 ++ NFA0, [First|Firsts], Free1);
build_nfa_list([], NFA, Firsts, Free) ->
    {NFA,reverse(Firsts),Free}.

epsilon_trans(Firsts) -> [ {epsilon,F} || F <- Firsts ].

%% build_nfa(RegExp, NextState, Action) -> {NFA,NextState,FirstState}.
%%  When building the NFA states for a ??? we don't build the end
%%  state, just allocate a State for it and return this state
%%  number. This allows us to avoid building unnecessary states for
%%  concatenation which would then have to be removed by overwriting
%%  an existing state.

build_nfa(RE, N0, Action) ->
    {NFA,N1,E} = build_nfa(RE, N0+1, N0, []),
    {[#nfa_state{no=E,accept={accept,Action}}|NFA],N1,N0}.

%% build_nfa(RegExp, NextState, FirstState, NFA) -> {NFA,NextState,EndState}.
%%  The NFA is a list of nfa_state is no predefined order. The state
%%  number of the returned EndState is already allocated!

build_nfa({'or',RE1,RE2}, N0, F, NFA0) ->
    {NFA1,N1,E1} = build_nfa(RE1, N0+1, N0, NFA0),
    {NFA2,N2,E2} = build_nfa(RE2, N1+1, N1, NFA1),
    E = N2,					%End state
    {[#nfa_state{no=F,edges=[{epsilon,N0},{epsilon,N1}]},
      #nfa_state{no=E1,edges=[{epsilon,E}]},
      #nfa_state{no=E2,edges=[{epsilon,E}]}|NFA2],
     N2+1,E};
build_nfa({concat,RE1, RE2}, N0, F, NFA0) ->
    {NFA1,N1,E1} = build_nfa(RE1, N0, F, NFA0),
    {NFA2,N2,E2} = build_nfa(RE2, N1, E1, NFA1),
    {NFA2,N2,E2};
build_nfa({kclosure,RE}, N0, F, NFA0) ->
    {NFA1,N1,E1} = build_nfa(RE, N0+1, N0, NFA0),
    E = N1,					%End state
    {[#nfa_state{no=F,edges=[{epsilon,N0},{epsilon,E}]},
      #nfa_state{no=E1,edges=[{epsilon,N0},{epsilon,E}]}|NFA1],
     N1+1,E};
build_nfa({pclosure,RE}, N0, F, NFA0) ->
    {NFA1,N1,E1} = build_nfa(RE, N0+1, N0, NFA0),
    E = N1,					%End state
    {[#nfa_state{no=F,edges=[{epsilon,N0}]},
      #nfa_state{no=E1,edges=[{epsilon,N0},{epsilon,E}]}|NFA1],
     N1+1,E};
build_nfa({optional,RE}, N0, F, NFA0) ->
    {NFA1,N1,E1} = build_nfa(RE, N0+1, N0, NFA0),
    E = N1,					%End state
    {[#nfa_state{no=F,edges=[{epsilon,N0},{epsilon,E}]},
      #nfa_state{no=E1,edges=[{epsilon,E}]}|NFA1],
     N1+1,E};
build_nfa({char_class,Cc}, N, F, NFA) ->
    {[#nfa_state{no=F,edges=[{char_class(Cc),N}]}|NFA],N+1,N};
build_nfa({comp_class,Cc}, N, F, NFA) ->
    {[#nfa_state{no=F,edges=[{comp_class(Cc),N}]}|NFA],N+1,N};
build_nfa(C, N, F, NFA) when integer(C) ->
    {[#nfa_state{no=F,edges=[{[C],N}]}|NFA],N+1,N}.

char_class(Cc) ->
    lists:foldl(fun ({C1,C2}, Set) -> union(seq(C1, C2), Set);
		    (C, Set) -> add_element(C, Set) end, [], Cc).

comp_class(Cc) -> subtract(seq(0, 255), char_class(Cc)).

%% build_dfa(NFA, NfaFirstState) -> {DFA,DfaFirstState}.
%%  Build a DFA from an NFA using "subset construction". The major
%%  difference from the book is that we keep the marked and unmarked
%%  DFA states in seperate lists. New DFA states are added to the
%%  unmarked list and states are marked by moving them to the marked
%%  list. We assume that the NFA accepting state numbers are in
%%  ascending order for the rules and use ordsets to keep this order.

build_dfa(NFA, Nf) ->
    D = #dfa_state{no=0,nfa=eclosure([Nf], NFA)},
    {build_dfa([D], 1, [], NFA),0}.

%% build_dfa([UnMarked], NextState, [Marked], NFA) -> DFA.
%%  Traverse the unmarked states. Temporarily add the current unmarked
%%  state to the marked list before calculating translation, this is
%%  to avoid adding too many duplicate states. Add it properly to the
%%  marked list afterwards with correct translations.

build_dfa([U|Us0], N0, Ms, NFA) ->
    {Ts,Us1,N1} = build_dfa(255, U#dfa_state.nfa, Us0, N0, [], [U|Ms], NFA),
    M = U#dfa_state{trans=Ts,accept=accept(U#dfa_state.nfa, NFA)},
    build_dfa(Us1, N1, [M|Ms], NFA);
build_dfa([], _, Ms, _) -> Ms.

%% build_dfa(Char, [NfaState], [Unmarked], NextState, [Transition], [Marked], NFA) ->
%%	{Transitions,UnmarkedStates,NextState}.
%%  Foreach NFA state set calculate the legal translations. N.B. must
%%  search *BOTH* the unmarked and marked lists to check if DFA state
%%  already exists. By test characters downwards and prepending
%%  transitions we get the transition lists in ascending order.

build_dfa(C, Set, Us, N, Ts, Ms, NFA) when C >= 0 ->
    case eclosure(move(Set, C, NFA), NFA) of
	S when S /= [] ->
	    case dfa_state_exist(S, Us, Ms) of
		{yes,T} ->
		    build_dfa(C-1, Set, Us, N, [{C,T}|Ts], Ms, NFA);
		no ->
		    U = #dfa_state{no=N,nfa=S},
		    build_dfa(C-1, Set, [U|Us], N+1, [{C,N}|Ts], Ms, NFA)
	    end;
	[] ->
	    build_dfa(C-1, Set, Us, N, Ts, Ms, NFA)
    end;
build_dfa(-1, _, Us, N, Ts, _, _) ->
    {Ts,Us,N}.

%% dfa_state_exist(Set, Unmarked, Marked) -> {yes,State} | no.

dfa_state_exist(S, Us, Ms) ->
    case keysearch(S, #dfa_state.nfa, Us) of
	{value,#dfa_state{no=T}} -> {yes,T};
	false ->
	    case keysearch(S, #dfa_state.nfa, Ms) of
		{value,#dfa_state{no=T}} -> {yes,T};
		false -> no
	    end
    end.

%% eclosure([State], NFA) -> [State].
%% move([State], Char, NFA) -> [State].
%%  These are straight out of the book. As eclosure uses ordsets then
%%  the generated state sets are in ascending order.

eclosure(Sts, NFA) -> eclosure(Sts, NFA, []).

eclosure([St|Sts], NFA, Ec) ->
    #nfa_state{edges=Es} = element(St, NFA),
    eclosure([ N || {epsilon,N} <- Es,
		    not is_element(N, Ec) ] ++ Sts,
	     NFA, add_element(St, Ec));
eclosure([], _, Ec) -> Ec.

move(Sts, C, NFA) ->
    [St || N <- Sts,
	   {C1,St} <- (element(N, NFA))#nfa_state.edges,
	   list(C1),
	   member(C, C1) ].

%% accept([State], NFA) -> {accept,A} | noaccept.
%%  Scan down the state list until we find an accepting state.

accept([St|Sts], NFA) ->
    case element(St, NFA) of
	#nfa_state{accept={accept,A}} -> {accept,A};
	#nfa_state{accept=noaccept} -> accept(Sts, NFA)
    end;
accept([], _) -> noaccept.

%% minimise_dfa(DFA, DfaFirst) -> {DFA,DfaFirst}.
%%  Minimise the DFA by removing equivalent states. We consider a
%%  state if both the transitions and the their accept state is the
%%  same.  First repeatedly run throught the DFA state list removing
%%  equivalent states and updating remaining transitions with
%%  remaining equivalent state numbers. When no more reductions are
%%  possible then pack the remaining state numbers to get consecutive
%%  states.

minimise_dfa(DFA0, Df0) ->
    case min_dfa(DFA0) of
	{DFA1,[]} ->				%No reduction!
	    {DFA2,Rs} = pack_dfa(DFA1),
	    {min_update(DFA2, Rs),min_use(Df0, Rs)};
	{DFA1,Rs} ->
	    minimise_dfa(min_update(DFA1, Rs), min_use(Df0, Rs))
    end.

min_dfa(DFA) -> min_dfa(DFA, [], []).

min_dfa([D|DFA0], Rs0, MDFA) ->
    {DFA1,Rs1} = min_delete(DFA0, D#dfa_state.trans, D#dfa_state.accept,
			    D#dfa_state.no, Rs0, []),
    min_dfa(DFA1, Rs1, [D|MDFA]);
min_dfa([], Rs, MDFA) -> {MDFA,Rs}.

%% min_delete(States, Trans, Action, NewN, Rs, MiniDFA) -> {MiniDFA,Rs}.
%%  Delete all states with same transactions and action. Return
%%  rewrites and minimised DFA with no duplicate states.

min_delete([#dfa_state{no=N,trans=T,accept=A}|DFA], T, A, NewN, Rs, MDFA) ->
    min_delete(DFA, T, A, NewN, [{N,NewN}|Rs], MDFA);
min_delete([D|DFA], T, A, NewN, Rs, MDFA) ->
    min_delete(DFA, T, A, NewN, Rs, [D|MDFA]);
min_delete([], _, _, _, Rs, MDFA) -> {MDFA,Rs}.

min_update(DFA, Rs) ->
    [ D#dfa_state{trans=min_update_trans(D#dfa_state.trans, Rs)} || D <- DFA ].

min_update_trans(Tr, Rs) ->
    [ {C,min_use(S, Rs)} || {C,S} <- Tr ].

min_use(Old, [{Old,New}|_]) -> New;
min_use(Old, [_|Reds]) -> min_use(Old, Reds);
min_use(Old, []) -> Old.

pack_dfa(DFA) -> pack_dfa(DFA, 0, [], []).

pack_dfa([D|DFA], NewN, Rs, PDFA) ->
    pack_dfa(DFA, NewN+1,
	     [{D#dfa_state.no,NewN}|Rs],
	     [D#dfa_state{no=NewN}|PDFA]);
pack_dfa([], _, Rs, PDFA) -> {PDFA,Rs}.

%% The main output is the yystate function which is built from the
%% DFA. It has the spec:
%%
%% yystate() -> InitialState.
%% yystate(State, InChars, Line, CurrTokLen, AcceptAction, AcceptLen) ->
%%      {Action, AcceptLength, RestChars, Line} |         Accepting end state
%%      {Action, AcceptLength, RestChars, Line, State} |  Accepting state
%%      {reject, AcceptLength, CurrTokLen, RestChars, Line, State} |
%%      {Action, AcceptLength, CurrTokLen, RestChars, Line, State}.

%% The return CurrTokLen is always the current number of characters
%% scanned in the current token. The returns have the follwoing
%% meanings:
%% {Action, AcceptLength, RestChars, Line} -
%%  The scanner has reached an accepting end-state, for example after
%%  a regexp "abc". Action is the action number and AcceptLength is
%%  the length of the matching token.
%%
%% {Action, AcceptLength, RestChars, Line, State} -
%%  The scanner has reached an accepting transition state,
%%  continuation depends on RestChars. If RestChars == [] (no more
%%  current characters) then we need to get more characters to see if
%%  it is an end-state, otherwise (eof or chars) then it is an end
%%  state.
%%
%% {reject, AcceptLength, CurrTokLen, RestChars, Line, State} -
%% {Action, AcceptLength, CurrTokLen, RestChars, Line, State} -
%%  The scanner has reached a non accepting transistion state. If
%%  RestChars == [] we need to get more characters to continue.
%%  Otherwise if 'reject' then no accepting state has been reached it
%%  is an error. If we have an Action and AcceptLength then these are
%%  the last accept state, use them and continue from there.

%% out_file(LeexState, DFA, DfaStart, [Action], Code) -> ok | error.
%%  Generate an output .erl file from the include file, the DFA and
%%  the code for the action.s

out_file(St0, DFA, DF, Actions, Code) ->
    io:fwrite("Writing file ~s, ", [St0#leex.efile]),
    case file:path_open([".", [code:lib_dir(),"/tools/include"]],
			St0#leex.ifile, [read]) of
	{ok,Ifile,_} ->
	    St1 = St0#leex{iport=Ifile},
	    case file:open(St1#leex.efile, [write]) of
		{ok,Ofile} ->
		    St2 = St1#leex{eport=Ofile},
		    out_file(Ifile, Ofile, St2#leex.module,
			     DFA, DF, Actions, Code),
		    file:close(Ifile),
		    file:close(Ofile),
		    io:fwrite("ok~n"),
		    ok;
		{error,_} ->
		    file:close(Ifile),
		    io:fwrite("~s: open error~n", [St1#leex.efile]),
		    error
	    end;
	{error,_} ->
	    io:fwrite("~s: open error~n", [St0#leex.ifile]),
	    error
    end.

%% out_file(IncFile, OutFile, ModName, DFA, DfaStart, Actions, Code) -> ok.
%%  Copy the include file line by line substituting special lines with
%%  generated code. We cheat by only looking at the first 5
%%  characters.

out_file(Ifile, Ofile, Mod, DFA, DF, Actions, Code) ->
    case io:get_line(Ifile, leex) of
	eof -> ok;
	Line ->
	    case substr(Line, 1, 5) of
		"##mod" -> io:fwrite(Ofile, "-module(~w).\n", [Mod]);
		"##cod" -> io:put_chars(Ofile, Code);
		"##dfa" -> out_dfa(Ofile, DFA, DF);
		"##act" -> out_actions(Ofile, Actions);
		_ -> io:put_chars(Ofile, Line)
	    end,
	    out_file(Ifile, Ofile, Mod, DFA, DF, Actions, Code)
    end.

out_dfa(File, DFA, DF) ->
    io:fwrite(File, "yystate() -> ~w.~n~n", [DF]),
    foreach(fun (S) -> out_trans(File, S) end, DFA),
    io:fwrite(File, "yystate(S, Ics, Line, Tlen, Action, Alen) ->~n", []),
    io:fwrite(File, "    {Action,Alen,Tlen,Ics,Line,S}.~n", []).
    
out_trans(File, #dfa_state{no=N,trans=[],accept={accept,A}}) ->
    %% Accepting end state, guaranteed done.
    io:fwrite(File, "yystate(~w, Ics, Line, Tlen, _, _) ->~n", [N]),
    io:fwrite(File, "    {~w,Tlen,Ics,Line};~n", [A]);
out_trans(File, #dfa_state{no=N,trans=Tr,accept={accept,A}}) ->
    %% Accepting state, but there maybe more.
    foreach(fun (T) -> out_accept_tran(File, N, A, T) end, pack_trans(Tr)),
    io:fwrite(File, "yystate(~w, Ics, Line, Tlen, _, _) ->~n", [N]),
    io:fwrite(File, "    {~w,Tlen,Ics,Line,~w};~n", [A,N]);
out_trans(File, #dfa_state{no=N,trans=Tr,accept=noaccept}) ->
    %% Non-accepting transition state.
    foreach(fun (T) -> out_noaccept_tran(File, N, T) end, pack_trans(Tr)),
    io:fwrite(File, "yystate(~w, Ics, Line, Tlen, Action, Alen) ->~n", [N]),
    io:fwrite(File, "    {Action,Alen,Tlen,Ics,Line,~w};~n", [N]).

out_accept_tran(File, N, A, {{Cf,Cl},S}) ->
    out_accept_head(File, N, io_lib:write_char(Cf), io_lib:write_char(Cl)),
    out_accept_body(File, S, "Line", A);
out_accept_tran(File, N, A, {$\n,S}) ->
    out_accept_head(File, N, "$\\n"),
    out_accept_body(File, S, "Line+1", A);
out_accept_tran(File, N, A, {C,S}) ->
    Char = io_lib:write_char(C),
    out_accept_head(File, N, Char),
    out_accept_body(File, S, "Line", A).

out_noaccept_tran(File, N, {{Cf,Cl},S}) ->
    out_noaccept_head(File, N, io_lib:write_char(Cf), io_lib:write_char(Cl)),
    out_noaccept_body(File, S, "Line");
out_noaccept_tran(File, N, {$\n,S}) ->
    out_noaccept_head(File, N, "$\\n"),
    out_noaccept_body(File, S, "Line+1");
out_noaccept_tran(File, N, {C,S}) ->
    Char = io_lib:write_char(C),
    out_noaccept_head(File, N, Char),
    out_noaccept_body(File, S, "Line").

out_accept_head(File, State, Char) ->
    io:fwrite(File, "yystate(~w, [~s|Ics], Line, Tlen, _, _) ->\n",
	      [State,Char]).

out_accept_head(File, State, Min, Max) ->
    io:fwrite(File, "yystate(~w, [C|Ics], Line, Tlen, _, _) when C >= ~s, C =< ~s ->\n",
	      [State,Min,Max]).

out_accept_body(File, Next, Line, Action) ->
    io:fwrite(File, "    yystate(~w, Ics, ~s, Tlen+1, ~w, Tlen);\n",
	      [Next,Line,Action]).

out_noaccept_head(File, State, Char) ->
    io:fwrite(File, "yystate(~w, [~s|Ics], Line, Tlen, Action, Alen) ->\n",
	      [State,Char]).

out_noaccept_head(File, State, Min, Max) ->
    io:fwrite(File, "yystate(~w, [C|Ics], Line, Tlen, Action, Alen) when C >= ~s, C =< ~s ->\n",
	      [State,Min,Max]).

out_noaccept_body(File, Next, Line) ->
    io:fwrite(File, "    yystate(~w, Ics, ~s, Tlen+1, Action, Alen);\n",
	      [Next,Line]).

%% pack_trans([{Char,State}]) -> [{Crange,State}] when
%%	Crange = {Char,Char} | Char.
%%  Pack the translation table into something more suitable for
%%  generating code. Ranges of characters with the same State are
%%  packed together, while solitary characters are left "as is". We
%%  KNOW how the pattern matching compiler works so solitary
%%  characters are stored before ranges. We do this using ordsets for
%%  for the packed table. Always break out $\n as solitary character.

pack_trans(Tr) -> pack_trans(Tr, []).

pack_trans([{C,S}|Tr], Pt) -> pack_trans(Tr, C, C, S, Pt);
pack_trans([], Pt) -> Pt.

pack_trans([{$\n,S1}|Tr], Cf, Cl, S, Pt0) ->
    Pt1 = pack_trans(Cf, Cl, S, Pt0),		%Take what we got ...
    Pt2 = add_element({$\n,S1}, Pt1),		%add separate $\n ...
    pack_trans(Tr, Pt2);			%and keep going
pack_trans([{C,S}|Tr], Cf, Cl, S, Pt) when C == Cl + 1 ->
    pack_trans(Tr, Cf, C, S, Pt);		%Add to range
pack_trans(Tr, Cf, Cl, S, Pt) ->
    pack_trans(Tr, pack_trans(Cf, Cl, S, Pt)).

pack_trans(C, C, S, Pt) -> add_element({C,S}, Pt);
pack_trans(Cf, Cl, S, Pt) when Cl == Cf + 1 ->
    add_element({Cf,S}, add_element({Cl,S}, Pt));
pack_trans(Cf, Cl, S, Pt) -> add_element({{Cf,Cl},S}, Pt).

%% out_actions(File, ActionList) -> ok.
%% Write out the action table.

out_actions(File, As) ->
    foreach(fun (A) -> out_action(File, A) end, As),
    io:fwrite(File, "yyaction(_, _, _, _) -> error.~n", []).

out_action(File, {A,empty_action}) ->
    io:fwrite(File, "yyaction(~w, _, _, _) -> skip_token;~n", [A]);
out_action(File, {A,Code,TokenChars,TokenLen,TokenLine}) ->
    Len = if TokenLen or TokenChars -> "TokenLen" ; true -> "_" end,
    Line = if TokenLine -> "TokenLine" ; true -> "_" end,
    Tcs = if TokenChars -> "YYtcs" ; true -> "_" end,
    io:fwrite(File, "yyaction(~w, ~s, ~s, ~s) ->~n", [A,Len,Tcs,Line]),
    if
	TokenChars == true ->
	    io:fwrite(File, "    TokenChars = yypre(YYtcs, TokenLen),~n", []);
	TokenChars == false -> ok
    end,
    io:fwrite(File, "    ~s;~n", [erl_pp:exprs(Code, 4, none)]).

%% out_dfa_graph(LeexState, DFA, DfaStart) -> ok | error.
%%  Writes the DFA to a .dot file in DOT-format which can be viewed
%%  with Graphviz.

out_dfa_graph(St, DFA, DF) ->
    io:fwrite("Writing DFA to file ~s, ", [St#leex.gfile]),
    case file:open(St#leex.gfile, [write]) of
	{ok,Gfile} ->
	    io:fwrite(Gfile, "digraph DFA {~n", []),
	    out_dfa_states(Gfile, DFA, DF),
	    out_dfa_edges(Gfile, DFA),
	    io:fwrite(Gfile, "}~n", []),
	    file:close(Gfile),
	    io:fwrite("ok~n"),
	    ok;
	{error,_} ->
	    io:fwrite("~s: open error~n", [St#leex.gfile]),
	    error
    end.

out_dfa_states(File, DFA, DF) ->
    foreach(fun (S) -> out_dfa_state(File, DF, S) end, DFA),
    io:fwrite(File, "~n", []).

out_dfa_state(File, DF, #dfa_state{no=DF, accept={accept,_}}) ->
    io:fwrite(File, "  ~b [shape=doublecircle color=green];~n", [DF]);
out_dfa_state(File, DF, #dfa_state{no=DF, accept=noaccept}) ->
    io:fwrite(File, "  ~b [shape=circle color=green];~n", [DF]);
out_dfa_state(File, _, #dfa_state{no=S, accept={accept,_}}) ->
    io:fwrite(File, "  ~b [shape=doublecircle];~n", [S]);    
out_dfa_state(File, _, #dfa_state{no=S, accept=noaccept}) ->
    io:fwrite(File, "  ~b [shape=circle];~n", [S]).

out_dfa_edges(File, DFA) ->
    foreach(fun (#dfa_state{no=S,trans=Trans}) ->
		    Pt = pack_trans(Trans),
		    Tdict = foldl(fun ({Cr,T}, D) ->
					  orddict:append(T, Cr, D)
				  end, orddict:new(), Pt),
		    foreach(fun (T) ->
				    Crs = orddict:fetch(T, Tdict),
				    Edgelab = dfa_edgelabel(Crs),
				    io:fwrite(File, "  ~b -> ~b [label=\"~s\"];~n",
					      [S,T,Edgelab])
			    end, sort(orddict:fetch_keys(Tdict)))
	    end, DFA).

dfa_edgelabel([C]) when is_integer(C) -> quote(C);
dfa_edgelabel(Cranges) ->
    "[" ++ map(fun ({A,B}) -> [quote(A), "-", quote(B)];
		   (C)     -> [quote(C)]
	       end, Cranges) ++ "]".

quote($^)  -> "\\^";
quote($.)  -> "\\.";
quote($$)  -> "\\$";
quote($-)  -> "\\-";
quote($[)  -> "\\[";
quote($])  -> "\\]";
quote($\s) -> "\\\\s";
quote($\")  -> "\\\"";
quote($\b) -> "\\\\b";
quote($\f) -> "\\\\f";
quote($\n) -> "\\\\n";
quote($\r) -> "\\\\r";
quote($\t) -> "\\\\t";
quote($\e) -> "\\\\e";
quote($\v) -> "\\\\v";
quote($\d) -> "\\\\d";
quote($\\) -> "\\\\";
quote(C) when 32 =< C, C =< 126 -> [C];
quote(C) when 0 =< C, C =< 255 ->
    <<T2:2,T1:3,T0:3>> = <<C>>,
    ["\\\\", $0+T2, $0+T1, $0+T0].
