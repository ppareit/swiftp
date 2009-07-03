%% @author Chris Anderson <jchris@grabb.it>
%% @copyright 2008 Chris Anderson.
%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2006 Mochi Media, Inc.
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without restriction,
%% including without limitation the rights to use, copy, modify, merge,
%% publish, distribute, sublicense, and/or sell copies of the Software,
%% and to permit persons to whom the Software is furnished to do
%% so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included
%% in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
%% IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
%% CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
%% TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
%% SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

%% @doc Parser to illustrate EEP
%% tests based on mochijson


-module(json_eep).
-author('bob@mochimedia.com').
-author('jchris@grabb.it').
-export([json_to_term/1, term_to_json/1]).
-export([test/0,profile/0]).

-define(Q, $\").

-define(LOG(Format, Args),
    io:format(Format, Args)).


term_to_json(T) ->
    S = json_encode(T),
    case is_list(S) of
    true ->
        % I don't know why this helps
        binary_to_list(list_to_binary(S));
    false ->
        binary_to_list(S)
    end.

json_to_term(S) ->
    {ok, Tokens, _} = json_lex2:string(S),
    % ?LOG("Tokens ~p~n",[Tokens]),
    {ok, Result} = json_grammar:parse(Tokens),
    % ?LOG("Result ~p~n",[Result]),
    Result.


json_encode(true) ->
    <<"true">>;
json_encode(false) ->
    <<"false">>;
json_encode(null) ->
    <<"null">>;
json_encode(I) when is_integer(I) andalso I >= -2147483648 andalso I =< 2147483647 ->
    %% Anything outside of 32-bit integers should be encoded as a float
    integer_to_list(I);
json_encode(I) when is_integer(I) ->
    format_float(float(I));
json_encode(F) when is_float(F) ->
    format_float(F);
json_encode(S) when is_binary(S); is_atom(S) ->
    json_encode_string(S);
json_encode(Array) when is_list(Array) ->
    json_encode_array(Array);
json_encode({Props}) when is_list(Props) ->
    json_encode_proplist(Props);
json_encode(Bad) ->
    exit({json_encode, {bad_term, Bad}}).

json_encode_array([]) ->
    <<"[]">>;
json_encode_array(L) ->
    F = fun (O, Acc) ->
                [$,, json_encode(O) | Acc]
        end,
    [$, | Acc1] = lists:foldl(F, "[", L),
    lists:reverse([$\] | Acc1]).

json_encode_proplist([]) ->
    <<"{}">>;
json_encode_proplist(Props) ->
    F = fun ({K, V}, Acc) ->
                % ?LOG("K ~p V ~p~n",[K, V]),
                KS = json_encode_string(K),
                VS = json_encode(V),
                % ?LOG("KS ~p VS ~p~n",[KS, VS]),
                [$,, VS, $:, KS | Acc]
        end,
    [$, | Acc1] = lists:foldl(F, "{", Props),
    lists:reverse([$\} | Acc1]).

json_encode_string(A) when is_atom(A) ->
    json_encode_string_unicode(xmerl_ucs:from_utf8(atom_to_list(A)), [?Q]);
json_encode_string(B) when is_binary(B) ->
    json_encode_string_unicode(xmerl_ucs:from_utf8(B), [?Q]);
json_encode_string(I) when is_integer(I) ->
    json_encode_string_unicode(integer_to_list(I), [?Q]);
json_encode_string(L) when is_list(L) ->
    json_encode_string_unicode(L, [?Q]).

json_encode_string_unicode([], Acc) ->
    lists:reverse([$\" | Acc]);
json_encode_string_unicode([C | Cs], Acc) ->
    Acc1 = case C of
               ?Q ->
                   [?Q, $\\ | Acc];
               %% Escaping solidus is only useful when trying to protect
               %% against "</script>" injection attacks which are only
               %% possible when JSON is inserted into a HTML document
               %% in-line. mochijson2 does not protect you from this, so
               %% if you do insert directly into HTML then you need to
               %% uncomment the following case or escape the output of encode.
               %%
               %% $/ ->
               %%    [$/, $\\ | Acc];
               %%
               $\\ ->
                   [$\\, $\\ | Acc];
               $\b ->
                   [$b, $\\ | Acc];
               $\f ->
                   [$f, $\\ | Acc];
               $\n ->
                   [$n, $\\ | Acc];
               $\r ->
                   [$r, $\\ | Acc];
               $\t ->
                   [$t, $\\ | Acc];
               C when C >= 0, C < $\s; C >= 16#7f, C =< 16#10FFFF ->
                   [unihex(C) | Acc];
               C when C < 16#7f ->
                   [C | Acc];
               _ ->
                   exit({json_encode, {bad_char, C}})
           end,
    json_encode_string_unicode(Cs, Acc1).

hexdigit(C) when C >= 0, C =< 9 ->
    C + $0;
hexdigit(C) when C =< 15 ->
    C + $a - 10.

unihex(C) when C < 16#10000 ->
    <<D3:4, D2:4, D1:4, D0:4>> = <<C:16>>,
    Digits = [hexdigit(D) || D <- [D3, D2, D1, D0]],
    [$\\, $u | Digits];
unihex(C) when C =< 16#10FFFF ->
    N = C - 16#10000,
    S1 = 16#d800 bor ((N bsr 10) band 16#3ff),
    S2 = 16#dc00 bor (N band 16#3ff),
    [unihex(S1), unihex(S2)].

format_float(F) ->
    format_float1(lists:reverse(float_to_list(F)), []).

format_float1([$0, $0, _, $e | Rest], []) ->
    strip_zeros(Rest, []);
format_float1([Sign, $e | Rest], Acc) ->
    strip_zeros(Rest, [$e, Sign | Acc]);
format_float1([C | Rest], Acc) ->
    format_float1(Rest, [C | Acc]).

strip_zeros(L=[$0, $. | _], Acc) ->
    lists:reverse(L, Acc);
strip_zeros([$0 | Rest], Acc) ->
    strip_zeros(Rest, Acc);
strip_zeros(L, Acc) ->
    lists:reverse(L, Acc).

%% Test for equivalence of Erlang terms.
%% Due to arbitrary order of construction, equivalent objects might
%% compare unequal as erlang terms, so we need to carefully recurse
%% through aggregates (tuples and objects).

equiv(T1, T2) when is_list(T1), is_list(T2) ->
    equiv_list(T1, T2);
equiv({T1}, {T2}) when is_list(T1), is_list(T2) ->
    equiv_object(T1, T2);
equiv(N1, N2) when is_number(N1), is_number(N2) -> N1 == N2;
equiv(S1, S2) when is_binary(S1), is_binary(S2) -> S1 == S2;
equiv(true, true) -> true;
equiv(false, false) -> true;
equiv(null, null) -> true.

%% Object representation and traversal order is unknown.
%% Use the sledgehammer and sort property lists.

equiv_object(Props1, Props2) ->
    L1 = lists:keysort(1, Props1),
    L2 = lists:keysort(1, Props2),
    Pairs = lists:zip(L1, L2),
    true = lists:all(fun({{K1, V1}, {K2, V2}}) ->
    equiv(K1, K2) and equiv(V1, V2)
    end, Pairs).

%% Recursively compare tuple elements for equivalence.

equiv_list([], []) ->
    true;
equiv_list([V1 | L1], [V2 | L2]) ->
    case equiv(V1, V2) of
    true ->
        equiv_list(L1, L2);
    false ->
        false
    end.

profile() ->
    {ok, Tracer} = fprof:profile(start),
    fprof:trace([start, {tracer, Tracer}]),
    profile_next(tests(binary)),
    fprof:trace(stop),
    fprof:analyse().
    

profile_next([]) -> ok;

profile_next([{_,J}|Rest]) ->
    Term = json_to_term(J),
    % term_to_json(Term),
    profile_next(Rest).

test() -> 
  test_next(tests(binary)).

test_next([]) -> {ok, passed};

test_next([{E,J}|Rest]) ->
  ?LOG("Test: Term ~p JSON ~s~n", [E, J]),
  Term = json_to_term(J),
  ?LOG("Decoded ~p~n", [Term]),
  true = equiv(E, Term),
  Json = term_to_json(Term),
  ?LOG("Encoded ~s~n~n", [Json]),
  true = equiv(Term, json_to_term(Json)),
  test_next(Rest).
  
tests(binary) ->
  [
    {{[{<<"key">>,<<"value">>}]}, "{\"key\":\"value\"}"},
    {{[]},"{}"},
    {[], "[]"},
    {1, "1"},
    {3.1416, "3.14160"}, % text representation may truncate, trail zeroes
    {-1, "-1"},
    {[-3.1416], "[-3.14160]"},
    {{[{<<"number">>, 12.0e10}]}, "{\"number\":1.20000e+11}"},
    {[1.234E+10], "[1.23400e+10]"},
    {[-1.234E-10], "[-1.23400e-10]"},
    {[10.0], "[1.0e+01]"},
    {[123.456], "[1.23456E+2]"},
    {[10.0], "[1e1]"},
    {<<"foo">>, "\"foo\""},
    {[<<"">>], "[\"\"]"},
    {[<<"1/4">>], "[\"1\/4\"]"},
    {[<<"name is \"Quentin\"">>], "[\"name is \\\"Quentin\\\"\"]"},
    {[<<"\n\n\n">>], "[\"\\n\\n\\n\"]"},
    {[iolist_to_binary("foo" ++ [5] ++ "bar")], "[\"foo\\u0005bar\"]"},
    {{[{<<"foo">>, <<"bar">>}]}, "{\"foo\":\"bar\"}"},
    {{[{<<"foo">>, <<"bar">>}, {<<"baz">>, 123}]}, "{\"foo\":\"bar\",\"baz\":123}"},
    {[[]], "[[]]"},
    {[1, <<"foo">>], "[1,\"foo\"]"},

    % json array in a json object
    {{[{<<"foo">>, [123]}]}, "{\"foo\":[123]}"},

    % json object in a json object
    {{[{<<"foo">>, {[{<<"bar">>, true}]}}]},
     "{\"foo\":{\"bar\":true}}"},

    % fold evaluation order
    {{[{<<"foo">>, []},
                     {<<"bar">>, {[{<<"baz">>, true}]}},
                     {<<"alice">>, <<"bob">>}]},
     "{\"foo\":[],\"bar\":{\"baz\":true},\"alice\":\"bob\"}"},

    % json object in a json array
    {[-123, <<"foo">>, {[{<<"bar">>, []}]}, null],
     "[-123,\"foo\",{\"bar\":[]},null]"}
  ].