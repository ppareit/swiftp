%%% File    : erlang_scan.xrl
%%% Author  : Robert Virding
%%% Purpose : Tkoen definitions for Erlang.

Definitions.
O	= [0-7]
D	= [0-9]
H	= [0-9a-fA-F]
U	= [A-Z]
L	= [a-z]
A	= ({U}|{L}|{D}|_|@)
WS	= ([\000-\s]|%.*)

Rules.
{D}+\.{D}+((E|e)(\+|\-)?{D}+)? :
			{token,{float,TokenLine,list_to_float(TokenChars)}}.
{D}+#{H}+	:	base(TokenLine, TokenChars).
{D}+		:	{token,{integer,TokenLine,list_to_integer(TokenChars)}}.
{L}{A}*		:	Atom = list_to_atom(TokenChars),
			{token,case reserved_word(Atom) of
				   true -> {Atom,TokenLine};
				   false -> {atom,TokenLine,Atom}
			       end}.
'(\\\^.|\\.|[^'])*' :
			%% Strip quotes.
			S = lists:sublist(TokenChars, 2, TokenLen - 2),
			case catch list_to_atom(string_gen(S)) of
			    {'EXIT',_} -> {error,"illegal atom " ++ TokenChars};
			    Atom -> {token,{atom,TokenLine,Atom}}
			end.
({U}|_){A}*	:	{token,{var,TokenLine,list_to_atom(TokenChars)}}.
"(\\\^.|\\.|[^"])*" :
			%% Strip quotes.
			S = lists:sublist(TokenChars, 2, TokenLen - 2),
			{token,{string,TokenLine,string_gen(S)}}.
\$(\\{O}{O}{O}|\\\^.|\\.|.) :
			{token,{integer,TokenLine,cc_convert(TokenChars)}}.
->		:	{token,{'->',TokenLine}}.
:-		:	{token,{':-',TokenLine}}.
=/=		:	{token,{'=/=',TokenLine}}.
==		:	{token,{'==',TokenLine}}.
=:=		:	{token,{'=:=',TokenLine}}.
/=		:	{token,{'/=',TokenLine}}.
>=		:	{token,{'>=',TokenLine}}.
=<		:	{token,{'=<',TokenLine}}.
<=		:	{token,{'<=',TokenLine}}.
\+\+		:	{token,{'++',TokenLine}}.
--		:	{token,{'--',TokenLine}}.
[]()[}{|!?/;:,.*+#<>=-] :
			{token,{list_to_atom(TokenChars),TokenLine}}.
\.{WS}		:	{end_token,{dot,TokenLine}}.
{WS}+		:	skip_token.

Erlang code.

-export([reserved_word/1]).

reserved_word('after') -> true;
reserved_word('begin') -> true;
reserved_word('case') -> true;
reserved_word('catch') -> true;
reserved_word('end') -> true;
reserved_word('fun') -> true;
reserved_word('if') -> true;
reserved_word('let') -> true;
reserved_word('of') -> true;
reserved_word('query') -> true;
reserved_word('receive') -> true;
reserved_word('when') -> true;
reserved_word('bnot') -> true;
reserved_word('not') -> true;
reserved_word('div') -> true;
reserved_word('rem') -> true;
reserved_word('band') -> true;
reserved_word('and') -> true;
reserved_word('bor') -> true;
reserved_word('bxor') -> true;
reserved_word('bsl') -> true;
reserved_word('bsr') -> true;
reserved_word('or') -> true;
reserved_word('xor') -> true;
reserved_word(_) -> false.

base(L, Cs) ->
    H = string:chr(Cs, $#),
    case list_to_integer(string:substr(Cs, 1, H-1)) of
	B when B > 16 -> {error,"illegal base"};
	B ->
	    case base(string:substr(Cs, H+1), B, 0) of
		error -> {error,"illegal based number"};
		N -> {token,{integer,L,N}}
	    end
    end.

base([C|Cs], Base, SoFar) when C >= $0, C =< $9, C < Base + $0 ->
    Next = SoFar * Base + (C - $0),
    base(Cs, Base, Next);
base([C|Cs], Base, SoFar) when C >= $a, C =< $f, C < Base + $a - 10 ->
    Next = SoFar * Base + (C - $a + 10),
    base(Cs, Base, Next);
base([C|Cs], Base, SoFar) when C >= $A, C =< $F, C < Base + $A - 10 ->
    Next = SoFar * Base + (C - $A + 10),
    base(Cs, Base, Next);
base([C|Cs], Base, SoFar) -> error;
base([], Base, N) -> N.

cc_convert([$$,$\\|Cs]) ->
    hd(string_escape(Cs));
cc_convert([$$,C]) -> C.

string_gen([$\\|Cs]) ->
    string_escape(Cs);
string_gen([C|Cs]) ->
    [C|string_gen(Cs)];
string_gen([]) -> [].

string_escape([O1,O2,O3|S]) when
  O1 >= $0, O1 =< $7, O2 >= $0, O2 =< $7, O3 >= $0, O3 =< $7 ->
    [(O1*8 + O2)*8 + O3 - 73*$0|string_gen(S)];
string_escape([$^,C|Cs]) ->
    [C band 31|string_gen(Cs)];
string_escape([C|Cs]) when C >= $\000, C =< $\s ->
    string_gen(Cs);
string_escape([C|Cs]) ->
    [escape_char(C)|string_gen(Cs)].

escape_char($n) -> $\n;				%\n = LF
escape_char($r) -> $\r;				%\r = CR
escape_char($t) -> $\t;				%\t = TAB
escape_char($v) -> $\v;				%\v = VT
escape_char($b) -> $\b;				%\b = BS
escape_char($f) -> $\f;				%\f = FF
escape_char($e) -> $\e;				%\e = ESC
escape_char($s) -> $\s;				%\s = SPC
escape_char($d) -> $\d;				%\d = DEL
escape_char(C) -> C.
