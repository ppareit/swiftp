Nonterminals array element elements object members member arrayobj.

Terminals '{' '}' '[' ']' string ',' ':' integer float true false null.

Rootsymbol element.

arrayobj -> array : '$1'.
arrayobj -> object : '$1'.

object -> '{' members '}' : {'$2'}.
object -> '{' '}' : {[]}.

members -> member ',' members : ['$1' | '$3'].
members -> member : ['$1'].

member -> string ':' element : {list_to_binary(element(3, '$1')),'$3'}.

array -> '[' elements ']' : '$2'.
array -> '[' ']' : [].

elements -> element ',' elements : ['$1' | '$3'].
elements -> element : ['$1'].

element -> string : list_to_binary(element(3, '$1')).
element -> array : '$1'.
element -> object : '$1'.
element -> integer : element(3, '$1').
element -> float : element(3, '$1').
element -> true : element(1, '$1').
element -> false : element(1, '$1').
element -> null : element(1, '$1').