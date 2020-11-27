-module(functions).
-export([first/1,second_list/1,second/1, shuffle/1]).

% return first element of a list
first([X|_]) ->
    X.

second_list([_|X]) ->
    X.

% return second element of a tuple
second({_,Y}) ->
    Y.

% return a pertmuted list
shuffle( List ) ->
    jumble( List, [] ).

jumble( [], Permuted_List ) ->
    Permuted_List;
jumble( List, Permuted_List ) ->
    Index = random:uniform( length(List) ) -1,
    {Left, [Element | Right]} = lists:split( Index, List ),
    jumble( lists:append( Left, Right ), [Element | Permuted_List] ).