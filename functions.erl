-module(functions).
-export([first/1,second_list/1,second/1, shuffle/1,getMaxAge/1,getMinAge/1,orderByAge/2]).

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

% return oldest element of a view
getMaxAge([[{ID,Pid},Age]]) -> [{ID,Pid},Age];
getMaxAge([[{ID,Pid},Age], [{_,_},Age1] |VS]) when Age >= Age1 ->
    getMaxAge([[{ID,Pid},Age] |VS]);
getMaxAge([[{_,_},Age], [{ID,Pid},Age1] |VS]) when Age < Age1 ->
    getMaxAge([[{ID,Pid},Age1] |VS]).

% return youngest element of a view
getMinAge([[{ID,Pid},Age]]) -> [{ID,Pid},Age];
getMinAge([[{ID,Pid},Age], [{_,_},Age1] |VS]) when Age < Age1 ->
    getMinAge([[{ID,Pid},Age] |VS]);
getMinAge([[{_,_},Age], [{ID,Pid},Age1] |VS]) when Age >= Age1 ->
    getMinAge([[{ID,Pid},Age1] |VS]).

% arrange in increasing order elements of a view
orderByAge([],Acc) -> Acc;
orderByAge([V|VS], Acc) ->
    orderByAge(lists:filter(fun (Elem) -> not lists:member(Elem, [getMinAge([V|VS])]) end, [V|VS] ), Acc ++ [getMinAge([V|VS])]).
