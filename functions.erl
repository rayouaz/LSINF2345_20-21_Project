-module(functions).
-export([first/1,second_list/1,second/1, shuffle/1,getMaxAge/1,getMinAge/1,orderByAge/2,keep_freshest_entrie/3,head1/3,remove_head/2,remove/2,remove_random/2]).

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

% arrange in increasing order by age elements of a view
orderByAge([],Acc) -> Acc;
orderByAge([V|VS], Acc) ->
    orderByAge(lists:filter(fun (Elem) -> not lists:member(Elem, [getMinAge([V|VS])]) end, [V|VS] ), Acc ++ [getMinAge([V|VS])]).

% keep the freshest entries of a view after a pullkeep_freshest_entrie([[{ID,Pid},Age]],[], Result) -> Result ++ [[{ID,Pid},Age]];
keep_freshest_entrie([[{ID,Pid},Age]],Acc, Result) -> keep_freshest_entrie(Acc, [], Result ++ [[{ID,Pid},Age]]);
keep_freshest_entrie([[{ID,Pid},Age], [{ID1,Pid1},Age1]|VS], Acc, Result) ->
    if
        Pid =:= Pid1 ->
            if 
                Age =< Age1 ->
                    keep_freshest_entrie([[{ID,Pid},Age]|VS], Acc, Result);
                Age > Age1 ->
                    keep_freshest_entrie([[{ID1,Pid1},Age1]|VS], Acc, Result)
            end;

        Pid =/= Pid1 ->
            keep_freshest_entrie([[{ID,Pid},Age]|VS], Acc ++ [[{ID1,Pid1},Age1]], Result)
    end.

head1(_,0,Result) -> Result;
head1([V|Vs],Counter,Result) ->
    head1(Vs,Counter-1,Result ++ [V]).

remove_head(V,0) -> V;
remove_head([V,V1|VS],S) ->
    remove_head([V1|VS],S-1).

remove(X, L) ->
    [Y || Y <- L, Y =/= X].

remove_random(V,0) -> V;
remove_random(V,Counter) ->
    remove_random(remove(lists:nth(rand:uniform(length(V)), V),V),Counter-1).
