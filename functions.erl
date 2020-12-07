-module(functions).
-export([first/1,second_list/1,second/1, shuffle/1,getMaxAge/1,getMinAge/1,orderByAge/2,keep_freshest_entrie/3,remove_old1/4,remove_head/3,remove/2,remove_random/2,lengthh/1,remove_old2/3,remove_head1/2,remove_random1/2]).

% this file contains functions that help to implement operation in active thread, passive thread and selectView

% return first element of a list
first([X|_]) ->
    X.

% return the list without the first element
second_list([_|X]) ->
    X.

% return the length of a list
lengthh([]) -> 0;
lengthh([_|XS]) -> 1 + lengthh(XS).

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

% keep the freshest entries of a view after a pull (remove duplicates)
keep_freshest_entrie([[{ID,Pid},Age]],[], Result) -> Result ++ [[{ID,Pid},Age]];
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


% remove H last elements and then call remove_old2 to do this
remove_old1([V|Vs],H,Result,C) -> 
    case lengthh([V|Vs]) < lists:min([lengthh([V|Vs])-C,H]) of
        true -> [V|Vs];
        false -> 
            case (H > lengthh([V|Vs]) -C) of
                true -> remove_old2([V|Vs],lengthh([V|Vs]) -(lengthh([V|Vs])-C),Result);
                false -> remove_old2([V|Vs],lengthh([V|Vs])-H,Result)
            end
    end.

remove_old2(_,0,Result) -> Result;
remove_old2([],_,Result) -> Result;
remove_old2([V|VS],HC,Result) ->
remove_old2(VS,HC-1,Result ++ [V]).


% remove the min(S, view.size-c) first element from the view by calling remove_head1
remove_head([V|VS],S,C) ->
    case (lengthh([V|VS]) - C) >= 0 of
        true ->
            case lengthh([V|VS]) =< lists:min([S,lengthh([V|VS])-C]) of
                true -> [V|VS];
                false -> remove_head1([V|VS],lists:min([S,lengthh([V|VS])-C]))
            end;
        false -> 
            [V|VS]
    end.

remove_head1(VS,0) ->VS;
remove_head1([_|VS],Counter) ->
    remove_head1(VS,Counter-1).


remove_random(V,C) ->
    case lengthh(V) > C of
        true -> remove_random1(V,(lengthh(V) -C));
        false -> V
    end.

remove_random1(V,0) -> V;
remove_random1(V,C) -> 
    remove_random1(remove(lists:nth(rand:uniform(lengthh(V)), V),V),C-1).

remove(X, L) ->
    [Y || Y <- L, Y =/= X].
