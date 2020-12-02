-module(node).
-export([initThreads/8, join/2, getNeigs/2, listen/0, peerSelection/2, activeThread/4, passiveThread/2] ).
-import(lists, [append/2,min/1]).
-import(timer, [sleep/1]).
-import(functions,[first/1,second_list/1,second/1,shuffle/1,getMaxAge/1,getMinAge/1,orderByAge/2,keep_freshest_entrie/3,head1/3,remove_head/2,remove/2,remove_random/2,lengthh/1]).
-record(options, {c, healer, swapper, pull, mode, cycleInMs}).
-record(state, {id, master, buffer, view, passivePid, activePid, killed}).
-record(log, {id, log}).

initThreads(Id, Size, Select, WithPull, H, S, Ms, BootstrapPID) ->
    io:format("hello ~p~n", [Id]),
    St = #state{id = Id , master = self(), buffer = [], view = getView(getNeigs(BootstrapPID, Id), [], BootstrapPID), passivePid = -1, activePid = -1, killed = false},
    O = #options{c = Size, healer = H, swapper = S, pull = WithPull, mode = Select, cycleInMs = Ms},
    Log = #log{id = Id, log = []},
    ActiveThreadPid = spawn(node, activeThread, [St, O, Log, 0]),
    St2 = St#state{activePid = ActiveThreadPid},
    PassiveThreadPid = spawn(node, passiveThread, [St2,O]),
    St3 = St2#state{passivePid = PassiveThreadPid},
    PassiveThreadPid !{updateState, {St3}},
    ActiveThreadPid ! {updateState, {St3}},
    ActiveThreadPid ! {ping, PassiveThreadPid},
    listen(ActiveThreadPid, PassiveThreadPid).


listen() ->
  receive
    kill -> ok;
    {initThreads, {BootstrapPID, Id, Size, Select, WithPull, H, S, Ms}} -> 
        initThreads(Id, Size, Select, WithPull, H, S, Ms, BootstrapPID)
  end.

listen(ActiveThreadPid, PassiveThreadPid) ->
  receive
    {kill} -> ActiveThreadPid ! {kill};
    {recover, Elected} ->  ActiveThreadPid ! {recover, Elected};
    {cycle} -> ActiveThreadPid ! {cycle};
    {push, {From, PeerBuffer}} -> PassiveThreadPid ! {push, {From, PeerBuffer}};
    {pull, {From, PeerBuffer}} -> ActiveThreadPid ! {pull, {From, PeerBuffer}};
    {askPassive} -> ActiveThreadPid ! {ping, {PassiveThreadPid}};
    {updateState, {State, To}} ->
      To ! {updateState, {State}},
      receive
        {updated} ->  ok
      end
  end,
  listen(ActiveThreadPid, PassiveThreadPid).


activeThread(S, O, Log, Counter) -> 
  receive 
    {cycle} -> 
      if 
        (S#state.killed =/= true) ->
          io:format("log:: ~p ~p ~p~n", [S#state.id, Counter, S#state.view]),
          %Log = Log ++[C ounter,S#state.view],
        if 
          (S#state.passivePid =/= -1) ->
            Peer = peerSelection(O#options.mode, S#state.view),
            Buffer = [[{S#state.id,self()},0]],
            S2 = S#state{view = permute(S#state.view)},
            S3 = S2,%#state{view = heal(S2#state.view,O#options.healer, [])},
            Buffer = fillBuffer(S3#state.view, Buffer, ceil((O#options.c/2)) - 1),
            Peer ! {push, {self(), Buffer}}, 
            if
              (O#options.pull =:= true) -> 
                receive 
                    {pull, {From, PeerBuffer}} -> 
                      S4 = S3;%#state{view = selectView(S3#state.view, PeerBuffer, O#options.healer, O#options.swapper, O#options.c)}
                    {cycle} -> S4 = S3
                end,
                SF = S4#state{view = increaseAge(S4#state.view, [])};
              (O#options.pull =/= true) -> 
                SF = S3#state{view = increaseAge(S3#state.view, [])}
            end,
            SF#state.master ! {updateState, {SF, SF#state.passivePid}},

            activeThread(SF, O, Log, Counter+1);
          true -> 
            io:format("~p ~p Wait passivePid~n", [S#state.id, S#state.passivePid]),
            S#state.master ! {askPassive},
            activeThread(S, O, Log, Counter)
        end;
      (S#state.killed =:= true) -> 
        S#state.master ! {updateState, {S, S#state.passivePid}},

        activeThread(S, O, Log, Counter)
      end;

    {ping, PassiveThreadPid} ->
       S2 = S#state{passivePid = PassiveThreadPid},
       io:format("~p OK passivePid ~p ~n", [S#state.id, PassiveThreadPid]),
       S2#state.master ! {updateState, {S2, S2#state.passivePid}},
       activeThread(S2, O, Log, Counter);

    {kill} -> 
      S2 = S#state{killed = true},
      S2#state.master ! {updateState, {S2, S2#state.passivePid}},
      io:format("~p killed ~n", [S#state.id]),
      activeThread(S2, O, Log, Counter);

    {recover, Elected} -> 
      io:format("~p recovereEEEd ~n", [S#state.id]),
      NewView = [[Elected,0]],
      S2 = S#state{view = NewView},
      S3 = S2#state{killed = false},
      io:format("~p new view ~n", [S3#state.view]),
      S3#state.master ! {updateState, {S3, S3#state.passivePid}},
      activeThread(S3, O, Log, Counter);

    {updateState, {UpdatedState}} ->
      UpdatedState#state.master ! {updated},
      activeThread(UpdatedState, O, Log, Counter)

    end.


passiveThread(S,O) -> 
    receive 
        {updateState, {State}} -> 
          S2 = State,
          S2#state.master ! {updated},
          passiveThread(S2,O);

        {push, {From, PeerBuffer}} -> 
          Buffer = [[{S#state.id,self()},0]],
          S2 = S#state{view = permute(S#state.view)},
          S3 = S2,%#state{view = heal(S2#state.view,O#options.healer, [])},
          Buffer = fillBuffer(S3#state.view, Buffer, ceil((O#options.c/2)) - 1),
          From ! {pull, {self(), Buffer}}, 
          S4 = S3,%#state{view = selectView(S3#state.view, PeerBuffer, O#options.healer, O#options.swapper, O#options.c)},
          S5 = S4#state{view = increaseAge(S4#state.view, [])},
          S5#state.master ! {updateState, {S5, S5#state.activePid}},
          passiveThread(S5,O)
    end.




fillBuffer([], Buffer, Count) -> ok;
fillBuffer([H|T], Buffer, Count) ->
    if 
      (Count == 0) -> ok;
      (Count > 0) ->
        NewBuffer = Buffer ++ [H],
        fillBuffer(T, NewBuffer, Count-1)
    end,
    Buffer.

join(BootServerPid, NodePid) ->
  BootServerPid ! { join, {self(), NodePid} },
  receive
    { joinOk, NodeId } ->
      NodeId
  end.

getNeigs(BootServerPid, NodeId) ->
  BootServerPid ! { getPeers, { self(), NodeId } },
  receive
    { getPeersOk, Neigs } -> Neigs
  end.

getView({[]}, View, BootServerPid) -> View;
getView({[H|T]}, View, BootServerPid) ->
  if H =/= nil -> 
    NewView = [[{H,getNeigPid(H, BootServerPid)}, 0]] ++ View,
    getView({T}, NewView, BootServerPid);
    H =:= nil -> 
    getView({T}, View, BootServerPid)
  end. 


getNeigPid(NeigID, BootServerPid) ->
  BootServerPid ! { getPeerPid, { self(), NeigID } },
  receive
    { getPeerPidOk, {PID} } ->  PID
  end.




%TODO



selectView(View, Buffer, H, S, C) ->
    remove_random(remove_head(head1(heal(keep_freshest_entrie(View ++ Buffer,[],[]),lists:min([lengthh(View)-C,H]),[]),lists:min([lengthh(heal(keep_freshest_entrie(View ++ Buffer,[],[]),lists:min([lengthh(View)-C,H]),[]))-C,H]),[]),lists:min([lengthh(head1(heal(keep_freshest_entrie(View ++ Buffer,[],[]),lists:min([lengthh(View)-C,H]),[]),lists:min([lengthh(View)-C,H]),[]))-C,S])),lengthh(remove_head(head1(heal(keep_freshest_entrie(View ++ Buffer,[],[]),lists:min([lengthh(View)-C,H]),[]),lists:min([lengthh(heal(keep_freshest_entrie(View ++ Buffer,[],[]),lists:min([lengthh(View)-C,H]),[]))-C,H]),[]),lists:min([lengthh(head1(heal(keep_freshest_entrie(View ++ Buffer,[],[]),lists:min([lengthh(View)-C,H]),[]),lists:min([lengthh(View)-C,H]),[]))-C,S])))-C).

% increase age of every element in a view
increaseAge([],Acc) -> Acc;
increaseAge([[{ID,Pid},Age]|VS], Acc) -> 
    increaseAge(VS,Acc ++ [[{ID,Pid},Age +1]]).


% return random node from the view
peerSelection(rand, View) -> second(first(lists:nth(rand:uniform(length(View)),View)));

% return node with highest age in the view
peerSelection(tail,[V]) -> second(first(V));

peerSelection(tail,[V,V1|VS]) ->
    case second_list(V) >= second_list(V1) of
        true -> peerSelection(tail,[V|VS]);
       
        false -> peerSelection(tail,[V1|VS])
    end.
    
permute(View) ->
    shuffle(View). %provisoire

% move H oldest items to the end of the view
heal(View,0,Acc) -> 
    View ++ [orderByAge(Acc,[])];
heal([V|VS], H, Acc) ->
    heal(lists:filter(fun (Elem) -> not lists:member(Elem, [getMaxAge([V|VS])]) end, [V|VS] ), H-1, Acc ++ [getMaxAge([V|VS])]).

swap(view,swapper) ->
    view.

