-module(node).
-export([initThreads/9, join/2, getNeigs/2, listen/0, peerSelection/2, activeThread/4, passiveThread/2,heal/4,selectView/5]).
-import(lists, [append/2,min/1]).
-import(timer, [sleep/1]).
-import(functions,[first/1,second_list/1,second/1,shuffle/1,getMaxAge/1,getMinAge/1,orderByAge/2,keep_freshest_entrie/3,head1/4,remove_head/3,remove/2,remove_random/2,lengthh/1,head2/3,remove_head1/2,remove_random1/2]).
-record(options, {c, healer, swapper, pull, mode, cycleInMs}).
-record(state, {id, master, buffer, view, passivePid, activePid, killed}).
-record(log, {id, log}).

initThreads(Id, Size, Select, WithPull, H, S, Ms, BootstrapPID, Counter) ->
    io:format("hello ~p~n", [Id]),
    St = #state{id = Id , master = self(), buffer = [], view = getView(getNeigs(BootstrapPID, Id), [], BootstrapPID), passivePid = -1, activePid = -1, killed = false},
    O = #options{c = Size, healer = H, swapper = S, pull = WithPull, mode = Select, cycleInMs = Ms},
    Log = #log{id = Id, log = []},
    ActiveThreadPid = spawn(node, activeThread, [St, O, Log, Counter]),
    St2 = St#state{activePid = ActiveThreadPid},
    PassiveThreadPid = spawn(node, passiveThread, [St2,O]),
    St3 = St2#state{passivePid = PassiveThreadPid},
    PassiveThreadPid !{updateState, {St3}},
    ActiveThreadPid ! {updateState, {St3}},
    listen(ActiveThreadPid, PassiveThreadPid).


listen() ->
  receive
    kill -> ok;
    {initThreads, {BootstrapPID, Id, Size, Select, WithPull, H, S, Ms, Counter}} -> 
        initThreads(Id, Size, Select, WithPull, H, S, Ms, BootstrapPID,Counter)
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
          Peer = peerSelection(O#options.mode, S#state.view),
          Buffer = [[{S#state.id,S#state.master},0]],
          S2 = S#state{view = permute(S#state.view)},
          S3 = S2#state{view = heal(S2#state.view,O#options.healer, [], S2#state.view)},
          Buffer2 = fillBuffer(S3#state.view, Buffer, ceil((O#options.c/2)) - 1),
          Peer ! {push, {S3#state.master, Buffer2}}, 
          if
            (O#options.pull =:= true) -> 
              receive 
                  {pull, {From, PeerBuffer}} -> 
                    S4 = S3#state{view = selectView(S3#state.view, PeerBuffer, O#options.healer, O#options.swapper, O#options.c)}
              after
                      100 -> S4 = S3
              end,
              SF = S4#state{view = increaseAge(S4#state.view, [])};
            (O#options.pull =/= true) -> 
              SF = S3#state{view = increaseAge(S3#state.view, [])}
          end,
          SF#state.master ! {updateState, {SF, SF#state.passivePid}},
          activeThread(SF, O, Log, Counter+1);
      (S#state.killed =:= true) -> 
        S#state.master ! {updateState, {S, S#state.passivePid}},
        activeThread(S, O, Log, Counter+1)
      end;

    {kill} -> 
      S2 = S#state{killed = true},
      S2#state.master ! {updateState, {S2, S2#state.passivePid}},
      io:format("~p killed ~n", [S#state.id]),
      activeThread(S2, O, Log, Counter);

    {recover, Elected} -> 
      io:format("~p recovered ~n", [S#state.id]),
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
          Buffer = [[{S#state.id,S#state.master},0]],
          S2 = S#state{view = permute(S#state.view)},
          S3 = S2#state{view = heal(S2#state.view,O#options.healer, [],S2#state.view)},
          Buffer2 = fillBuffer(S3#state.view, Buffer, ceil((O#options.c/2)) - 1),
          From ! {pull, {S3#state.master, Buffer2}}, 
          io:format("node ~p :: view before selectView: ~p  buffer before selectView: ~p~n", [S3#state.id ,S3#state.view, Buffer2 ]),
          S4 = S3#state{view = selectView(S3#state.view, PeerBuffer, O#options.healer, O#options.swapper, O#options.c)},
          io:format("node ~p :: view after selectView: ~p~n", [S4#state.id , S4#state.view]),
          S5 = S4#state{view = increaseAge(S4#state.view, [])},
          S5#state.master ! {updateState, {S5, S5#state.activePid}},
          passiveThread(S5,O)
    end.




fillBuffer([], Buffer, Count) -> Buffer;
fillBuffer([H|T], Buffer, Count) ->
    if 
      (Count == 0) -> Buffer2 = Buffer;
      (Count > 0) ->
        NewBuffer = Buffer ++ [H],
        Buffer2 = fillBuffer(T, NewBuffer, Count-1)
    end,
    Buffer2.

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
    remove_head(head1(heal(keep_freshest_entrie(View ++ Buffer,[],[]),H,[], keep_freshest_entrie(View ++ Buffer,[],[])),H,[],C),S,C).

% increase age of every element in a view
increaseAge([],Acc) -> Acc;
increaseAge([[{ID,Pid},Age]|VS], Acc) -> 
    increaseAge(VS,Acc ++ [[{ID,Pid},Age +1]]).


% return random node from the view
peerSelection(rand,[]) -> [];
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
heal([],_,_, []) -> [];
heal([],_,_, [C|CS]) -> [C|CS];
heal([],0,Acc, [C|CS]) -> [C|CS];
heal([V|VS],0,Acc, [C|CS]) -> [V|VS] ++ orderByAge(Acc,[]);
heal([V|VS], H, Acc, [C|CS]) ->
    case (lengthh([C|CS]) > H) of
        true -> heal(lists:filter(fun (Elem) -> not lists:member(Elem, [getMaxAge([V|VS])]) end, [V|VS] ), H-1, Acc ++ [getMaxAge([V|VS])], [C|CS]);
        false -> heal([],0,[V|VS],[C|CS])
    end.

swap(view,swapper) ->
    view.

