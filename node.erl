-module(node).
-export([initThreads/8, join/2, getNeigs/2, listen/0, peerSelection/2, activeThread/4, passiveThread/2, clock/1] ).
-import(lists, [append/2]).
-import(timer, [sleep/1]).
-import(functions,[first/1,second_list/1,second/1,shuffle/1]).
-record(options, {c, healer, swapper, pull, mode, cycleInMs}).
-record(state, {id, buffer, view, passivePid, activePid, killed}).
-record(log, {id, log}).

initThreads(Id, Size, Select, WithPull, H, S, Ms, BootstrapPID) ->
    io:format("hello ~p~n", [Id]),
    St = #state{id = Id , buffer = [], view = getView(getNeigs(BootstrapPID, Id), [], BootstrapPID), passivePid = -1, activePid = -1, killed = false},
    O = #options{c = Size, healer = H, swapper = S, pull = WithPull, mode = Select, cycleInMs = Ms},
    Log = #log{id = Id, log = []},
    ActiveThreadPid = spawn(node, activeThread, [St, O, Log, 0]),
    St#state{activePid = activeThreadPid},
    PassiveThreadPid = spawn(node, passiveThread, [St,O]),
    ActiveThreadPid ! {ping, PassiveThreadPid},
    spawn(node, clock, [self()]),
    listen(ActiveThreadPid).


clock(PeerPid) ->
  sleep(1000),
  PeerPid ! {cycle},
  clock(PeerPid).

listen() ->
  receive
    kill -> ok;
    {initThreads, {BootstrapPID, Id, Size, Select, WithPull, H, S, Ms}} -> 
        initThreads(Id, Size, Select, WithPull, H, S, Ms, BootstrapPID)
  end.

listen(ActiveThreadPid) ->
  receive
    kill -> ActiveThreadPid ! kill;
    recover -> ActiveThreadPid ! {recover, {electedPeer}};
    {cycle} -> ActiveThreadPid ! {cycle}
  end,
  listen(ActiveThreadPid).


activeThread(S, O, Log, Counter) -> 
  receive 
    {cycle} -> 
      if (S#state.killed =/= true) ->
        io:format("Id: ~p counter: ~p  view: ~p~n", [S#state.id, Counter, S#state.view]),
        %Log = Log ++[Counter,S#state.view],
        %if (S#state.passivePid =/= -1) ->
          %Peer = peerSelection(O#options.mode, S#state.view),
          %Buffer = [[self(),0]],
          %S#state{view = permute(S#state.view)},
          %S#state{view = heal(S#state.view,O#options.healer)},
          %Buffer = fillBuffer(S#state.view, Buffer, (O#options.c/2) - 1)
          %Peer ! {push, self(), Buffer}, 
          %if (O#options.pull =:= true) -> 
          %    receive 
          %        {push, Buffer} -> #state.view = selectView(S3#state.view, Buffer, O#options.healer, O#options.swapper, O#options.c)
          %    end
          %end
          %S#state{view = increaseAge(S#state.view)},
          %S#state.passivePid ! {updateState, S}
        %end,
        activeThread(S, O, Log, Counter+1)
      end;
    {ping, PassiveThreadPid} ->
       S#state{passivePid = PassiveThreadPid},
       activeThread(S, O, Log, Counter);
    {kill} -> 
      S#state{killed = true},
      io:format("~p Killed", [S#state.id]),
      activeThread(S, O, Log, Counter);
    {recover, {electedPeer}} -> 
      NewView = [[electedPeer,0]],
      S#state{view = NewView},
      activeThread(S, O, Log, Counter);
    {updateState, UpdatedState} ->
      activeThread(UpdatedState, O, Log, Counter)
    end.



fillBuffer([H|T], Buffer, Count) ->
    if 
      (Count == 0) -> ok;
      (Count > 0) ->
        Buffer = Buffer + H,
        fillBuffer(T, Buffer, Count-1)
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

passiveThread(state,options) -> 
    receive 
        {updateState, {State}} -> ok
    end.

selectView(view, buffer, h, swapper, c) -> view.

% increase age of every element in a view
increaseAge([],Acc) -> Acc;
increaseAge([[{ID,Pid},Age]|VS], Acc) -> 
    increaseAge(VS,Acc ++ [{ID,Pid},Age +1]).


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

heal(view, heal) ->
    view.

swap(view,swapper) ->
    view.

