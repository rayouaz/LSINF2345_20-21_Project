-module(node).
<<<<<<< HEAD
-export([initThreads/8, join/1, getNeigs/2, listen/0, peerSelection/2, activeThread/4, passiveThread/2]).
=======
-export([initThreads/8, join/1, getNeigs/2, listen/0, peerSelection/2]).
>>>>>>> 6f5c22fd0b492953b9d10ef4bd7d17ddc9f64fc8
-import(lists, [append/2]).
-import(timer, [sleep/1]).
-import(functions,[first/1,second_list/1,second/1,shuffle/1]).
-record(options, {c, healer, swapper, pull, mode, cycleInMs}).
-record(state, {id, buffer, view, passivePid, activePid}).
-record(log, {id, log}).

initThreads(Id, Size, Select, WithPull, H, S, Ms, BootstrapPID) ->
    io:format("hello ~p~n", [Id]),
    St = #state{id = Id , buffer = [], view = getNeigs(BootstrapPID, Id), passivePid = -1, activePid = -1},
    O = #options{c = Size, healer = H, swapper = S, pull = WithPull, mode = Select, cycleInMs = Ms},
    Log = #log{id = Id, log = []},
    ActiveThreadPid = spawn(node, activeThread, [St, O, Log, 0]),
    St#state{activePid = activeThreadPid},
    PassiveThreadPid = spawn(node, passiveThread, [St,O]),
    ActiveThreadPid ! {ping, PassiveThreadPid}.

activeThread(S, O, Log, Counter) -> 
    io:format("Id: ~p counter: ~p  view: ~p~n", [S#state.id, Counter, S#state.view]),
    %Log = Log ++[Counter,S#state.view],
    %if (PassiveThreadPid == -1) ->
    %    receive
    %        {ping, PassiveThreadPid} ->
    %            S#state{passivePid = PassiveThreadPid}
    %    end
    %end,
    sleep(1000),
    PassiveThreadPid = S#state.passivePid,

    PassiveThreadPid = S#state.passivePid,
    %Peer = peerSelection(O#options.mode, S#state.view),
    Buffer = [[self(),0]],
    %S2 = S#state{view = permute(S#state.view)},
    %S3 = S2#state{view = heal(S2#state.view,O#options.healer)},
    %Buffer = fillBuffer(S3#state.view, Buffer, (O#options.c/2) - 1),
    %Peer ! {push, self(), Buffer}, 
    %if (O#options.pull =:= true) -> 
    %    receive 
    %        {push, Buffer} -> #state.view = selectView(S3#state.view, Buffer, O#options.healer, O#options.swapper, O#options.c)
    %    end
    %end
    %S4 = S3#state{view = increaseAge(S3#state.view)},
    %PassiveThreadPid ! {updateState, S4},
    activeThread(S, O, Log, Counter+1).

fillBuffer([H|T], Buffer, Count) ->
    if (Count == 0) -> ok
    end,
    if (Count > 0) ->
        Buffer = Buffer + H,
        fillBuffer(T, Buffer, Count-1)
    end,
    Buffer.

join(BootServerPid) ->
  BootServerPid ! { join, self() },
  receive
    { joinOk, NodeId } ->
      NodeId
  end.

getNeigs(BootServerPid, NodeId) ->
  BootServerPid ! { getPeers, { self(), NodeId } },
  receive
    { getPeersOk, Neigs } ->  Neigs
  end.

listen() ->
  receive
    kill -> ok;
    {initThreads, {BootstrapPID, Id, Size, Select, WithPull, H, S, Ms}} -> 
        initThreads(Id, Size, Select, WithPull, H, S, Ms, BootstrapPID)
  end.



%TODO

passiveThread(state,options) -> 
    receive 
        {updateState, {State}} -> ok
    end.

selectView(view, buffer, h, swapper, c) -> view.
increaseAge(view) -> view.


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

