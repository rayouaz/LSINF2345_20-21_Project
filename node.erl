-module(node).
-export([initThreads/8, join/1, getNeigs/2, listen/0,first/1,second/1, second_list/1, peerSelection/2]).
-import(lists, [append/2]).
-import(timer, [sleep/1]).
-record(options, {c, healer, swapper, pull, mode, cycleInMs}).
-record(state, {id, buffer, view, passivePid, activePid}).
-record(log, {id, log}).

initThreads(Id, Size, Select, WithPull, H, S, Ms, BootstrapPID) ->
    io:format("hello ~n~p", [Id]),
    S = #state{id = Id , buffer = [], view = getNeigs(BootstrapPID, Id), passivePid = -1, activePid = -1},
    O = #options{c = Size, healer = H, swapper = S, pull = WithPull, mode = Select, cycleInMs = Ms},
    Log = #log{id = Id, log = []},
    ActiveThreadPid = spawn(node, activeThread, [S, O, Log, 0]),
    S#state{activePid = activeThreadPid},
    PassiveThreadPid = spawn(node, passiveThread, [S,O]),
    ActiveThreadPid ! {ping, PassiveThreadPid}.

activeThread(S, O, Log, Counter) -> 
    io:format("Id: ~n~p counter: ~n~p view: ~n~p", [S#state.id, Counter, S#state.view]),
    Log = Log++[Counter,S#state.view],
    timer:sleep(cycleInMs),
    PassiveThreadPid = S#state.passivePid,
    if (PassiveThreadPid == -1) ->
        receive
            {ping, PassiveThreadPid} ->
                S#state{passivePid = PassiveThreadPid}
        end
    end,
    PassiveThreadPid = S#state.passivePid,
    Peer = peerSelection(O#options.mode, S#state.view),
    Buffer = [[self(),0]],
    S = S#state{view = permute(S#state.view)},
    S = S#state{view = heal(S#state.view,O#options.healer)},
    Buffer = fillBuffer(S#state.view, Buffer, (O#options.c/2) - 1),
    Peer ! {push, self(), Buffer}, 
    if (O#options.pull =:= true) -> 
        receive 
            {push, Buffer} -> #state.view = selectView(S#state.view, Buffer, O#options.healer, O#options.swapper, O#options.c)
        end
    end,
    S = S#state{view = increaseAge(S#state.view)},
    PassiveThreadPid ! {updateState, S},
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

passiveThread(state,options) -> ok.

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
    
permute(view) ->
    lists:reverse(view). %provisoire

heal(view, heal) ->
    view.

swap(view,swapper) ->
    view.

% return first element of a list
first([X|_]) ->
    X.

second_list([_|X]) ->
    X.

% return second element of a tuple
second({_,Y}) ->
    Y.

