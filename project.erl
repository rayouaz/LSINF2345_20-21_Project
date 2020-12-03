- module(project).
- import(bootstrap_server, [listen/3]).
- import(lists, [concat/1, reverse/1]).
-import(timer, [sleep/1]).
- import(node, [join/2, getNeigs/2, listen/0, initThread/9]).
- export([launch/1, normal/1]).

makeNet(N, BootServerPid) -> makeNet(N, BootServerPid, [], 0).

makeNet(N, BootServerPid, Net, Counter) ->
  NodePid = spawn(node, listen, []),
  NodeId = node:join(BootServerPid, NodePid),

  Node = { NodeId, NodePid },
  if
    N =/= Counter + 1 ->
      NewNet = makeNet(N, BootServerPid, Net ++ [ Node ], Counter + 1);
    N =:= Counter + 1 ->
      NewNet = Net ++ [ Node ]
  end,
  NewNet.


startNet([], BootServerPid, CycleInMs, Counter) -> ok;
startNet([{ID,PID}|T], BootServerPid, CycleInMs, Counter) ->
    PID ! {initThreads, {BootServerPid, ID, 8, rand, true, 3, 3, CycleInMs, Counter}},    %change options here ID,size,mode,pull,H,s,cycleInMs
    startNet(T, BootServerPid, CycleInMs, Counter).

cycle(NetList,0,CycleInMs,NetList) -> ok;
cycle([{ID,PID}|T], N, CycleInMs, NetList) ->
  PID ! {cycle},
  cycle(T, N, CycleInMs, NetList);
cycle([], N, CycleInMs, NetList) ->
  sleep(CycleInMs),
  cycle(NetList, N-1, CycleInMs, NetList).

normal(N) ->
  % Creates server with an empty tree
  BootServerPid = spawn(bootstrap_server, listen, [ 0, {}, [] ]),
  %NetList = makeNet(N, BootServerPid),
  CycleInMs = 1000,
  FourtyPercentLen = floor(N*0.4),
  TwentyPercentLen = floor(N*0.2),

  Net1 = makeNet(N, BootServerPid),
  startNet(Net1, BootServerPid, CycleInMs,0),
  cycle(Net1, 30, CycleInMs, Net1).



launch(N) ->
  % Creates server with an empty tree
  BootServerPid = spawn(bootstrap_server, listen, [ 0, {}, [] ]),
  %NetList = makeNet(N, BootServerPid),
  CycleInMs = 1000,
  FourtyPercentLen = floor(N*0.4),
  TwentyPercentLen = floor(N*0.2),

  Net1 = makeNet(FourtyPercentLen, BootServerPid),
  PhaseOne = getSublist(Net1, 0, FourtyPercentLen,0,[]),
  startNet(PhaseOne, BootServerPid, CycleInMs,0),
  cycle(PhaseOne, 30, CycleInMs, Net1),

  Net2 =  makeNet(FourtyPercentLen+TwentyPercentLen, BootServerPid, Net1, FourtyPercentLen), 
  PhaseTwo = getSublist(Net2, FourtyPercentLen, FourtyPercentLen+TwentyPercentLen, 0, []),
  startNet(PhaseTwo, BootServerPid, CycleInMs,30),
  PhaseOneTwo = lists:append(PhaseOne,PhaseTwo),
  cycle(PhaseOneTwo, 30, CycleInMs, Net2),

  Net3 =  makeNet(FourtyPercentLen+(TwentyPercentLen*2), BootServerPid, Net2, FourtyPercentLen+TwentyPercentLen), 
  PhaseThree = getSublist(Net3, FourtyPercentLen+TwentyPercentLen, FourtyPercentLen+(TwentyPercentLen*2), 0, []),
  startNet(PhaseThree, BootServerPid, CycleInMs,60),
  PhaseOneTwoThree = lists:append(PhaseOneTwo, PhaseThree),
  cycle(PhaseOneTwoThree, 30, CycleInMs, Net3),

  Net4 =  makeNet(FourtyPercentLen+(TwentyPercentLen*3), BootServerPid, Net3, FourtyPercentLen+(TwentyPercentLen*2)), 
  PhaseFour = getSublist(Net4, FourtyPercentLen+(TwentyPercentLen*2), FourtyPercentLen+(TwentyPercentLen*3), 0, []),
  startNet(PhaseFour, BootServerPid, CycleInMs,90),
  NetList = Net4,
  cycle(NetList, 30, CycleInMs, NetList),

  KilledProcess = kill(NetList, [], ceil(N*0.6)),
  cycle(NetList, 30 , CycleInMs, NetList),
  RecoverList = getSublist(KilledProcess, 0,floor(length(KilledProcess)*0.6),0 , []),
  recover(RecoverList, first(reverse(NetList))),
  cycle(NetList, 30 , CycleInMs, NetList),
  kill(NetList, [], N).


kill([], Killed, 0) -> ok;
kill([{ID,PID}|T], Killed, 0) -> Killed;
kill([{ID,PID}|T], Killed, Deaths) ->
  PID ! {kill},
  Killed2 = Killed ++ [{ID,PID}],
  kill(T,Killed2,Deaths-1).
  
recover([], Elected) -> ok;
recover([{ID,PID}|T], Elected) -> 
  %io:format("recover ~p ~n", [PID]),
  PID ! {recover, Elected},
  recover(T,Elected).

first([X|_]) ->
    X.

getSublist([], Start,End, Count, ReturnList) -> ReturnList;
getSublist([H|T], Start,End, Count, ReturnList) ->
  if 
    Count =:= End ->
      ReturnList;
    Count >= Start ->
      NewReturnList = ReturnList++[H],
      getSublist(T, Start,End, Count+1, NewReturnList) ;
    Count < Start ->
      getSublist(T, Start,End, Count+1, ReturnList)
  end.