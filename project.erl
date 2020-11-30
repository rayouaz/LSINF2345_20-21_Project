- module(project).
- import(bootstrap_server, [listen/3]).
- import(lists, [concat/1, reverse/1]).
-import(timer, [sleep/1]).
- import(node, [join/2, getNeigs/2, listen/0, initThread/8]).
- export([launch/1]).

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


startNet([], BootServerPid, CycleInMs) -> ok;
startNet([{ID,PID}|T], BootServerPid, CycleInMs) ->
    PID ! {initThreads, {BootServerPid, ID, 7, rand, true, 2, 3, CycleInMs}},    %change options here ID,size,mode,pull,H,s,cycleInMs
    startNet(T, BootServerPid, CycleInMs).

launch(N) ->
  % Creates server with an empty tree
  BootServerPid = spawn(bootstrap_server, listen, [ 0, {}, [] ]),
  NetList = makeNet(N, BootServerPid),
  CycleInMs = 1000,
  startNet(NetList, BootServerPid, CycleInMs),
  sleep(CycleInMs*10),
  KilledProcess = kill(NetList, [], ceil(N*0.6)),
  sleep(CycleInMs*10),
  recover(KilledProcess, first(reverse(NetList))),
  sleep(CycleInMs*10),
  kill(NetList, [], N).

kill([], Killed, 0) -> ok;
kill([{ID,PID}|T], Killed, 0) -> Killed;
kill([{ID,PID}|T], Killed, Deaths) ->
  PID ! {kill},
  Killed2 = Killed ++ [{ID,PID}],
  kill(T,Killed2,Deaths-1).
  
recover([], Elected) -> ok;
recover([{ID,PID}|T], Elected) -> 
  io:format("recover ~p ~n", [PID]),
  PID ! {recover, Elected},
  recover(T,Elected).

first([X|_]) ->
    X.