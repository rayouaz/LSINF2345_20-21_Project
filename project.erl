- module(project).
- import(bootstrap_server, [listen/3]).
- import(lists, [concat/1]).
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


startNet([], BootServerPid) -> ok;
startNet([{ID,PID}|T], BootServerPid) ->
    PID ! {initThreads, {BootServerPid, ID, 7, rand, true, 2, 3, 1000}},    %change options here ID,size,mode,pull,H,s,cycleInMs
    startNet(T, BootServerPid).

launch(N) ->
  % Creates server with an empty tree
  BootServerPid = spawn(bootstrap_server, listen, [ 0, {}, [] ]),

  NetList = makeNet(N, BootServerPid),
  startNet(NetList, BootServerPid).
  %KilledProcess = kill(NetList, 30),
  %Recovered = recover(KilledProcess, Elected),
  %cycle(60, NetList ++ Recovered).
