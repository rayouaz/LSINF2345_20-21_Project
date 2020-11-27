- module(project).
- import(bootstrap_server, [listen/2]).
- import(lists, [concat/1]).

- import(node, [join/2, getNeigs/3, listen/0, initThread/8]).
- export([launch/1]).

makeNet(N, BootServerPid) -> makeNet(N, BootServerPid, [], 0).

makeNet(N, BootServerPid, Net, Counter) ->
  NodePid = spawn(node, listen, []),
  NodeId = node:join(BootServerPid),
  Node = { NodeId, NodePid },
  if
    N =/= Counter + 1 ->
      NewNet = makeNet(N, BootServerPid, Net ++ [ Node ], Counter + 1);
    N =:= Counter + 1 ->
      NewNet = Net ++ [ Node ],
      io:format("Hi2 ~p~n", [Net])

  end,
  NewNet.


startNet([], BootServerPid) -> ok;
startNet([{ID,PID}|T], BootServerPid) ->
    io:format("Hided~p~n", [PID]),
    PID ! {initThreads, {BootServerPid, ID, 3, head, true, 2, 3, 100}},    %change options here ID,size,mode,pull,H,s,cycleInMs
    startNet(T, BootServerPid).

launch(N) ->
  % Creates server with an empty tree
  BootServerPid = spawn(bootstrap_server, listen, [ 0, {} ]),

  NetList = makeNet(N, BootServerPid),
  io:format("List ~p~n", [NetList]),

  startNet(NetList, BootServerPid).