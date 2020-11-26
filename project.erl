- module(project).
- import(bootstrap_server, [listen/2]).
- import(node, [join/2, getNeigs/3, listen/0, initThread/7]).
- export([launch/1]).

makeNet(N, BootServerPid) -> makeNet(N, BootServerPid, [], 0, []).

makeNet(N, BootServerPid, Net, Counter, NetList) ->
  NodePid = spawn(node, listen, []),
  NodeId = node:join(BootServerPid),
  Node = { NodeId, NodePid },
  NetList = NetList ++ [Node],
  if
    N =/= Counter + 1 ->
      makeNet(N, BootServerPid, Net ++ [ Node ], Counter + 1, NetList ++ [NodePid]);
    N =:= Counter + 1 ->
      Net ++ [ Node ]
  end,
  NetList.



startNet([]) -> ok;
startNet([[ID|PID]|T]) ->
    PID ! node:initThread(ID, 3, head, true, 2, 3, 1),    %change options here
    startNet(T).

launch(N) ->
  % Creates server with an empty tree
  BootServerPid = spawn(bootstrap_server, listen, [ 0, {} ]),
  NetList = makeNet(N, BootServerPid),
  startNet(NetList).