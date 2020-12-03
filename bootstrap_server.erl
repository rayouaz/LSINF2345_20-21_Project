- module(bootstrap_server).
- import(tree, [add/2, getNeigs/2]).
- export([listen/3]).





listen(NodeId, Tree, PidList) ->
  io:format("Bootstrap server is listening...~n", []),
  receive
    { join, {From, NodePid} } ->
      NewTree = tree:add(NodeId , Tree),
      NewPidList =  [{NodeId, NodePid}] ++ PidList,
      From ! { joinOk, NodeId },
      listen(NodeId + 1, NewTree, NewPidList);
    { getPeers, { From, ForNodeId } } ->
      Neigs = tree:getNeigs(ForNodeId, Tree),
      From ! { getPeersOk, { Neigs }  },
      listen(NodeId, Tree, PidList);
    { getPeerPid, { From, ForId } } ->
      Pid = getPid(ForId,PidList),
      From ! { getPeerPidOk, { Pid }  },
      listen(NodeId, Tree, PidList)
  end.

getPid(ForID, [{ID,PID}|T]) ->
  if  
    ID =:= ForID ->
     PID;
    ID =/= ForID ->
     getPid(ForID, T)
  end;

getPid(ForID, []) -> [].


