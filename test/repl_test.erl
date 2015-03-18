% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at http://mozilla.org/MPL/2.0/.

% Execute with: ./detest test/repl_test.erl
-module(dist_test).
-export([cfg/1,setup/1,cleanup/1,run/1]).
%-export([killconns/0,call_start/1,call_receive/1]).
-define(INF(F,Param),io:format("~p ~p:~p ~s~n",[ltime(),?MODULE,?LINE,io_lib:fwrite(F,Param)])).
-define(INF(F),?INF(F,[])).
-define(NUMACTORS,100).
numactors() ->
	?NUMACTORS.
-include_lib("eunit/include/eunit.hrl").
-include("test_util.erl").
-include_lib("kernel/include/file.hrl").

% These nodes should be running from LXC on local machine
-define(ND1,[{name,node1},{rpcport,45551},{ssh,"node1",22,"/opt/actordb",[{user,"root"}]}]).
-define(ND2,[{name,node2},{rpcport,45552},{ssh,"node2",22,"/opt/actordb",[{user,"root"}]}]).
-define(ND3,[{name,node3},{rpcport,45553},{ssh,"node3",22,"/opt/actordb",[{user,"root"}]}]).
-define(ND4,[{name,node4},{rpcport,45554},{ssh,"node4",22,"/opt/actordb",[{user,"root"}]}]).
-define(ND5,[{name,node5},{rpcport,45555},{ssh,"node5",22,"/opt/actordb",[{user,"root"}]}]).


cfg(Args) ->
	Nodes = [?ND1,?ND2,?ND3,?ND4,?ND5],
	Groups = [[{name,"grp1"},{nodes,[node1]}],[{name,"grp2"},{nodes,[node2]}],[{name,"grp3"},{nodes,[node3]}],
          [{name,"grp4"},{nodes,[node4]}],[{name,"grp5"},{nodes,[node5]}]],
	[
		% these dtl files get nodes value as a parameter and whatever you add here.
		{global_cfg,[{"test/etc/nodes.yaml",[{groups,Groups}]},
		             % schema does not need additional any parameters.
		             "test/etc/schema.yaml"]},
		% Config files per node. For every node, its property list is added when rendering.
		% if name contains app.config or vm.args it gets automatically added to run node command
		% do not set cookie or name of node in vm.args this is set by detest
		{per_node_cfg,["test/etc/app.config"]},
		% cmd is appended to erl execute command, it should execute your app.
		% It can be set for every node individually. Add it to that list if you need it, it will override this value.
		{cmd,"-s actordb_core +S 2 +A 2"},

		{detest_name,'detest@home'},
		        
        % in ms, how long to wait to connect to node. If running with valgrind it takes a while.
         {connect_timeout,20000},
        
        % in ms, how long to wait for application start once node is started
         {app_wait_timeout,20000},
		
		% which app to wait for to consider node started
		{wait_for_app,actordb_core},
		% What RPC to execute for stopping nodes (optional, def. is {init,stop,[]})
		{stop,{actordb_core,stop_complete,[]}},
		{nodes,Nodes}
	].

% Before starting nodes
setup(Param) ->
	filelib:ensure_dir([butil:ds_val(path,Param),"/log/"]),
	butil:set_permission([butil:ds_val(path,Param),"/log"]).

% Nodes have been closed
cleanup(_Param) ->
	ok.

run(Param) ->
	[Nd1,Nd2,Nd3,Nd4,Nd5|_] = Ndl = butil:ds_vals([node1,node2,node3,node4,node5],Param),
	lager:info("Calling node to init ~p, connected to: ~p",[Nd1,nodes(connected)]),
	ok = rpc:call(Nd1,actordb_cmd,cmd,[init,commit,butil:ds_val(path,Param)++"/node1/etc"],10000),
	ok = wait_tree(Nd1,10000),
	timer:sleep(1000),
	
	lager:info("Isolating node1,node2, me ~p",[node()]),
	isolate([?ND1,?ND2],[?ND3,?ND4,?ND5]),
	%rpc:call(Nd1,?MODULE,killconns,[]),
	%rpc:call(Nd2,?MODULE,killconns,[]),
	%rpc:call(Nd3,?MODULE,killconns,[]),
	%rpc:call(Nd4,?MODULE,killconns,[]),
	%rpc:call(Nd5,?MODULE,killconns,[]),
	
	%damocles:isolate_between_interfaces([Nd1ip, Nd2ip], [Nd3ip,Nd4ip,Nd5ip]),
	
	% nd1 should be leader but now it can only communicate with node2
	{badrpc,_} = rpc:call(Nd1,actordb_sharedstate,write_global,[key,123],5000),
	lager:info("Abandoned call, trying in ~p",[Nd3]),
	{ok,_} = rpc:call(Nd3,actordb_sharedstate,write_global,[key1,321],2000),
	lager:info("Write success. Restoring network. Do we have abandoned write?"),
	%damocles:restore_all_interfaces(),
	%cleanup(1),
	123 = rpc:call(Nd1,actordb_sharedstate,read,[<<"global">>,key],15000),
	321 = rpc:call(Nd1,actordb_sharedstate,read,[<<"global">>,key1],15000),
	lager:info("REACHED END SUCCESSFULLY"),
	%{ok,_} = rpc:call(Nd1,?MODULE,call_start,[node2],10000),
	%{ok,_} = rpc:call(Nd1,?MODULE,call_start,[node3],10000),
	ok.


isolate([ToIsolate|T], [_|_] = IsolateFrom) ->
	[begin
		Cmd = "iptables -A INPUT -s "++butil:tolist(butil:ds_val(name,F))++" -j DROP",
		lager:info("On node=~p, running: ~s",[butil:ds_val(name,ToIsolate),Cmd]),
		detest:cmd(ToIsolate,Cmd)
	end || F <- IsolateFrom],
	isolate(T,IsolateFrom);
isolate([],_) ->
	ok.


% Called on nodes
%killconns() ->
%	L = supervisor:which_children(ranch_server:get_connections_sup(bkdcore_in)),
%	[exit(Pid,stop) || {bkdcore_rpc,Pid,worker,[bkdcore_rpc]} <- L].

% This module is loaded inside every executed node. So we can rpc to these functions on every node.
%call_start(Nd) ->
%	lager:info("Calling from=~p to=~p, at=~p, connected=~p~n",[node(), Nd, time(),nodes(connected)]),
	%{ok,_} = rpc:call(Nd,?MODULE,call_receive,[node()],1000).
%	{ok,_} = bkdcore_rpc:call(butil:tobin(Nd),{?MODULE,call_receive,[bkdcore:node_name()]}).

%call_receive(From) ->
%	lager:info("Received call on=~p from=~p, at=~p~n",[node(), From, time()]),
%	{ok,node()}.
