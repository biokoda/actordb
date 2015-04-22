% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at http://mozilla.org/MPL/2.0/.

% Execute with: ./detest test/repl_test.erl
-module(dist_test).
-export([cfg/1,setup/1,cleanup/1,run/1]).
-export([killconns/0]).
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
	case Args of
		["partition"] ->
			Nodes = [?ND1,?ND2,?ND3,?ND4,?ND5],
			Groups = [[{name,"grp1"},{nodes,[node1]}],[{name,"grp2"},{nodes,[node2]}],[{name,"grp3"},{nodes,[node3]}],
          [{name,"grp4"},{nodes,[node4]}],[{name,"grp5"},{nodes,[node5]}]];
		["random"] ->
			Nodes = [?ND1,?ND2,?ND3,?ND4,?ND5],
			Groups = [[{name,"grp1"},{nodes,[node1,node2,node3,node4,node5]}]]
	end,
	[
		% these dtl files get nodes value as a parameter and whatever you add here.
		{global_cfg,[{"test/etc/nodes.yaml",[{groups,Groups}]},
		             % schema does not need additional any parameters, but we do have to rename it
		             {{"test/etc/simple_schema.yaml","schema.yaml"},[]}]},
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
	[detest:cmd(Nd,"iptables --flush") || Nd <- [?ND1,?ND2,?ND3,?ND4,?ND5]],
	ok.

run(Param) ->
	case butil:ds_val(args,Param) of
		[TestType|_] ->
			run(TestType,Param);
		[] ->
			run("partition",Param)
	end.

run("partition",Param) ->
	[Nd1,Nd2,Nd3,Nd4,Nd5|_] = Ndl = butil:ds_vals([node1,node2,node3,node4,node5],Param),
	lager:info("Calling node to init ~p, connected to: ~p",[Nd1,nodes(connected)]),
	"ok" = rpc:call(Nd1,actordb_cmd,cmd,[init,commit,butil:ds_val(path,Param)++"/node1/etc"],10000),
	ok = wait_tree(Nd1,10000),
	timer:sleep(1000),

	lager:info("Isolating node1,node2, me ~p",[node()]),
	isolate([?ND1,?ND2],[?ND3,?ND4,?ND5]),
	rpc:call(Nd1,?MODULE,killconns,[]),
	rpc:call(Nd2,?MODULE,killconns,[]),
	rpc:call(Nd3,?MODULE,killconns,[]),
	rpc:call(Nd4,?MODULE,killconns,[]),
	rpc:call(Nd5,?MODULE,killconns,[]),
	timer:sleep(2000),

	% nd1 should be leader but now it can only communicate with node2
	{badrpc,_} = rpc:call(Nd1,actordb_sharedstate,write_global,[key,123],5000),
	lager:info("Abandoned call, trying in ~p",[Nd3]),
	[1,2,3,4,5,6,7,8,9,10] = rpc:call(Nd3,lists,seq,[1,10],5000),
	ok = rpc:call(Nd3,actordb_sharedstate,write_global,[key1,321],8000),


	%123 = rpc:call(Nd1,actordb_sharedstate,read,[<<"global">>,key],15000),
	321 = rpc:call(Nd3,actordb_sharedstate,read,[<<"global">>,key1],15000),
	lager:info("REACHED END SUCCESSFULLY"),
	ok;
run("random",Param) ->
	NWriters = 3,
	[Nd1,Nd2,Nd3,Nd4,Nd5|_] = Ndl = butil:ds_vals([node1,node2,node3,node4,node5],Param),
	"ok" = rpc:call(Nd1,actordb_cmd,cmd,[init,commit,butil:ds_val(path,Param)++"/node1/etc"],10000),
	ok = wait_tree(Nd1,10000),
	timer:sleep(1000),

	% Execute workers for every node. Eeach worker will send updates to its assigned node but they will all go
	%  to the same actor. Every worker has his own row that he is working in.
	Home = self(),
	Pids = [spawn_monitor(fun() -> incwriter(Home,Nd,0) end) || Nd <- Ndl],

	ok.


isolate([ToIsolate|T], [_|_] = IsolateFrom) ->
	[begin
		Cmd1 = "iptables -A INPUT -s "++butil:tolist(butil:ds_val(name,F))++" -m conntrack --ctstate NEW,ESTABLISHED,RELATED -j DROP",
		Cmd2 = "iptables -A OUTPUT -s "++butil:tolist(butil:ds_val(name,ToIsolate))++" -m conntrack --ctstate NEW,ESTABLISHED,RELATED -j DROP",
		lager:info("On node=~p, running: ~s",[butil:ds_val(name,ToIsolate),Cmd1]),
		lager:info("On node=~p, running: ~s",[butil:ds_val(name,F),Cmd2]),
		detest:cmd(ToIsolate,Cmd1),
		detest:cmd(F,Cmd2)
	end || F <- IsolateFrom],
	isolate(T,IsolateFrom);
isolate([],_) ->
	ok.


% Called on nodes. Kills all RPC connections.
killconns() ->
	L = supervisor:which_children(ranch_server:get_connections_sup(bkdcore_in)),
	[exit(Pid,stop) || {bkdcore_rpc,Pid,worker,[bkdcore_rpc]} <- L].


% Increment counter. Every 100 writes check if number that should be written is actually the one that is in the DB.
incwriter(Home,Nd,0) ->
	{ok,_} = exec([Nd],<<"actor type1(testactor) create;",
	                     "insert into tab values ('",(butil:tobin(Nd))/binary,"',1);">>),
	incwriter(Home,Nd,1);
incwriter(Home,Nd,N) ->
	checkhome(Home),
	case N rem 100 of
		0 ->
			case exec([Nd],<<"actor type1(testactor);",
			                 "select * from tab WHERE id='",(butil:tobin(Nd))/binary,"';">>) of
				{ok,[{columns,_},{rows,[{_,N}]}]} ->
					ok;
				{ok,[{columns,_},{rows,[{_,N1}]}]} ->
					lager:error("Value mismatch, should be=~p, is=~p",[N,N1]),
					exit(normal);
				{error,_} ->
					ok
			end;
		_ ->
			ok
	end,
	case exec([Nd],<<"actor type1(testactor);",
	                 "update tab set val=val+1 WHERE id='",(butil:tobin(Nd))/binary,"';">>) of
		{ok,_} ->
			incwriter(Home,Nd,N+1);
		{error,_Err} ->
			lager:info("Failed update ~p",[Nd]),
			incwriter(Home,Nd,N)
	end.
