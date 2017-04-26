% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at http://mozilla.org/MPL/2.0/.
-module(actordb_console).
-export([main/1,cmd/1, map_print/1]).
% -include_lib("actordb_core/include/actordb.hrl").
-define(PROMPT,"actordb>").

-define(COMMANDS,delim()++"Databases:\n"++
"use config (use c)  initialize/add nodes and user account management\n"++
"use schema (use s)  set schema\n"++
"use actordb (use a) (default) run queries on database\n"++
delim()++
"Commands:\n"++
"open         (windows only) open and execute .sql file\n"++
"q            exit\n"++
"h            print this header\n"++
"commit (c)   execute transaction\n"++
"rollback (r) abort transaction\n"++
"print (p)    print transaction\n"
"show (s)     show schema\n"
"show status  show database status\n"
"show queries show currently running queries\n"
"show shards  show shards on node\n"++delim()).

delim() ->
	"*******************************************************************\n".

% curdb changed with use statements
% actordb - default can run queries directly
% config - for adding groups and nodes
% schema - for changing schema
-record(dp,{env = shell, curdb = actordb, req, resp, stop = false, buffer = [], wait = true,
	addr = "127.0.0.1", port = 33306, username = "", framed = false,
	password = "", execute, timeout_after = infinity, print, noshell = false}).

main(Args) ->
	register(home,self()),
	% For some reason we must call this to load crypto immediately.
	% Otherwise it fails when using wx.
	application:ensure_all_started(crypto),
	crypto:hash(sha, "asdf"),
	case os:type() of
		{win32,_} ->
			spawn(fun() -> (catch actordb_wxconsole:wxrun()),halt(1) end),
			timer:sleep(50),
			P = parse_args(#dp{env = wx}, Args);
		% {unix,darwin} ->
		% 	spawn(fun() -> (catch wxrun()),halt(1) end),
		% 	P = parse_args(#dp{env = wx}, Args);
		_ ->
			{ok,Comfile} = file:read_file("/tmp/comfile"),
			[Req,Resp] = binary:split(Comfile,<<"\n">>),
			file:delete("/tmp/comfile"),
			ReqPipe = open_port(binary_to_list(Req), [in,eof,binary]),
			RespPipe = open_port(binary_to_list(Resp), [out,eof,binary]),
			P = setpw(parse_args(#dp{req = ReqPipe, resp = RespPipe, env = shell},Args))
	end,
	dologin(P),
	case P#dp.execute of
		undefined ->
			case P#dp.curdb of
				actordb ->
					print(P,?COMMANDS),
					dopipe(P);
				_ ->
					change_prompt(P),
					print(P,?COMMANDS),
					dopipe(P)
			end;
		{script,Bin} ->
			cmd_lines(P,binary:split(Bin,<<"\n">>,[global])),
			halt(1);
		{qry,Q} ->
			send_query(P#dp{timeout_after = 2000},list_to_binary(Q))
	end.

setpw(#dp{password = prompt} = P) ->
	case dopipe(P#dp{timeout_after = 50}) of
		timeout ->
			print(P,"~~~~getpass"),
			Pw = dopipe(P),
			P#dp{password = Pw};
		Pw ->
			P#dp{password = Pw}
	end;
setpw(P) ->
	P.

dologin(P) ->
	application:stop(actordb_client),
	PoolInfo = [{size, 1}, {max_overflow, 5}],
	WorkerParams = [{hostname, P#dp.addr},
		{username, P#dp.username},
		{password, P#dp.password},
		{port,P#dp.port},
		{framed, P#dp.framed}
	],
	case actordb_client:start(PoolInfo,WorkerParams) of
		ok ->
			% print(P,"Connected to DB\n"),
			ok;
		{error,{login_failed,Msg}} when P#dp.env == wx ->
			print(P,Msg),
			wxproc ! dologin;
		Err ->
			print(P,"Connect/login error: ~p~n",[Err]),
			cmd(P,<<"q">>)
	end.



cmd_lines(P,[H|T]) ->
	case rem_spaces(H) of
		<<"//",_/binary>> ->
			cmd_lines(P,T);
		<<"%",_/binary>> ->
			cmd_lines(P,T);
		<<"--",_/binary>> ->
			cmd_lines(P,T);
		_ ->
			cmd_lines(cmd(P,H),T)
	end;
cmd_lines(P,[]) ->
	P.

rem_spaces(<<" ",X/binary>>) ->
	rem_spaces(X);
rem_spaces(<<"\n",X/binary>>) ->
	rem_spaces(X);
rem_spaces(<<"\r",X/binary>>) ->
	rem_spaces(X);
rem_spaces(X) ->
	X.

parse_args(P,["-h"|_]) ->
	L = "Flags:\n"++
	"  -h                   Print this help and exit.\n"++
	"  -u   <username>      Set username. You will be prompted for password. Not required if ActorDB is uninitalized.\n"++
	"  -pw  <password>      Set login password (optional). This will avoid the prompt.\n"++
	"  -f   <file>          Execute statements from file and exit.\n"++
	"  -use <database>      actordb (def), config or schema.\n"++
	"  -q   \"query\"         Execute query and exit.\n"++
	"  -noshell             Do not create a shell. Useful when running queries with -q.\n"++
	"  -framed              Use framed thrift protocol (must be enabled on server as well).\n"++
	"  -print <default|min|csv|csvh>\n",
	% "  -w   wait for commit to send query to actordb\n",
	print(P,"Call with: actordb_console -u username IP[:ThriftPort]\n"++L),
	halt(1);
parse_args(P,["-f",File|T]) ->
	{ok,F} = file:read_file(File),
	parse_args(P#dp{execute = {script,F}},T);
parse_args(P,["-u",Username|T]) ->
	case P#dp.password of
		[_|_] ->
			parse_args(P#dp{username = Username},T);
		_ ->
			parse_args(P#dp{username = Username, password = prompt},T)
	end;
parse_args(P,["-use",A|T]) when A == "a"; A == "actordb"; A == "actor" ->
	parse_args(P#dp{curdb = actordb},T);
parse_args(P,["-use",A|T]) when A == "c"; A == "config" ->
	parse_args(P#dp{curdb = config},T);
parse_args(P,["-use",A|T]) when A == "s"; A == "schema" ->
	parse_args(P#dp{curdb = schema},T);
parse_args(P,["-q",Q|T]) ->
	parse_args(P#dp{execute = {qry, Q}, wait = false},T);
parse_args(P,["-pw",Password|T]) ->
	parse_args(P#dp{password = Password},T);
parse_args(P,["-w"|T]) ->
	parse_args(P#dp{wait = true},T);
parse_args(P,["-framed"|T]) ->
	parse_args(P#dp{framed = true}, T);
parse_args(P,["-print", "min"|T]) ->
	parse_args(P#dp{print = min},T);
parse_args(P,["-print", "csv"|T]) ->
	parse_args(P#dp{print = csv},T);
parse_args(P,["-print", "csvh"|T]) ->
	parse_args(P#dp{print = csvh},T);
parse_args(P,["-print", _|T]) ->
	parse_args(P,T);
parse_args(P,["-noshell"|T]) ->
	parse_args(P#dp{noshell = true},T);
parse_args(P,["-"++Something|T]) ->
	print(P,"Unrecognized option: ~s",["-"++Something]),
	parse_args(P,T);
parse_args(P,[Addr|T]) ->
	case string:tokens(Addr,":") of
		[Address,Port] ->
			parse_args(P#dp{addr = Address, port = list_to_integer(Port)},T);
		[Address] ->
			parse_args(P#dp{addr = Address},T)
	end;
parse_args(P,[]) when P#dp.noshell, P#dp.execute == undefined ->
	halt(1);
parse_args(P,[]) ->
	P.

cmd(C) ->
	cmd(#dp{env = test},iolist_to_binary(C)).
cmd(P,<<";",Rem/binary>>) ->
	cmd(P,Rem);
cmd(P,<<>>) ->
	P;
cmd(P,<<"dim=",Dim/binary>>) ->
	case binary:split(Dim,<<"\n">>) of
		[DimBin,Rem] ->
			ok;
		[DimBin] ->
			Rem = <<>>
	end,
	case binary:split(DimBin,<<",">>) of
		[_Rows,_Cols] ->
			% print(P,"Rows=~p,Cols=~p",[_Rows,_Cols]);
			ok;
		_ ->
			ok
	end,
	cmd(P,Rem);
cmd(P,<<"h">>) ->
	print(P,?COMMANDS);
cmd(P,<<"s">>) ->
	cmd(P,<<>>,show);
cmd(P,<<"S">>) ->
	cmd(P,<<>>,show);
cmd(P,<<"c">>) ->
	cmd(P,<<>>,commit);
cmd(P,<<"C">>) ->
	cmd(P,<<>>,commit);
cmd(P,<<"p">>) ->
	cmd(P,<<>>,print);
cmd(P,<<"P">>) ->
	cmd(P,<<>>,print);
cmd(P,<<"r">>) ->
	cmd(P,<<>>,rollback);
cmd(P,<<"R">>) ->
	cmd(P,<<>>,rollback);
cmd(_P,<<"q">>) ->
	case whereis(wxproc) of
		undefined ->
			halt(1);
		_ ->
			wxproc ! stop,
			timer:sleep(200),
			halt(1)
	end;
cmd(P,Bin) when is_binary(Bin) ->
	cmd(P,Bin,actordb_sql:parse(Bin)).
cmd(P,Bin,Tuple) ->
	case Tuple of
		{use,<<"c">>} ->
			cmd(P,<<>>,{use,<<"config">>});
		{use,<<"s">>} ->
			cmd(P,<<>>,{use,<<"schema">>});
		{use,<<"a">>} ->
			cmd(P,<<>>,{use,<<"actordb">>});
		{use,<<"C">>} ->
			cmd(P,<<>>,{use,<<"config">>});
		{use,<<"S">>} ->
			cmd(P,<<>>,{use,<<"schema">>});
		{use,<<"A">>} ->
			cmd(P,<<>>,{use,<<"actordb">>});
		{use,Name} ->
			case string:to_lower(binary_to_list(Name)) of
				"actordb" ->
					print_help(change_prompt(P#dp{curdb = actordb}));
				"config" ->
					print_help(change_prompt(P#dp{curdb = config}));
				"schema" ->
					print_help(change_prompt(P#dp{curdb = schema}));
				_ ->
					print(P,"Invalid db")
			end;
		{show,<<>>} ->
			cmd(P,Bin,{show,<<"schema">>});
		{show,Show} ->
			case string:to_lower(binary_to_list(Show)) of
				"queries"++_ ->
					send_query(change_prompt(P#dp{buffer = []}), <<"show queries;">>);
				"schema"++_ ->
					cmd(P,Bin,show);
				"status"++_ ->
					send_query(change_prompt(P#dp{buffer = []}), <<"show status;">>);
				"shards"++_ ->
					send_query(change_prompt(P#dp{buffer = []}), <<"show shards;">>)
			end;
		show when P#dp.curdb == config ->
			send_cfg_query(change_prompt(P#dp{buffer = []}),<<"show schema;">>);
		show ->
			send_schema_query(change_prompt(P#dp{buffer = []}),<<"show schema;">>);
		print ->
			print(P,io_lib:fwrite("~s",[butil:iolist_join(lists:reverse(P#dp.buffer),"\n")]));
		rollback ->
			change_prompt(P#dp{buffer = []});
		commit when P#dp.buffer == [] ->
			print(P,"Nothing to commit.");
		commit when P#dp.curdb == schema ->
			send_schema_query(change_prompt(P#dp{buffer = []}),lists:reverse(P#dp.buffer));
		commit when P#dp.curdb == config ->
			send_cfg_query(change_prompt(P#dp{buffer = []}),lists:reverse(P#dp.buffer));
		commit ->
			send_query(change_prompt(P#dp{buffer = []}),lists:reverse(P#dp.buffer));
		{commit,_,_} ->
			cmd(P,<<>>,commit);
		_ when P#dp.curdb == actordb ->
			append(P,Bin);
		% Let actordb deal with it, unless it is config db
		{fail,_} when P#dp.curdb /= config andalso (P#dp.wait orelse P#dp.curdb == schema)  ->
			append(P,Bin);
		{fail,_} ->
			print(P,"Unrecognized command.");
		% R when element(1,R) == show ->
		% 	cmd_show(P,R);
		{actor,Type,SubType} ->
			cmd_actor(P,{actor,Type,SubType},Bin);
		% create_table ->
		% 	change_prompt(cmd_create(P,Bin));
		R when element(1,R) == select ->
			cmd_select(P,R,Bin);
		R when element(1,R) == insert ->
			cmd_insert(P,R,Bin);
		R when element(1,R) == update ->
			cmd_update(P,R,Bin);
		R when element(1,R) == delete ->
			cmd_delete(P,R,Bin);
		R when element(1,R) == management ->
			cmd_usermng(P,R,Bin);
		_ when is_tuple(Tuple), is_tuple(element(3,Tuple)), is_binary(element(2,Tuple)) ->
			RemBin = element(2,Tuple),
			ThisSize = byte_size(Bin) - byte_size(RemBin),
			NextSize = byte_size(RemBin),
			<<This:ThisSize/binary,Next:NextSize/binary>> = Bin,
			cmd(cmd(P,This,element(1,Tuple)), Next);
		_ ->
			print(P,"Unrecognized command. ~p",[P#dp.curdb])
	end.

% cmd_show(#dp{curdb = actordb} = P,_R) ->
% 	P;
% cmd_show(P,_R) ->
% 	P.

append(P,<<>>) ->
	P;
append(P,Bin) ->
	case binary:last(Bin) of
		$; ->
			change_prompt(P#dp{buffer = [Bin|P#dp.buffer]});
		C when C == $\s; C == $\t; C == $\r; C == $\n ->
			S = byte_size(Bin)-1,
			<<Bin1:S/binary,_>> = Bin,
			append(P,Bin1);
		_ ->
			change_prompt(P#dp{buffer = [[Bin,";"]|P#dp.buffer]})
	end.

cmd_actor(#dp{curdb = config} = P,_,_) ->
	print(P,"actor statements do not belong in config db");
cmd_actor(P,_,Bin) ->
	append(P,Bin).
% cmd_actor(#dp{curdb = actordb} = P,{actor,_Type},Bin) ->
% 	P#dp{buffer = [Bin|P#dp.buffer]}.

cmd_insert(#dp{curdb = actordb, wait = false} = P,_,Bin) ->
	send_query(P,Bin);
cmd_insert(#dp{curdb = config, wait = false} = P, _, Bin) ->
	send_cfg_query(P,Bin);
cmd_insert(P,_,Bin) ->
	append(P,Bin).

cmd_usermng(#dp{curdb = config} = P,_,Bin) ->
	append(P,Bin);
cmd_usermng(P,_,_) ->
	print(P,"Not in config database.").

cmd_update(#dp{curdb = actordb, wait = false} = P,_,Bin) ->
	send_query(P,Bin);
cmd_update(#dp{curdb = config, wait = false} = P,_,Bin) ->
	send_cfg_query(P,Bin);
cmd_update(P,_,Bin) ->
	append(P,Bin).

cmd_select(#dp{curdb = actordb, wait = false} = P,_,Bin) ->
	send_query(P,Bin);
cmd_select(#dp{curdb = actordb} = P,_,Bin) ->
	append(P,Bin);
cmd_select(#dp{curdb = schema} = P,_,_) ->
	print(P,"select statements do not belong in schema.");
cmd_select(P,_,Bin) ->
	send_cfg_query(P,Bin).

% cmd_create(#dp{curdb = actordb, wait = false} = P,Bin) ->
% 	send_query(P,Bin);
% cmd_create(#dp{curdb = actordb} = P,Bin) ->
% 	append(P,Bin);
% cmd_create(P,_) ->
% 	print(P,"Can not run create on current db.").

cmd_delete(#dp{curdb = actordb, wait = false} = P,_R,Bin) ->
	send_query(P,Bin);
cmd_delete(#dp{curdb = actordb} = P,_R,Bin) ->
	append(P,Bin);
cmd_delete(P,_,Bin) ->
	append(P,Bin).

send_cfg_query(P,Bin) ->
	Cfg = actordb_client:config([{query_timeout, 5000},{blob_tuple,true}]),
	case catch actordb_client:exec_config(Cfg,butil:tobin(Bin)) of
		{ok,{false,Map}} ->
			map_print(P,Map);
		{ok,{changes,_Rowid,_NChanged}} ->
			print(P,"Config updated.",[]);
		{error,{login_failed,_}} when P#dp.env == wx ->
			wxproc ! dologin,
			P;
		{'EXIT',{noproc,_}} ->
			print(P,"No session."),
			halt(1);
		Err ->
			print(P,"Error: ~p",[Err])
	end.

send_schema_query(P,Bin) ->
	Cfg = actordb_client:config([{query_timeout, 5000},{blob_tuple,true}]),
	case catch actordb_client:exec_schema(Cfg, butil:tobin(Bin)) of
		{ok,{false,Map}} ->
			map_print(P,Map);
		{ok,{changes,_Rowid,_NChanged}} ->
			print(P,"Schema updated.",[]);
		{error,{login_failed,_}} when P#dp.env == wx ->
			wxproc ! dologin,
			P;
		{'EXIT',{noproc,_}} ->
			print(P,"No session."),
			halt(1);
		Err ->
			print(P,"Error: ~p",[Err])
	end.

send_query(P,Bin) when P#dp.buffer /= [] ->
	send_query(P#dp{buffer = []},lists:reverse(append(P,Bin)));
send_query(P,Bin) ->
	Cfg = actordb_client:config([{query_timeout, 5000},{blob_tuple,true}]),
	case catch actordb_client:exec(Cfg, butil:tobin(Bin)) of
		{ok,{false,Map}} ->
			map_print(P,Map);
		{ok,{changes,Rowid,NChanged}} ->
			print(P,"Rowid: ~p, Rows changed: ~p",[Rowid,NChanged]);
		{error,{login_failed,_}} when P#dp.env == wx ->
			wxproc ! dologin,
			P;
		{'EXIT',{noproc,_}} ->
			print(P,"No session."),
			halt(1);
		Err ->
			print(P,"Error: ~p",[Err])
	end.

print(P,F) ->
	print(P,F,[]).
print(#dp{env = test} = P,F,A) ->
	io:format(F++"~n",A),
	P;
print(#dp{env = wx} = P,F,A) ->
	io:format(F,A),
	io:format("\n"),
	wxproc ! {print,io_lib:fwrite(F,A)},
	P;
print(P,F,A) ->
	port_command(P#dp.resp, [unicode:characters_to_binary(io_lib:format(F,A)),<<"\r\n">>]),
	P.

change_prompt(P) when P#dp.execute /= undefined ->
	P;
change_prompt(#dp{env = wx} = P) ->
	case P#dp.curdb of
		actordb ->
			wxproc ! {prompt,"actordb"++uncommited(P)++"> "};
		config ->
			wxproc ! {prompt,"actordb:config"++uncommited(P)++"> "};
		schema ->
			wxproc ! {prompt,"actordb:schema"++uncommited(P)++"> "}
	end,
	P;
change_prompt(P) ->
	case P#dp.curdb of
		actordb ->
			print(P,"~~~~actordb"++uncommited(P)++"> ");
		config ->
			print(P,"~~~~actordb:config"++uncommited(P)++"> ");
		schema ->
			print(P,"~~~~actordb:schema"++uncommited(P)++"> ")
	end.

uncommited(#dp{buffer = []}) ->
	"";
uncommited(P) ->
	" ("++integer_to_list(length(P#dp.buffer))++")".

print_help(P) when P#dp.execute /= undefined ->
	P;
print_help(#dp{env = test} = P) ->
	P;
print_help(#dp{curdb = actordb} = P) ->
	P;
% print_help(#dp{curdb = users} = P) ->
% 	print(P,"MySQL commands https://dev.mysql.com/doc/refman/5.1/en/user-account-management.html");
print_help(#dp{curdb = config} = P) ->
	% Url = "https://dev.mysql.com/doc/refman/5.1/en/user-account-management.html\n",
	Usr = "CREATE USER 'myuser' IDENTIFIED BY 'mypass'\n",
	Usrg = "GRANT read,write ON * to 'myuser'\n",
	U = "For user account management:\n"++Usr++Usrg,
	E = "To create/modify servers, run inserts to these tables: \n"++
		"CREATE TABLE groups (name TEXT, type TEXT DEFAULT 'cluster');\n"++
		"CREATE TABLE nodes (name TEXT, group_name TEXT);\n",
	N = "(optional) To store varius configuration info you can use:\n"++
		"CREATE TABLE state (id TEXT, val);\n",
	print(P,delim()++U++delim()++E++delim()++N++delim());
print_help(#dp{curdb = schema} = P) ->
	S = "actor type1; CREATE TABLE tab (id INTEGER PRIMARY KEY, val TEXT);\n",
	R = "WARNING: Schema is not overwritten but appended.\n"++
		"         Any pre-existing type will have old and new\n"++
		"         statements as its schema.\n",
	print(P,delim()++"Create or modify schema for actor types. Example:\n"++S++delim()++R++delim()).

dopipe(#dp{stop = true}) ->
	ok;
dopipe(#dp{env = wx} = P) ->
	After = P#dp.timeout_after,
	receive
		{login,U,Pw} ->
			NP = P#dp{username = U, password = Pw},
			dologin(NP),
			dopipe(NP);
		{dofile,Pth} ->
			{ok,Bin} = file:read_file(Pth),
			dopipe(cmd_lines(P,binary:split(Bin,<<"\n">>,[global])));
		{exec,Str} ->
			case catch cmd(P,Str) of
				#dp{} = NP ->
					dopipe(NP);
				X ->
					print(P,io_lib:fwrite("~p",[X])),
					dopipe(P)
			end
	after After ->
		timeout
	end;
dopipe(P) ->
	After = P#dp.timeout_after,
	receive
		{_, {data, Data}} ->
			Line = string:tokens(binary_to_list(Data),"\n"),
			case Line of
				[Q] when (Q == "Q" orelse Q == "q") andalso P#dp.password /= prompt ->
					print(P,"Bye!"),
					ok;
				_ when P#dp.password == prompt ->
					Data;
				_ ->
					case catch cmd(P,Data) of
						#dp{} = NP ->
							dopipe(NP);
						X ->
							% port_command(P#dp.resp, [io_lib:fwrite("~p",[X]),<<"\n">>]),
							print(P,io_lib:fwrite("~p",[X])),
							dopipe(P)
					end
			end;
		X ->
			port_command(P#dp.resp, [io_lib:fwrite("~p",[X]),<<"\n">>]),
			io:format("Received ~p~n",[X])
	after After ->
		timeout
	end.

map_print(M) when is_list(M) ->
	map_print(#dp{env = test},M);
map_print(M) ->
	map_print([M]).
map_print(#dp{print = Pr} = P,[]) when Pr /= undefined ->
	P;
map_print(P,[]) ->
	print(P,"No results.");
map_print(#dp{print = min} = P,L) ->
	Str = [begin
		ML = lists:keysort(1,[{atom_to_list(K),V} || {K,V} <- maps:to_list(M)]),
		[butil:iolist_join([to_unicode(V) || {_,V} <- ML],"|")]
	end || M <- L],
	print(P,butil:iolist_join(Str,"\n")),
	P;
map_print(#dp{print = Csv} = P,L) when Csv == csv; Csv == csvh ->
	Str = [begin
		ML = lists:keysort(1,[{atom_to_list(K),V} || {K,V} <- maps:to_list(M)]),
		[butil:iolist_join([quote(V) || {_,V} <- ML],",")]
	end || M <- L],
	case Csv of
		csvh ->
			Keys = lists:sort([atom_to_list(K) || K <- maps:keys(hd(L))]),
			print(P,butil:iolist_join(Keys,","));
		_ ->
			ok
	end,
	print(P,butil:iolist_join(Str,"\n")),
	P;
map_print(P,M) ->
	Keys = maps:keys(hd(M)),
	map_print(P,Keys,M,[]).

quote(X) when is_list(X); is_binary(X) ->
	[$\",to_unicode(re:replace(X, "\"", "\"\"", [global, unicode,{return,list}])),$\"];
quote(X) ->
	to_unicode(X).

to_unicode(undefined) ->
	"null";
to_unicode({blob,B}) ->
	"0x"++binary_to_list(butil:dec2hex(B));
to_unicode(B) when is_binary(B) ->
	case unicode:characters_to_list(B) of
		R when is_list(R) ->
			R;
		_ ->
			"0x"++butil:tolist(butil:dec2hex(B))
	end;
to_unicode(B) when is_list(B) ->
	to_unicode(iolist_to_binary(B));
to_unicode(B) ->
	to_unicode(butil:tobin(B)).

map_print(P,[Key|T],Maps,L) ->
	Lenk = length(butil:tolist(Key)),
	Len = lists:max([Lenk|[length(to_unicode(maps:get(Key,M))) || M <- Maps]]),
	map_print(P,T,Maps,[{Key,Len}|L]);
map_print(P,[],Maps,L1) ->
	L = lists:reverse(L1),
	Width = lists:sum([Len || {_,Len} <- L]),
	Chars = length(L)+1 + Width,
	Delim = string:right("",Chars,$*),
	Delim1 = string:right("",Chars,$-),
	print(P,"~s",[Delim]),
	StrKeys = [io_lib:format("~ts",[string:left(to_unicode(K),Len+1,$\s)]) || {K,Len} <- L],
	print(P,"~ts|",[StrKeys]),
	print(P,"~s",[Delim1]),
	map_print1(P,Maps,L),
	print(P,"~s",[Delim1]).

map_print1(P,[M|T],Keys) ->
	print(P,[[io_lib:format("~ts",[string:left(to_unicode(maps:get(K,M)),Len+1,$\s)]) || {K,Len} <- Keys],"|"]),
	map_print1(P,T,Keys);
map_print1(_,[],_) ->
	[].
