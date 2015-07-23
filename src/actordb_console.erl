-module(actordb_console).
-export([main/1,cmd/1, map_print/1]).
% -compile(export_all).
-include_lib("actordb_core/include/actordb.hrl").

% TODO:
% on connect, check if initialized (select on config):
% - if not, print help for init user needs to create group,nodes and root user
%   Provide a shortcut command to create single node init.
% - if yes, print standard commands

-define(COMMANDS,"Databases:\n"++
"use config - initialize/add nodes and user account management\n"++
"use schema - set schema\n"++
"use actordb - (default) run queries on database\n").

% curdb changed with use statements 
% actordb - default can run queries directly
% config - for adding groups and nodes
% schema - for changing schema
-record(dp,{env = shell, curdb = actordb, req, resp, stop = false, buffer = [],
	addr = "127.0.0.1", port = 33306, username = "", password = ""}).

main(["pipe", Req,Resp|Args]) ->
	ReqPipe = open_port(Req, [in,eof,binary]),
	RespPipe = open_port(Resp, [out,eof,binary]),
	P = parse_args(#dp{req = ReqPipe, resp = RespPipe, env = shell},Args),
	PoolInfo = [{size, 1}, {max_overflow, 5}],
	WorkerParams = [{hostname, P#dp.addr},
		{username, P#dp.username},
		{password, P#dp.password},
		{port,P#dp.port}
	],
	case actordb_client:start(PoolInfo,WorkerParams) of
		ok ->
			% print(P,"Connected to DB\n"),
			ok;
		Err ->
			print(P,"Connect/login error: ~p~n",[Err]),
			halt(1)
	end,
	port_command(RespPipe, [?COMMANDS,<<"\r\n">>]),
	dopipe(P);
main(_) ->
	ok.

parse_args(P,["-h"|_]) ->
	print(P,"Call with: actordb_console -u username -p password IP[:ThriftPort]\n"),
	halt(1);
parse_args(P,["-u",Username|T]) ->
	parse_args(P#dp{username = Username},T);
parse_args(P,["-p",Password|T]) ->
	parse_args(P#dp{password = Password},T);
parse_args(P,[Addr]) ->
	case string:tokens(Addr,":") of
		[Addr,Port] ->
			P#dp{addr = Addr, port = list_to_integer(Port)};
		[Addr] ->
			P#dp{addr = Addr}
	end;
parse_args(P,[]) ->
	P.

cmd(C) ->
	cmd(#dp{env = test},iolist_to_binary(C)).
cmd(P,<<";",Rem/binary>>) ->
	cmd(P,Rem);
cmd(P,<<>>) ->
	P;
cmd(P,Bin) when is_binary(Bin) ->
	cmd(P,Bin,actordb_sql:parse(Bin)).
cmd(P,Bin,Tuple) ->
	case Tuple of
		{fail,_} ->
			print(P,"Unrecognized command.");
		{use,Name} ->
			case string:to_lower(binary_to_list(Name)) of
				"actordb" ->
					print_help(change_prompt(P#dp{curdb = actordb}));
				"config" ->
					print_help(change_prompt(P#dp{curdb = config}));
				"schema" ->
					print_help(change_prompt(P#dp{curdb = schema}))
			end;
		#show{} = R ->
			cmd_show(P,R);
		print ->
			print(P,io_lib:fwrite("~s",[butil:iolist_join(lists:reverse(P#dp.buffer),"\n")]));
		rollback ->
			change_prompt(P#dp{buffer = []});
		commit ->
			send_cfg_query(change_prompt(P#dp{buffer = []}),lists:reverse(P#dp.buffer));
		create_table ->
			change_prompt(cmd_create(P,Bin));
		#select{} = R ->
			cmd_select(P,R,Bin);
		#insert{} = R ->
			change_prompt(cmd_insert(P,R,Bin));
		#update{} = R ->
			change_prompt(cmd_update(P,R,Bin));
		#delete{} = R ->
			change_prompt(cmd_delete(P,R,Bin));
		#management{} = R ->
			change_prompt(cmd_usermng(P,R,Bin));
		_ when is_tuple(Tuple), is_tuple(element(1,Tuple)), is_binary(element(2,Tuple)) ->
			cmd(cmd(P,Bin,element(1,Tuple)), element(2,Tuple));
		_ ->
			print(P,"Unrecognized command.")
	end.

cmd_show(#dp{curdb = actordb} = P,_R) ->
	P;
cmd_show(P,_R) ->
	P.

cmd_insert(#dp{curdb = actordb} = P,_,Bin) ->
	send_query(P,Bin);
% cmd_insert(#dp{curdb = config} = P,_R,Bin) ->
% 	% T = (R#insert.table)#table.name,
% 	% V = []
% 	P#dp{buffer = [Bin|P#dp.buffer]};
cmd_insert(P,_,Bin) ->
	P#dp{buffer = [Bin|P#dp.buffer]}.

cmd_usermng(#dp{curdb = config} = P,_,Bin) ->
	P#dp{buffer = [Bin|P#dp.buffer]};
cmd_usermng(P,_,_) ->
	print(P,"Not in config database.").

cmd_update(#dp{curdb = actordb} = P,_,Bin) ->
	send_query(P,Bin);
cmd_update(P,_,Bin) ->
	P#dp{buffer = [Bin|P#dp.buffer]}.

cmd_select(#dp{curdb = actordb} = P,_,Bin) ->
	send_query(P,Bin);
cmd_select(P,_,Bin) ->
	send_cfg_query(P,Bin).

cmd_create(#dp{curdb = actordb} = P,Bin) ->
	send_query(P,Bin);
cmd_create(P,_) ->
	print(P,"Can not run create on current db.").

cmd_delete(#dp{curdb = actordb} = P,_R,Bin) ->
	send_query(P,Bin);
cmd_delete(P,_,_) ->
	print(P,"Can not run delete on current db.").

send_cfg_query(P,Bin) ->
	case actordb_client:exec_config(butil:tobin(Bin)) of
		{ok,{false,Map}} ->
			map_print(P,Map);
		{ok,{changes,Rowid,NChanged}} ->
			print(P,"Rowid: ~p, Rows changed: ~p",[Rowid,NChanged]);
		Err ->
			print(P,"Error: ~p~n",[Err])
	end.

send_query(P,Bin) ->
	P.

print(P,F) ->
	print(P,F,[]).
print(#dp{env = test} = P,F,A) ->
	io:format(F++"~n",A),
	P;
print(P,F,A) ->
	port_command(P#dp.resp, [io_lib:format(F,A),<<"\r\n">>]),
	P.

change_prompt(P) ->
	case P#dp.curdb of
		actordb ->
			print(P,"~~~~actordb"++uncommited(P)++">");
		config ->
			print(P,"~~~~actordb:config"++uncommited(P)++">");
		schema ->
			print(P,"~~~~actordb:schema"++uncommited(P)++">")
	end.

uncommited(#dp{buffer = []}) ->
	"";
uncommited(P) ->
	" ("++integer_to_list(length(P#dp.buffer))++")".

print_help(#dp{env = test} = P) ->
	P;
print_help(#dp{curdb = actordb} = P) ->
	P;
% print_help(#dp{curdb = users} = P) ->
% 	print(P,"MySQL commands https://dev.mysql.com/doc/refman/5.1/en/user-account-management.html");
print_help(#dp{curdb = config} = P) ->
	Delim = "*******************************************************************\n",
	Url = "https://dev.mysql.com/doc/refman/5.1/en/user-account-management.html\n",
	U = "For user account management use mysql syntax.\n"++Url,
	E = "To create/modify servers, run inserts to these tables: \n",
	G = "CREATE TABLE groups (name TEXT, type TEXT DEFAULT 'cluster');\n",
	N = "CREATE TABLE nodes (name TEXT, group_name TEXT);\n",
	print(P,Delim++U++Delim++E++G++N++Delim++c()++Delim);
print_help(#dp{curdb = schema} = P) ->
	S = "actor type1; CREATE TABLE tab (id INTEGER PRIMARY KEY, val TEXT);\n",
	print(P,"Create or modify schema for actor types. Example:\n"++S++c()).

c() ->
	"To commit run: commit\nTo abort run: rollback\nTo view transaction: print\n".

dopipe(#dp{stop = true}) ->
	ok;
dopipe(P) ->
	receive
		{_, {data, Data}} ->
			Line = string:tokens(binary_to_list(Data),"\n"),
			case Line of
				["q"] ->
					ok;
				_ ->
					case catch cmd(P,Data) of
						#dp{} = NP ->
							dopipe(NP);
						X ->
							port_command(P#dp.resp, [io_lib:fwrite("~p",[X]),<<"\n">>]),
							dopipe(P)
					end
			end;
		X ->
			port_command(P#dp.resp, [io_lib:fwrite("~p",[X]),<<"\n">>]),
			io:format("Received ~p~n",[X])
	end.


map_print(M) when is_list(M) ->
	map_print(#dp{env = test},M);
map_print(M) ->
	map_print([M]).
map_print(P,M) ->
	Keys = maps:keys(hd(M)),
	map_print(P,Keys,M,[]).

map_print(P,[Key|T],Maps,L) ->
	Lenk = length(butil:tolist(Key)),
	Len = lists:max([Lenk|[length(butil:tolist(maps:get(Key,M))) || M <- Maps]]),
	map_print(P,T,Maps,[{Key,Len}|L]);
map_print(P,[],Maps,L1) ->
	L = lists:reverse(L1),
	Width = lists:sum([Len || {_,Len} <- L]),
	Chars = length(L)+1 + Width,
	Delim = string:right("",Chars,$*),
	Delim1 = string:right("",Chars,$-),
	print(P,"~s",[Delim]),
	StrKeys = [io_lib:format("~s",[string:left(butil:tolist(K),Len+1,$\s)]) || {K,Len} <- L],
	print(P,"~s|",[StrKeys]),
	print(P,"~s",[Delim1]),
	StrVals = map_print1(Maps,L),
	print(P,"~s",[StrVals]),
	print(P,"~s",[Delim1]).

map_print1([M|T],Keys) ->
	case T of
		[] ->
			End = "";
		_ ->
			End = "\n"
	end,
	[[io_lib:format("~s",[string:left(butil:tolist(maps:get(K,M)),Len+1,$\s)]) || {K,Len} <- Keys],"|",End|map_print1(T,Keys)];
map_print1([],_) ->
	[].

