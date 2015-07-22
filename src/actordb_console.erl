-module(actordb_console).
-export([main/1,cmd/1]).
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
-record(dp,{env = shell, curdb = actordb, req, resp, stop = false, buffer = []}).

main(["pipe", Req,Resp|Args]) ->
	ReqPipe = open_port(Req, [in,eof,binary]),
	RespPipe = open_port(Resp, [out,eof,binary]),
	P = #dp{req = ReqPipe, resp = RespPipe, env = shell},
	case Args of
		[] ->
			port_command(RespPipe, [?COMMANDS,<<"\r\n">>]);
		_ ->
			ok
	end,
	dopipe(parse_args(P,Args));
main(_) ->
	ok.

parse_args(P,_) ->
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
		rollback ->
			change_prompt(P#dp{buffer = []});
		commit ->
			change_prompt(send_query(P#dp{buffer = []},lists:reverse(P#dp.buffer)));
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

cmd_update(#dp{curdb = actordb} = P,_,Bin) ->
	send_query(P,Bin);
cmd_update(P,_,Bin) ->
	P#dp{buffer = [Bin|P#dp.buffer]}.

% cmd_select(#dp{curdb = actordb} = P,_,Bin) ->
% 	send_query(P,Bin);
cmd_select(P,_,Bin) ->
	% P.
	send_query(P,Bin).

cmd_create(#dp{curdb = actordb} = P,Bin) ->
	send_query(P,Bin);
cmd_create(P,_) ->
	print(P,"Can not run create on current db.").

cmd_delete(#dp{curdb = actordb} = P,_R,Bin) ->
	send_query(P,Bin);
cmd_delete(P,_,_) ->
	print(P,"Can not run delete on current db.").


send_query(P,_Bin) ->
	P.

print(P,F) ->
	print(P,F,[]).
print(#dp{env = test} = P,F,A) ->
	io:format(F,A),
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
	"To commit run: commit\nTo abort run: rollback\n".

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

