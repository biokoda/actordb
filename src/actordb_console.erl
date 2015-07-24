-module(actordb_console).
-export([main/1,cmd/1, map_print/1]).
-include_lib("actordb_core/include/actordb.hrl").
-include_lib("wx/include/wx.hrl").
-define(PROMPT,"actordb>").

% TODO:
% on connect, check if initialized (select on config):
% - if not, print help for init user needs to create group,nodes and root user
%   Provide a shortcut command to create single node init.
% - if yes, print standard commands

-define(COMMANDS,delim()++"Databases:\n"++
"use config   initialize/add nodes and user account management\n"++
"use schema   set schema\n"++
"use actordb  (default) run queries on database\n"++
delim()++
"Commands:\n"++
"q            exit\n"++
"h            print this header\n"++
"commit       execute transaction\n"++
"rollback     to abort transaction\n"++
"print        print transaction\n"++delim()).

delim() ->
	"*******************************************************************\n".


% curdb changed with use statements 
% actordb - default can run queries directly
% config - for adding groups and nodes
% schema - for changing schema
-record(dp,{env = shell, curdb = actordb, req, resp, stop = false, buffer = [], wait = false,
	addr = "127.0.0.1", port = 33306, username = "", password = "", filebin}).

main(Args) ->
	register(home,self()),
	case os:type() of
		{win32,_} ->
			spawn(fun() -> (catch wxrun()),halt(1) end),
			P = parse_args(#dp{env = wx}, Args);
		% {unix,darwin} ->
		% 	spawn(fun() -> (catch wxrun()),halt(1) end),
		% 	P = parse_args(#dp{env = wx}, Args);
		_ ->
			ReqPipe = open_port("/tmp/actordb.req", [in,eof,binary]),
			RespPipe = open_port("/tmp/actordb.resp", [out,eof,binary]),
			P = parse_args(#dp{req = ReqPipe, resp = RespPipe, env = shell},Args)
	end,
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
	case P#dp.filebin of
		undefined ->
			% port_command(RespPipe, [?COMMANDS,<<"\r\n">>]),
			print(P,?COMMANDS),
			dopipe(P);
		_ ->
			cmd_lines(P,binary:split(P#dp.filebin,<<"\n">>,[global])),
			halt(1)
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
	"  -h   print this help and exit\n"++
	"  -u   username\n"++
	"  -p   password\n"++
	"  -f   <file> execute statements from file and exit\n"++
	"  -w   wait for commit to send query to actordb\n",
	print(P,"Call with: actordb_console -u username -p password IP[:ThriftPort]\n"++L),
	halt(1);
parse_args(P,["-f",File|T]) ->
	{ok,F} = file:read_file(File),
	parse_args(P#dp{filebin = F},T);
parse_args(P,["-u",Username|T]) ->
	parse_args(P#dp{username = Username},T);
parse_args(P,["-p",Password|T]) ->
	parse_args(P#dp{password = Password},T);
parse_args(P,["-w"|T]) ->
	parse_args(P#dp{wait = true},T);
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
cmd(P,<<"h">>) ->
	print(P,?COMMANDS);
cmd(_P,<<"q">>) ->
	halt(1);
cmd(P,Bin) when is_binary(Bin) ->
	cmd(P,Bin,actordb_sql:parse(Bin)).
cmd(P,Bin,Tuple) ->
	case Tuple of
		% Let actordb deal with it, unless it is config db
		{fail,_} when P#dp.curdb /= config andalso (P#dp.wait orelse P#dp.curdb == schema)  ->
			append(P,Bin);
		{fail,_} ->
			print(P,"Unrecognized command.");
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
		#show{} = R ->
			cmd_show(P,R);
		{actor,Type,SubType} ->
			cmd_actor(P,{actor,Type,SubType},Bin);
		print ->
			print(P,io_lib:fwrite("~s",[butil:iolist_join(lists:reverse(P#dp.buffer),"\n")]));
		rollback ->
			change_prompt(P#dp{buffer = []});
		commit when P#dp.curdb == schema ->
			send_schema_query(change_prompt(P#dp{buffer = []}),lists:reverse(P#dp.buffer));
		commit when P#dp.curdb /= actordb ->
			send_cfg_query(change_prompt(P#dp{buffer = []}),lists:reverse(P#dp.buffer));
		commit ->
			send_query(change_prompt(P#dp{buffer = []}),lists:reverse(P#dp.buffer));
		{commit,B} ->
			cmd(cmd(P,<<>>,commit),B);
		% create_table ->
		% 	change_prompt(cmd_create(P,Bin));
		#select{} = R ->
			cmd_select(P,R,Bin);
		#insert{} = R ->
			cmd_insert(P,R,Bin);
		#update{} = R ->
			cmd_update(P,R,Bin);
		#delete{} = R ->
			cmd_delete(P,R,Bin);
		#management{} = R ->
			cmd_usermng(P,R,Bin);
		_ when is_tuple(Tuple), is_tuple(element(3,Tuple)), is_binary(element(2,Tuple)) ->
			RemBin = element(2,Tuple),
			ThisSize = byte_size(Bin) - byte_size(RemBin),
			NextSize = byte_size(RemBin),
			<<This:ThisSize/binary,Next:NextSize/binary>> = Bin,
			cmd(cmd(P,This,element(1,Tuple)), Next);
		_ ->
			print(P,"Unrecognized command11. ~p",[P#dp.curdb])
	end.

cmd_show(#dp{curdb = actordb} = P,_R) ->
	P;
cmd_show(P,_R) ->
	P.

append(P,Bin) ->
	case binary:last(Bin) of
		$; ->
			change_prompt(P#dp{buffer = [Bin|P#dp.buffer]});
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
cmd_insert(P,_,Bin) ->
	append(P,Bin).

cmd_usermng(#dp{curdb = config} = P,_,Bin) ->
	append(P,Bin);
cmd_usermng(P,_,_) ->
	print(P,"Not in config database.").

cmd_update(#dp{curdb = actordb, wait = false} = P,_,Bin) ->
	send_query(P,Bin);
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
	case actordb_client:exec_config(butil:tobin(Bin)) of
		{ok,{false,Map}} ->
			map_print(P,Map);
		{ok,{changes,_Rowid,_NChanged}} ->
			print(P,"Config updated.",[]);
		Err ->
			print(P,"Error: ~p",[Err])
	end.

send_schema_query(P,Bin) ->
	case actordb_client:exec_schema(butil:tobin(Bin)) of
		{ok,{false,Map}} ->
			map_print(P,Map);
		{ok,{changes,_Rowid,_NChanged}} ->
			print(P,"Schema updated.",[]);
		Err ->
			print(P,"Error: ~p",[Err])
	end.

send_query(P,Bin) when P#dp.buffer /= [] ->
	send_query(P#dp{buffer = []},lists:reverse(append(P,Bin)));
send_query(P,Bin) ->
	case actordb_client:exec(butil:tobin(Bin)) of
		{ok,{false,Map}} ->
			map_print(P,Map);
		{ok,{changes,Rowid,NChanged}} ->
			print(P,"Rowid: ~p, Rows changed: ~p",[Rowid,NChanged]);
		Err ->
			print(P,"Error: ~p",[Err])
	end.

print(P,F) ->
	print(P,F,[]).
print(#dp{env = test} = P,F,A) ->
	io:format(F++"~n",A),
	P;
print(#dp{env = wx} = P,F,A) ->
	wxproc ! {print,io_lib:fwrite(F,A)},
	P;
print(P,F,A) ->
	port_command(P#dp.resp, [io_lib:format(F,A),<<"\r\n">>]),
	P.

change_prompt(#dp{env = wx} = P) ->
	case P#dp.curdb of
		actordb ->
			wxproc ! {prompt,"actordb"++uncommited(P)++">"};
		config ->
			wxproc ! {prompt,"actordb:config"++uncommited(P)++">"};
		schema ->
			wxproc ! {prompt,"actordb:schema"++uncommited(P)++">"}
	end,
	P;
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
	Url = "https://dev.mysql.com/doc/refman/5.1/en/user-account-management.html\n",
	U = "For user account management use mysql syntax.\n"++Url,
	E = "To create/modify servers, run inserts to these tables: \n",
	G = "CREATE TABLE groups (name TEXT, type TEXT DEFAULT 'cluster');\n",
	N = "CREATE TABLE nodes (name TEXT, group_name TEXT);\n",
	print(P,delim()++U++delim()++E++G++N++delim());
print_help(#dp{curdb = schema} = P) ->
	S = "actor type1; CREATE TABLE tab (id INTEGER PRIMARY KEY, val TEXT);\n",
	R = "WARNING: Schema is not overwritten but appended.\n"++
		"         Any pre-existing type will have old and new\n"++
		"         statements as its schema.\n",
	print(P,delim()++"Create or modify schema for actor types. Example:\n"++S++delim()++R++delim()).

dopipe(#dp{stop = true}) ->
	ok;
dopipe(#dp{env = wx} = P) ->
	receive
		{exec,Str} ->
			print(P,Str),
			case catch cmd(P,Str) of
				#dp{} = NP ->
					dopipe(NP);
				X ->
					print(P,io_lib:fwrite("~p",[X])),
					dopipe(P)
			end
	end;
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
							% port_command(P#dp.resp, [io_lib:fwrite("~p",[X]),<<"\n">>]),
							print(P,io_lib:fwrite("~p",[X])),
							dopipe(P)
					end
			end;
		X ->
			port_command(P#dp.resp, [io_lib:fwrite("~p",[X]),<<"\n">>]),
			io:format("Received ~p~n",[X])
	end.

wxrun() ->
	register(wxproc,self()),
	Wx = wx:new(),
	Dlg = wxDialog:new(Wx,-1,"ActorDB Shell",[{size,{640,480}},{style,?wxRESIZE_BORDER bor ?wxDEFAULT_DIALOG_STYLE}]),
	Sizer = wxBoxSizer:new(?wxVERTICAL),
	TextDisplay = wxTextCtrl:new(Dlg,4,[{style, ?wxTE_MULTILINE bor ?wxTE_READONLY}]),
	TextInput = wxTextCtrl:new(Dlg,5,[{style, ?wxDEFAULT bor ?wxHSCROLL bor ?wxTE_PROCESS_ENTER}]),
	SzFlags = [{proportion, 0}, {border, 4}, {flag, ?wxALL}],
	wxSizer:add(Sizer,TextDisplay,[{flag, ?wxEXPAND},{proportion, 1}|SzFlags]),
	wxSizer:add(Sizer,TextInput,[{proportion, 0},{border, 4}, {flag, ?wxEXPAND}]),
	wxTextCtrl:setEditable(TextInput,true),
	wxDialog:setSizer(Dlg,Sizer),
	wxDialog:show(Dlg),
	wxWindow:setFocus(TextInput),
	wxTextCtrl:writeText(TextInput,?PROMPT),
	wxEvtHandler:connect(Dlg,close_window),
	wxEvtHandler:connect(TextInput,command_text_enter),
	% wxEvtHandler:connect(TextInput,key_down,[{skip,true}]),
	wxEvtHandler:connect(TextInput,key_down,[{callback,fun input/2},{userData,{TextInput,?PROMPT}}]),
	wxloop(TextDisplay,TextInput,?PROMPT).

wxloop(Disp,Input,Prompt) ->
	receive
		{prompt,Str} ->
			wxTextCtrl:setValue(Input,Str),
			wxTextCtrl:setInsertionPoint(Input,length(Str)),
			wxEvtHandler:disconnect(Input,key_down),
			wxEvtHandler:connect(Input,key_down,[{callback,fun input/2},{userData,{Input,Str}}]),
			wxloop(Disp,Input,Str);
		{print,Str} ->
			wxTextCtrl:writeText(Disp,Str),
			wxTextCtrl:writeText(Disp,"\n"),
			wxloop(Disp,Input,Prompt);
		down ->
			wxloop(Disp,Input,Prompt);
		up ->
			wxloop(Disp,Input,Prompt);
		Wx when Wx#wx.obj == Input ->
			wxTextCtrl:setValue(Input,Prompt),
			wxTextCtrl:setInsertionPoint(Input,length(Prompt)),
			Cmd = Wx#wx.event,
			Str = Cmd#wxCommand.cmdString,
			Print = lists:sublist(Str,length(Prompt)+1,length(Str)),
			home ! {exec, unicode:characters_to_binary(Print)},
			wxloop(Disp,Input,Prompt);
		Wx ->
			Cmd = Wx#wx.event,
			case Cmd of
				#wxClose{} ->
					halt(1);
				_ ->
					ok
			end
	end.

input(Wx, Obj)  ->
	Cmd = Wx#wx.event,
	case Cmd of
		#wxKey{keyCode = ?WXK_UP} ->
			wxEvent:skip(Obj);
		#wxKey{keyCode = ?WXK_DOWN} ->
			wxEvent:skip(Obj);
		#wxKey{keyCode = ?WXK_BACK} ->
			{Input,Prompt} = Wx#wx.userData,
			Len = length(wxTextCtrl:getValue(Input)),
			case Len > length(Prompt) of
				true ->
					wxEvent:skip(Obj);
				false ->
					ok
			end;
		_ ->
			wxEvent:skip(Obj)
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

