% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at http://mozilla.org/MPL/2.0/.
-module(actordb_console).
-export([main/1,cmd/1, map_print/1]).
% -include_lib("actordb_core/include/actordb.hrl").
-include_lib("wx/include/wx.hrl").
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
"show (s)     show schema\n"++delim()).

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
			timer:sleep(50),
			P = parse_args(#dp{env = wx}, Args);
		% {unix,darwin} ->
		% 	spawn(fun() -> (catch wxrun()),halt(1) end),
		% 	P = parse_args(#dp{env = wx}, Args);
		_ ->
			ReqPipe = open_port("/tmp/actordb.req", [in,eof,binary]),
			RespPipe = open_port("/tmp/actordb.resp", [out,eof,binary]),
			P = setpw(parse_args(#dp{req = ReqPipe, resp = RespPipe, env = shell},Args))
	end,
	dologin(P),
	% print(P,"SALT=~p",[actordb_client:salt()]),
	case P#dp.filebin of
		undefined ->
			% port_command(RespPipe, [?COMMANDS,<<"\r\n">>]),
			print(P,?COMMANDS),
			dopipe(P);
		_ ->
			cmd_lines(P,binary:split(P#dp.filebin,<<"\n">>,[global])),
			halt(1)
	end.

setpw(#dp{password = prompt} = P) ->
	print(P,"~~~~getpass"),
	Pw = dopipe(P),
	P#dp{password = Pw};
setpw(P) ->
	P.

dologin(P) ->
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
	"  -h            Print this help and exit.\n"++
	"  -u <username> Set username. You will be prompted for password. Not required if ActorDB is uninitalized.\n"++
	"  -f <file>     Execute statements from file and exit.\n",
	% "  -w   wait for commit to send query to actordb\n",
	print(P,"Call with: actordb_console -u username IP[:ThriftPort]\n"++L),
	halt(1);
parse_args(P,["-f",File|T]) ->
	{ok,F} = file:read_file(File),
	parse_args(P#dp{filebin = F},T);
parse_args(P,["-u",Username|T]) ->
	parse_args(P#dp{username = Username, password = prompt},T);
% parse_args(P,["-p",Password|T]) ->
% 	parse_args(P#dp{password = Password},T);
% parse_args(P,["-p"|T]) ->
% 	parse_args(P#dp{password = prompt},T);
parse_args(P,["-w"|T]) ->
	parse_args(P#dp{wait = true},T);
parse_args(P,[Addr]) ->
	case string:tokens(Addr,":") of
		[Address,Port] ->
			P#dp{addr = Address, port = list_to_integer(Port)};
		[Address] ->
			P#dp{addr = Address}
	end;
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
			print(P,"SCHEMA"),
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
	case catch actordb_client:exec_config(butil:tobin(Bin)) of
		{ok,{false,Map}} ->
			map_print(P,Map);
		{ok,{changes,_Rowid,_NChanged}} ->
			print(P,"Config updated.",[]);
		{'EXIT',{noproc,_}} when P#dp.env == wx ->
			wxproc ! dologin,
			P;
		{'EXIT',{noproc,_}} ->
			print(P,"No session."),
			halt(1);
		Err ->
			print(P,"Error: ~p",[Err])
	end.

send_schema_query(P,Bin) ->
	case catch actordb_client:exec_schema(butil:tobin(Bin)) of
		{ok,{false,Map}} ->
			map_print(P,Map);
		{ok,{changes,_Rowid,_NChanged}} ->
			print(P,"Schema updated.",[]);
		{'EXIT',{noproc,_}} when P#dp.env == wx ->
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
	case catch actordb_client:exec(butil:tobin(Bin)) of
		{ok,{false,Map}} ->
			map_print(P,Map);
		{ok,{changes,Rowid,NChanged}} ->
			print(P,"Rowid: ~p, Rows changed: ~p",[Rowid,NChanged]);
		{'EXIT',{noproc,_}} when P#dp.env == wx ->
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
	end;
dopipe(P) ->
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
	end.

-record(wc,{wx, dlg, input, disp, prompt = ?PROMPT,history_pos = 0,current = "",history = []}).
wxrun() ->
	register(wxproc,self()),
	Wx = wx:new(),
	Dlg = wxDialog:new(Wx,-1,"ActorDB Shell",[{size,{640,480}},{style,?wxRESIZE_BORDER bor ?wxDEFAULT_DIALOG_STYLE}]),
	Sizer = wxBoxSizer:new(?wxVERTICAL),
	TextDisplay = wxTextCtrl:new(Dlg,4,[{style, ?wxTE_MULTILINE bor ?wxTE_READONLY}]),
	TextInput = wxTextCtrl:new(Dlg,5,[{style, ?wxDEFAULT bor ?wxTE_PROCESS_ENTER}]),
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
	wxEvtHandler:connect(TextDisplay,key_down),
	% I guess its a broken erlang wx implementation. I don't see how I can read
	% text from clipboard
	% wxEvtHandler:connect(TextInput,command_text_paste,[{skip,false}]),
	wxEvtHandler:connect(TextInput,key_down,[{callback,fun input/2},{userData,{TextInput,?PROMPT}}]),
	wxloop(#wc{wx = Wx, dlg = Dlg, input = TextInput, disp = TextDisplay}),
	wx:destroy(Wx).

-record(lg,{dlg, uinp, pinp, btn}).

wxloop(P) ->
	receive
		{prompt,Str} ->
			wxTextCtrl:setValue(P#wc.input,Str),
			wxTextCtrl:setInsertionPoint(P#wc.input,length(Str)),
			wxEvtHandler:disconnect(P#wc.input,key_down),
			wxEvtHandler:connect(P#wc.input,key_down,[{callback,fun input/2},{userData,{P#wc.input,Str}}]),
			self() ! {print,""},
			wxloop(P#wc{prompt = Str});
		{print,Str} ->
			wxTextCtrl:writeText(P#wc.disp,Str),
			wxTextCtrl:writeText(P#wc.disp,"\n"),
			wxloop(P);
		up ->
			case P#wc.history_pos of
				0 ->
					Str = wxTextCtrl:getValue(P#wc.input),
					Cur = lists:sublist(Str,length(P#wc.prompt)+1,length(Str));
				_ ->
					Cur = P#wc.current
			end,
			case catch lists:nth(P#wc.history_pos+1,P#wc.history) of
				{'EXIT',_} ->
					wxloop(P);
				NewStr ->
					wxTextCtrl:setValue(P#wc.input,P#wc.prompt++NewStr),
					wxTextCtrl:setInsertionPoint(P#wc.input,length(NewStr)+length(P#wc.prompt)),
					wxloop(P#wc{current = Cur, history_pos = P#wc.history_pos+1})
			end;
		down ->
			case P#wc.history_pos > 0 of
				true ->
					case P#wc.history_pos > 1 of
						true ->
							Str = lists:nth(P#wc.history_pos-1,P#wc.history);
						false ->
							Str = P#wc.current
					end,
					wxTextCtrl:setValue(P#wc.input,P#wc.prompt++Str),
					wxTextCtrl:setInsertionPoint(P#wc.input,length(Str)+length(P#wc.prompt)),
					wxloop(P#wc{history_pos = P#wc.history_pos-1});
				false ->
					wxloop(P)
			end;
		stop ->
			ok;
		Wx when Wx#wx.obj == P#wc.disp ->
			wxWindow:setFocus(P#wc.input),
			wxloop(P);
		Wx when Wx#wx.obj == P#wc.input ->
			Cmd = Wx#wx.event,
			case Cmd of
				#wxMouse{} ->
					% self() ! {print,"MOUSE!"},
					wxloop(P);
				% #wxClipboardText{} ->
				% 	Clip = wxClipboard:get(),
				% 	self() ! {print,io_lib:fwrite("~p~n",[get(clip)])},
				% 	wxloop(Disp,Input,Prompt);
				_ ->
					wxTextCtrl:setValue(P#wc.input,P#wc.prompt),
					wxTextCtrl:setInsertionPoint(P#wc.input,length(P#wc.prompt)),
					Str = Cmd#wxCommand.cmdString,
					Print = lists:sublist(Str,length(P#wc.prompt)+1,length(Str)),
					case Print of
						"open" ->
							% spawn(fun() ->
							% {wildCard,"*.sql"}
							File = wxFileDialog:new(P#wc.dlg,[{defaultDir,"."}]),
							case wxDialog:showModal(File) == ?wxID_OK of
								true ->
									home ! {dofile,wxFileDialog:getPath(File)};
								_ ->
									ok
							end,
							wxFileDialog:destroy(File),
							wxWindow:setFocus(P#wc.input);
						"login" ->
							self() ! dologin,
							wxloop(P);
						_ ->
							self() ! {print,Str},
							home ! {exec, unicode:characters_to_binary(Print)}
					end,
					wxloop(P#wc{history = [Print|P#wc.history]})
			end;
		Wx when element(1,Wx) == wx ->
			Cmd = Wx#wx.event,
			case Cmd of
				#wxClose{} ->
					halt(1);
				_ ->
					wxloop(P)
			end;
		dologin ->
			Dlg = wxDialog:new(P#wc.dlg,7,"Login",[{style,?wxRESIZE_BORDER bor ?wxDEFAULT_DIALOG_STYLE}]),
			VSizer = wxBoxSizer:new(?wxVERTICAL),
			HSizer1 = wxBoxSizer:new(?wxHORIZONTAL),
			HSizer2 = wxBoxSizer:new(?wxHORIZONTAL),
			ULabel = wxTextCtrl:new(Dlg,-1,[{value,"Username:"},{style, ?wxDEFAULT bor ?wxTE_READONLY}]),
			PLabel = wxTextCtrl:new(Dlg,-1,[{value,"Password:"},{style, ?wxDEFAULT bor ?wxTE_READONLY}]),
			UInp = wxTextCtrl:new(Dlg,-1,[{style, ?wxDEFAULT bor ?wxTE_PROCESS_ENTER}]),
			PInp = wxTextCtrl:new(Dlg,-1,[{style, ?wxDEFAULT bor ?wxTE_PASSWORD bor ?wxTE_PROCESS_ENTER}]),
			Btn = wxButton:new(Dlg,-1,[{label,"Login"}]),
			SzFlags = [{proportion, 0}, {border, 4}, {flag, ?wxALL}],
			wxSizer:add(HSizer1,ULabel,[{flag, ?wxEXPAND},{proportion, 1}|SzFlags]),
			wxSizer:add(HSizer1,UInp,[{proportion, 0},{border, 4}, {flag, ?wxEXPAND}]),
			wxSizer:add(HSizer2,PLabel,[{flag, ?wxEXPAND},{proportion, 1}|SzFlags]),
			wxSizer:add(HSizer2,PInp,[{proportion, 0},{border, 4}, {flag, ?wxEXPAND}]),
			wxSizer:add(VSizer,HSizer1),
			wxSizer:add(VSizer,HSizer2),
			wxSizer:add(VSizer,Btn,[{proportion,0},{flag,?wxEXPAND}]),
			wxDialog:setSizerAndFit(Dlg,VSizer),
			UP = #lg{dlg = Dlg, uinp = UInp, pinp = PInp, btn = Btn},
			wxEvtHandler:connect(Btn,command_button_clicked,[{callback,fun btn/2},{userData,UP}]),
			wxEvtHandler:connect(UInp,command_text_enter,[{callback,fun uinp/2},{userData,UP}]),
			wxEvtHandler:connect(PInp,command_text_enter,[{callback,fun pinp/2},{userData,UP}]),
			wxEvtHandler:connect(UInp,key_down,[{callback,fun ukey/2},{userData,UP}]),
			wxEvtHandler:connect(PInp,key_down,[{callback,fun pkey/2},{userData,UP}]),
			wxWindow:setFocus(UInp),
			wxDialog:showModal(Dlg),
			wxDialog:destroy(Dlg),
			wxWindow:setFocus(P#wc.input),
			wxloop(P)
	end.

uinp(Wx,_Obj) ->
	P = Wx#wx.userData,
	wxWindow:setFocus(P#lg.pinp).
pinp(Wx,_Obj) ->
	P = Wx#wx.userData,
	U = wxTextCtrl:getValue(P#lg.uinp),
	Pw = wxTextCtrl:getValue(P#lg.pinp),
	home ! {login,U,Pw},
	wxWindow:close(P#lg.dlg).

ukey(Wx,Obj) ->
	Cmd = Wx#wx.event,
	P = Wx#wx.userData,
	case Cmd of
		#wxKey{keyCode = ?WXK_TAB} ->
			wxWindow:setFocus(P#lg.pinp);
		_ ->
			wxEvent:skip(Obj)
	end.
pkey(Wx,Obj) ->
	Cmd = Wx#wx.event,
	P = Wx#wx.userData,
	case Cmd of
		#wxKey{keyCode = ?WXK_TAB} ->
			wxWindow:setFocus(P#lg.btn);
		_ ->
			wxEvent:skip(Obj)
	end.
btn(Wx,Obj) ->
	pinp(Wx,Obj).


input(Wx, Obj)  ->
	Cmd = Wx#wx.event,
	case Cmd of
		#wxKey{keyCode = ?WXK_UP} ->
			% wxEvent:skip(Obj);
			wxproc ! up,
			ok;
		#wxKey{keyCode = ?WXK_DOWN} ->
			% wxEvent:skip(Obj);
			wxproc ! down,
			ok;
		#wxKey{keyCode = ?WXK_HOME} ->
			{Input,Prompt} = Wx#wx.userData,
			wxTextCtrl:setInsertionPoint(Input,length(Prompt));
			%wxEvent:skip(Obj);
		#wxKey{keyCode = K} when K == ?WXK_BACK; K == ?WXK_LEFT ->
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
map_print(P,[]) ->
	print(P,"No results.");
map_print(P,M) ->
	Keys = maps:keys(hd(M)),
	map_print(P,Keys,M,[]).

to_unicode(B) when is_binary(B) ->
	unicode:characters_to_list(B);
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
	StrVals = map_print1(Maps,L),
	print(P,"~ts",[StrVals]),
	print(P,"~s",[Delim1]).

map_print1([M|T],Keys) ->
	case T of
		[] ->
			End = "";
		_ ->
			End = "\n"
	end,
	[[io_lib:format("~ts",[string:left(to_unicode(maps:get(K,M)),Len+1,$\s)]) || {K,Len} <- Keys],"|",End|map_print1(T,Keys)];
map_print1([],_) ->
	[].
