-module(actordb_wxconsole).
-include_lib("wx/include/wx.hrl").
-define(PROMPT,"actordb>").
-export([main/1, wxrun/0]).

main(A) ->
	actordb_console:main(A).



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
