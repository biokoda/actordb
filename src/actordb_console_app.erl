%%%-------------------------------------------------------------------
%% @doc actordb_console public API
%% @end
%%%-------------------------------------------------------------------

-module(actordb_console_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    actordb_console_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
