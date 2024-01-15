%%%-------------------------------------------------------------------
%% @doc nova_db public API
%% @end
%%%-------------------------------------------------------------------

-module(nova_db_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    nova_db_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
