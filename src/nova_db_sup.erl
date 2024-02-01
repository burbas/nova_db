%%%-------------------------------------------------------------------
%% @doc nova_db top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(nova_db_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    logger:info("Starting nova_db"),
    Connections = application:get_env(nova_db, connections, []),
    PoolSpecs = lists:map(fun({Module, Options}) ->
                                  PoolArgs = [{name, {local, Module}},
                                              {worker_module, nova_db_worker},
                                              {size, maps:get(size, Options, 10)},
                                              {max_overflow, maps:get(max_overflow, Options, 5)}],
                                  %% Todo! Remove poolboy specific arguments from Options
                                  poolboy:child_spec(Module, PoolArgs, Options#{adapter => Module})
                          end, Connections),
    SupFlags = #{strategy => one_for_one,
                 intensity => 10,
                 period => 10},
    {ok, {SupFlags, PoolSpecs}}.

%% internal functions
