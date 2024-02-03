-module(nova_db_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([
         start_link/1
        ]).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2
        ]).


start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).


init(Args) ->
    logger:info("Starting NovaDB worker"),
    process_flag(trap_exit, true),
    Adapter = maps:get(adapter, Args),
    AdapterState = Adapter:init(Args),
    {ok, #{adapter => Adapter, adapter_state => AdapterState}}.

handle_call({Function, Args}, _From, State = #{adapter := Adapter, adapter_state := AdapterState}) ->
    case erlang:apply(Adapter, Function, [AdapterState|Args]) of
        {noreply, AdapterState0} ->
            %% We always need to respond :P
            {reply, ok, State#{adapter_state => AdapterState0}};
        {reply, Reply, AdapterState0} ->
            {reply, Reply, State#{adapter_state => AdapterState0}}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, #{adapter := Adapter, adapter_state := AdapterState}) ->
    logger:info("Terminating NovaDB worker with reason ~p", [Reason]),
    case erlang:function_exported(Adapter, terminate, 1) of
        true ->
            Adapter:terminate(AdapterState),
            ok;
        _ ->
            ok
    end.
