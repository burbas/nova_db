-module(nova_db_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([
         start_link/0
        ]).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).


-export({
         adapter :: atom(),
         adapter_state :: any()
        }).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).


init(Args) ->
    logger:info("Starting NovaDB worker"),
    process_flag(trap_exit, true),
    {ok, Adapter} = maps:get(adapter, Args),
    case erlang:exported_function(Adapter, init, 1) of
        true ->
            AdapterState = Adapter:init(Args),
            {ok, #{adapter => Adapter, adapter_state => AdapterState}};
        _ ->
            {ok, #{adapter => Adapter}}
    end.

handle_call({Function, Args}, _From, State = #{adapter := Adapter, adapter_state := AdapterState}) ->
    Reply = erlang:apply(Adapter, Function, [AdapterState|Args]),
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, State = #{adapter := Adapter, adapter_state := AdapterState}) ->
    logger:info("Terminating NovaDB worker with reason ~p", [Reason]),
    case erlang:exported_function(Adapter, terminate, 1) of
        true ->
            Adapter:terminate(AdapterState),
            ok;
        _ ->
            ok
    end.
