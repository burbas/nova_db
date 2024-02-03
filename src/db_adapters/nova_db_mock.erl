-module(nova_db_mock).
-export([
         init/1,
         save/2,
         find/3,
         find/4
        ]).

-behaviour(nova_db_adapter).

init(_Args) ->
    logger:info("Initializing mock DB"),
    case ets:info(?MODULE) of
        undefined ->
            ets:new(?MODULE, [named_table, public, {keypos, 2}]);
        _ -> ok
    end,
    #{}.


find(State, Type, Key) ->
    case ets:lookup(?MODULE, Key) of
        [] -> {ok, undefined};
        [Record] -> {ok, Record}
    end.

find(State, Type, Conditions, Options) ->
    Conditions0 = [{Key, Value} || {Key, Op, Value} <- Conditions, Op == eq],
    Attributes = Type:module_info(attributes),
    Fields = proplists:get_value(field_names, Attributes),
    Query = erlang:list_to_tuple([Type|build_query(Fields, Conditions0)]),
    Result = ets:match_object(?MODULE, Query),
    {reply, Result, State}.

save(State, Record) ->
    ets:insert(?MODULE, Record),
    {noreply, State}.


%% Private functions
build_query([], _Conditions) ->
    [];
build_query([Key|Tl], Conditions) ->
    case lists:keyfind(Key, 1, Conditions) of
        {_, Value} ->
            [Value|build_query(Tl, Conditions)];
        false ->
            ['_'|build_query(Tl, Conditions)]
    end.
