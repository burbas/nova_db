-module(nova_db_mock).
-export([
         init/1,
         save_record/2
        ]).


init(_Args) ->
    logger:info("Initializing mock DB"),
    case ets:info(?MODULE) of
        undefined ->
            ets:new(?MODULE, [named_table, public, {keypos, 3}]);
        _ -> ok
    end,
    #{}.

save_record(State, Record) ->
    ets:insert(?MODULE, Record),
    {noreply, State}.
