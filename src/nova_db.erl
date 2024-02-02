-module(nova_db).
-export([
         save/1
        ]).


save(Record) ->
    io:format("Record: ~p~n", [Record]),
    Module = element(1, Record),
    io:format("Module: ~p~n", [Module]),
    case Module:validate(Record) of
        ok ->
            AdapterName = Module:db_mod(Record),
            poolboy:transaction(AdapterName, fun(Worker) ->
                                                     gen_server:call(Worker, {save_record, [Record]})
                                             end);
        {error, Reasons} ->
            {error, Reasons}
    end.
