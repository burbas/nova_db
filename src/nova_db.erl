-module(nova_db).
-export([
         find/2,
         find/3,
         save/1
        ]).

find(Table, Key) ->
    AdapterName = db_mod(Table),
    poolboy:transaction(AdapterName, fun(Worker) ->
                                    gen_server:call(Worker, {find, [Table, Key]})
                                end).

find(Table, Conditions, Options) ->
    AdapterName = db_mod(Table),
    poolboy:transaction(AdapterName, fun(Worker) ->
                                    gen_server:call(Worker, {find, [Table, Conditions, Options]})
                                end).

save(Record) ->
    Module = element(1, Record),
    case Module:validate(Record) of
        ok ->
            AdapterName = db_mod(Module),
            poolboy:transaction(AdapterName, fun(Worker) ->
                                                     gen_server:call(Worker, {save, [Record]})
                                             end);
        {error, Reasons} ->
            {error, Reasons}
    end.



%% Private functions
db_mod(Table) ->
    Connections = application:get_env(nova_db, connections, []),
    case proplists:lookup(connection, Table:module_info(attributes)) of
        none ->
            {DBMod, _Options} = hd(Connections),
            DBMod;
        {connection, Connection} ->
            Connection
    end.
