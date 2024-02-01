-module(nova_db_base).
-export([
         save/1,
         validate/1,
         value/2
        ]).

save(Record) ->
    case validate(Record) of
        ok ->
            save_record(Record);
        {error, Reason} ->
            {error, Reason}
    end.


%% Only used to be overridden
validate(_Record) -> true.

value(Record, Field) ->
    Attributes = ?MODULE:module_info(attributes),
    {_, RecordInfo} = lists:keyfind(record_info, 1, Attributes),
    Index = get_index(Field, RecordInfo),
    element(Index, Record+1). %% +1 since we have removed the record-name

%% Private functions
save_record(Record) ->
    nova_db:save(Record).


get_index(Field, []) -> throw({error, {field_not_found, Field}});
get_index(Field, [Field|_]) ->
    1;
get_index(Field, [_Hd|Tl]) ->
    1+get_index(Field, Tl).
