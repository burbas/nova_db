-module(nova_db_pt_ignore).
-export([
         parse_transform/2
        ]).


parse_transform(Forms, _Options) ->
    get_module_attr(Forms).



get_module_attr([{attribute, _, module, _Module}=M|_Rest]) ->
    [M];
get_module_attr([_Hd|Tl]) ->
    get_module_attr(Tl).
