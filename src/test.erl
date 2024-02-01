-module(test, [
               Id,
               Name,
               Type,
               Value
              ]).
-compile({parse_transform, nova_db_pt}).

-export([test/0]).
-export([value/2]).
-repo(my_repo).

test() ->
    ok.

value(Id, Name) ->
    {Id, Name}.
