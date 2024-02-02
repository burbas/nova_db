-module(test, [
               Id,
               Name,
               Type,
               Value
              ]).
-compile({parse_transform, nova_db_pt}).
-repo(test).
