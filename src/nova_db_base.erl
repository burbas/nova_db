-module(nova_db_base).
-export([
         save/0,
         validate/0
        ]).

%% We need to flag this with the nova_db_pt_ignore parse transform since we are using THIS variable without
%% it being defined in this module. THIS comes from the pmod_pt parse transform and will contain the
%% parameterized module that is inheriting from this module.
-compile({parse_transform, nova_db_pt_ignore}).

save() ->
    nova_db:save(THIS).

validate() ->
    ok.
