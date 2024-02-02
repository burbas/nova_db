-module(nova_db_base).
-export([
         db_mod/0,
         save/0,
         validate/0
        ]).

%% We need to flag this with the nova_db_pt_ignore parse transform since we are using THIS variable without
%% it being defined in this module. THIS comes from the pmod_pt parse transform and will contain the
%% parameterized module that is inheriting from this module.
-compile({parse_transform, nova_db_pt_ignore}).

db_mod() ->
    Module = element(1, THIS),
    Connections = application:get_env(nova_db, connections, []),
    case proplists:lookup(connection, Module:module_info(attributes)) of
        none ->
            {DBMod, _Options} = hd(Connections),
            %% Take the first repo in the configuration
            DBMod;
        {connection, Connection} ->
            %% We trust the user to have given us a valid connection
            Connection
    end.

save() ->
    nova_db:save(THIS).


validate() ->
    ok.
