-module(nova_db_compiler).

compile(Filename) ->
    {ok, Tokens, _} = erl_scan:file(Filename),
    %% We need to modify the forms we've gotten from scanner
    %% to make them suitable for the parser.
    parse_tokens(Token),
    ok.


parse_tokens([{'-', _}, {'atom', _, 'module'}, {'(', _}, {'atom', _, ModuleName}, {')', _}|Tokens]) ->
    {Vars, Tokens0} = parse_vars(Tokens, []).



parse_vars([{var, _, _}=V|Vars], Ack) ->
    parse_vars(Vars, [V|Ack]);
parse_vars([_|_]=T, Ack) ->
    {lists:reverse(Ack), T}.
