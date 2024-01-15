-module(nova_db_pt).

-export([
         parse_transform/2,
         format_error/1
         ]).

parse_transform(Forms, Options) ->
    case transform(Forms) of
        false ->
            Forms;
        {true, Forms0} ->
            pmod_pt:parse_transform(Forms0, Options)
    end.

format_error({Error, _Line, _Column}) ->
    Error.



transform(Forms0) ->
    case is_model(Forms0) of
        true ->
            {ok, BaseForms} = epp:parse_file("nova_db_base.erl", []),
            fix_exports(Forms0, BaseForms);
        _ ->
            false
    end.



fix_exports([], {Name, Exports, Ack}, _) ->
    %% Let's assemble
    [{attribute, 1, module, Name}|[{attribute, 2, export, Exports}|lists:reverse(Ack)]];
fix_exports([{attribute, _LineNo, module, Name}|Tl], {ModName, Exports, Ack}, SecondRun) ->
    case SecondRun of
        false ->
            %% We can skip this since it's the first occurence of module-attribute
            fix_exports(Tl, {Name, Exports, Ack}, true);
        _ ->
            fix_exports(Tl, {ModName, Exports, Ack}, SecondRun)
    end;
fix_exports([{attribute, _LineNo, export, Exports}|Tl], {Name, _, Ack}, false) ->
    %% We are at the right place - just extract the exports
    fix_exports(Tl, {Name, Exports, Ack}, true);
fix_exports([{attribute, _LineNo, export, Exports}|Tl], {Name, ExportsAck, Ack}, true) ->
    fix_exports(Tl, {Name, Exports++ExportsAck, Ack}, true);
fix_exports([Hd|Tl], {Name, Exports, Ack}, SecondRun) ->
    fix_exports(Tl, {Name, Exports, [Hd|Ack]}, SecondRun).


is_model([]) -> false;
is_model([{'-', _}, {atom, _, repo}, {'(', _}|Tl]) ->
    true;
is_model([Hd|Tl]) ->
    is_model(Tl).
