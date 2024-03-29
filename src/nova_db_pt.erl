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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

transform(Forms0) ->
    case is_model(Forms0) of
        true ->
            SrcDir = code:lib_dir(nova_db, src),
            BaseFile = filename:join(SrcDir, "nova_db_base.erl"),
            {ok, BaseForms} = epp:parse_file(BaseFile, []),
            ExportsForms0 = concat_exports(Forms0, []),
            ExportsBaseForms = concat_exports(BaseForms, ExportsForms0),
            BaseForms0 = exclude_functions(BaseForms, ExportsForms0),
            case ExportsForms0 of
                [] ->
                    %% We don't have any exports from the model
                    {true, inject_mod(BaseForms0, Forms0, ExportsBaseForms, true)};
                _ ->
                    {true, inject_mod(lists:droplast(BaseForms0), Forms0,
                                      ExportsBaseForms ++ ExportsForms0, false)}
            end;
        _ ->
            false
    end.


inject_mod(BaseForms, [], _Exports, _ExportAfterMod) ->
    BaseForms;
inject_mod(BaseForms, [{attribute, Row, module, ModuleDef}=A|Tl], Exports, ExportAfterMod) ->
    case ModuleDef of
        {_Name, ListOfFields} ->
            ListOfFields0 = [ erlang:list_to_atom(string:lowercase(erlang:atom_to_list(X))) || X <- ListOfFields ],
            case ExportAfterMod of
                true ->
                    [A, {attribute, Row, export, Exports}, {attribute, Row, field_names, ListOfFields0}|inject_mod(BaseForms, Tl, undefined, true)];
                false ->
                    [A, {attribute, Row, field_names, ListOfFields0}|inject_mod(BaseForms, Tl, undefined, true)]
            end;
        Name ->
            [A, {attribute, Row, export, Exports}|inject_mod(BaseForms, Tl, undefined, true)]
        end;
inject_mod(BaseForms, [{attribute, Row, export, _}|Tl], Exports, false) ->
    case Exports of
        undefined ->
            inject_mod(BaseForms, Tl, Exports, false);
        _ ->
            [{attribute, Row, export, Exports}|inject_mod(BaseForms, Tl, undefined, false)]
    end;
inject_mod(BaseForms, [{function, _Row, _Func, _Arity, _Clauses}|_Tl]=F, _Exports, _ExportAfterMod) ->
    BaseForms ++ F;
inject_mod(BaseForms, [Hd|Tl], Exports, ExportAfterMod) ->
    [Hd|inject_mod(BaseForms, Tl, Exports, ExportAfterMod)].

exclude_functions([], _) ->
    [];
exclude_functions([{attribute, _, _, _}|Tl], ExcludedFunctions) ->
    exclude_functions(Tl, ExcludedFunctions);
exclude_functions([{function, _, Func, Arity, _}=F|Tl], ExcludedFunctions) ->
    case lists:member({Func, Arity}, ExcludedFunctions) of
        true ->
            exclude_functions(Tl, ExcludedFunctions);
        false ->
            [F|exclude_functions(Tl, ExcludedFunctions)]
    end;
exclude_functions([Hd|Tl], ExcludedFunctions) ->
    [Hd|exclude_functions(Tl, ExcludedFunctions)].

concat_exports([], _) ->
    [];
concat_exports([{attribute, _, export, List}|Tl], ExcludeList) ->
    concat_exports_list(List, ExcludeList) ++ concat_exports(Tl, ExcludeList);
concat_exports([_Hd|Tl], ExcludeList) ->

    concat_exports(Tl, ExcludeList).

concat_exports_list([], _) -> [];
concat_exports_list([{Func, Arity}=T|Tl], ExcludeList) when is_atom(Func) andalso
                                                            is_integer(Arity)  ->
    case lists:member(T, ExcludeList) of
        true ->
            concat_exports_list(Tl, ExcludeList);
        false ->
            [T|concat_exports_list(Tl, ExcludeList)]
    end.

is_model([]) -> false;
is_model([{attribute, _, repo, _}|_Tl]) ->
    true;
is_model([_Hd|Tl]) ->
    is_model(Tl).
