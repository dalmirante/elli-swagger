-module(elli_swagger_file).

-export([get_file/1, get_swagger_files/0]).

-define(NOT_FOUND_MESSAGE, "Resource Not Found").

get_file(Filename) ->
    retrieve_file([elli_swagger|get_documented_modules()], Filename).

get_swagger_files() ->
    DocumentedModules = get_documented_modules(),
    lists:foldl(fun(Module, PathsMap) ->
                    ModulePrivDir = get_priv_dir(Module),
                    [ModuleDocumentationFile] = list_dir(ModulePrivDir),
                    ModulePaths = get_paths_from_file(ModulePrivDir ++ ModuleDocumentationFile),
                    maps:update_with(<<"paths">>, fun(Value) ->
                                               maps:merge(Value, ModulePaths)
                                            end, ModulePaths, PathsMap)
                end, #{}, DocumentedModules).

get_paths_from_file(ModuleDocumentationFile) ->
    {ok, JSONDocumentation} = file:read_file(ModuleDocumentationFile),
    DocumentationMap = jsx:decode(JSONDocumentation, [return_maps]),
    maps:get(<<"paths">>, DocumentationMap).

get_documented_modules() ->
    application:get_env(elli_swagger, documented_modules, []).

get_priv_dir(ModuleName) ->
    code:priv_dir(ModuleName) ++ "/swagger/".

retrieve_file([], _Filename) ->
    {404, [], ?NOT_FOUND_MESSAGE};
retrieve_file([Module|ModulesList], Filename) ->
    ModulePrivDir = get_priv_dir(Module),

    case exists_in_directory(list_dir(ModulePrivDir), Filename) of
        true -> {ok, [], {file, ModulePrivDir ++ Filename}};
        false -> retrieve_file(ModulesList, Filename)
    end.

list_dir(Path) ->
    {ok, DirectoryEntries} = file:list_dir(Path),
    DirectoryEntries.

exists_in_directory([], _) -> false;
exists_in_directory([EntryFile|Entries], Filename) ->
    (EntryFile =:= Filename) orelse exists_in_directory(Entries, Filename).
