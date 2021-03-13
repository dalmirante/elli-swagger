-module(elli_swagger_file).

-export([file_exists/2]).

file_exists(Path, Filename) ->
    Entries = list_directory(Path),
    exists_in_directory(Entries, Filename).

list_directory(Path) ->
    {ok, DirectoryEntries} = file:list_dir(Path),
    DirectoryEntries.

exists_in_directory([], _) -> false;
exists_in_directory([EntryFile|Entries], Filename) ->
    (EntryFile =:= Filename) orelse exists_in_directory(Entries, Filename).