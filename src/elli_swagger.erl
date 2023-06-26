-module(elli_swagger).

-export([start_link/1]).

-type path() :: iodata().
-type http_method() :: atom().
-type mimetype() :: atom() | iodata().
-type http_status_code() :: non_neg_integer().

-type swagger_properties() :: #{iodata() => #{type => iodata(),
                                              description => iodata()}}.
-type swagger_schema() :: #{type := iodata(),
                            properties => swagger_properties()}.
-type responses_map() :: #{http_status_code() := #{description => iodata(),
                                                   schema => swagger_schema()}}.
-type path_contents() :: #{http_method() := #{summary => binary(),
                                              description => binary(),
                                              produces => [mimetype()],
                                              responses => responses_map() }}.
-type swagger_paths_field() :: #{path() := path_contents()}.
-type documentation() :: #{paths := swagger_paths_field(),
                           definitions => #{iodata() => responses_map()},
                           host => binary(),
                           schema => http | https,
                           basePath => binary()}.

-type elli_swagger_t() :: [{binary(), documentation()}].

%% Elli swagger plugin

init(State0) ->
    io:fwrite("The plugin worked as a charm!").

-export_type([elli_swagger_t/0]).

start_link(ElliOptions) ->
    CallbackModule = proplists:get_value(callback, ElliOptions),
    CallbackArgs = proplists:get_value(callback_args, ElliOptions),
    ElliConfiguration = delete_keys([callback, callback_args], ElliOptions),

    ElliSwaggerPaths =
        case lists:member({elli_swagger_config, 0}, CallbackModule:module_info(exports)) of
            true -> CallbackModule:elli_swagger_config();
            false -> []
        end,
    ElliSwaggerConfiguration = update_configuration_map(ElliSwaggerPaths, #{}),

    start_elli(ElliSwaggerConfiguration, [{CallbackModule, CallbackArgs}], ElliConfiguration).

delete_keys([], FinalList) ->
    FinalList;
delete_keys([Key|KeysList], List) ->
    NewList = lists:keydelete(Key, 1, List),
    delete_keys(KeysList, NewList).

start_elli(Metadata, ElliMiddlewareConfig, ElliConfiguration) ->
    ElliMiddleware = [{mods, [{elli_swagger_handler, Metadata}|ElliMiddlewareConfig]}],
    elli:start_link([{callback, elli_middleware}, {callback_args, ElliMiddleware}] ++ ElliConfiguration).

update_configuration_map([], Acc) ->
    Acc;
update_configuration_map([{Path, Metadata} | Configurations], Acc) ->
    PathKey = binary:split(Path, <<"/">>, [global, trim_all]),
    update_configuration_map(Configurations, Acc#{PathKey => Metadata}).
