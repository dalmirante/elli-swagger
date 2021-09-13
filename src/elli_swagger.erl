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

-type elli_swagger_t() :: {binary(), term(), module(), documentation()}.

-export_type([elli_swagger_t/0]).

start_link(ElliOptions) ->
    Modules = proplists:get_all_values(callback, ElliOptions),
    ElliConfiguration = proplists:delete(callback, ElliOptions),
    {ElliSwaggerConfiguration, ElliMiddlewareConfig} = start(Modules, {#{}, []}),
    start_elli(ElliSwaggerConfiguration, ElliMiddlewareConfig, ElliConfiguration).

start([], Acc) ->
    Acc;
start([Module|Modules], {MapAcc, ModulesAcc}) ->
    ElliSwaggerConfiguration = Module:elli_swagger_config(),
    start(Modules,  {update_configuration_map(ElliSwaggerConfiguration, MapAcc), [{Module, []} | ModulesAcc]}).


start_elli(Metadata, ElliMiddlewareConfig, ElliConfiguration) ->
    ElliMiddleware = [{mods, [{elli_swagger_handler, Metadata}|ElliMiddlewareConfig]}],
    elli:start_link([{callback, elli_middleware}, {callback_args, ElliMiddleware}] ++ ElliConfiguration).

update_configuration_map([], Acc) ->
    Acc;
update_configuration_map([{Path, Handler, Arguments, Metadata} | Configurations], Acc) ->
    PathKey = binary:split(Path, <<"/">>, [global, trim_all]),
    update_configuration_map(Configurations, Acc#{PathKey => #{handler => Handler,
                                                               arguments => Arguments,
                                                               metadata => Metadata}}).
