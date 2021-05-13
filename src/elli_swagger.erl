-module(elli_swagger).

-export([start_link/1,
         start_link/2]).

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

start_link(Modules) ->
    start(Modules, 8080, #{}).

start_link(Modules, Port) ->
    start(Modules, Port, #{}).

start([], Port, Acc) ->
    start_elli(Acc, Port);
start([Module|Modules], Port, Acc) ->
    ElliSwaggerConfiguration = Module:elli_swagger_config(),
    start(Modules, Port, update_configuration_map(ElliSwaggerConfiguration, Acc)).


start_elli(Metadata, Port) ->
    ElliMiddleware = [{mods, [{elli_swagger_handler, Metadata}]}],
    elli:start_link([{callback, elli_middleware}, {callback_args, ElliMiddleware}, {port, Port}]).

update_configuration_map([], Acc) ->
    Acc;
update_configuration_map([{Path, Handler, Arguments, Metadata} | Configurations], Acc) ->
    PathKey = binary:split(Path, <<"/">>, [global, trim_all]),
    update_configuration_map(Configurations, Acc#{PathKey => #{handler => Handler,
                                                               arguments => Arguments,
                                                               metadata => Metadata}}).
