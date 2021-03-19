-module(elli_swagger).

-export([get_json_documentation/0]).
-export([start/2]).

start(ElliHandlers, Port) ->
    ElliHandlerWithSwagger = [{elli_swagger_handler, []}|ElliHandlers],
    ElliMiddlewareArgs = [{mods, ElliHandlerWithSwagger}],
    elli:start_link([{callback, elli_middleware}, {callback_args, ElliMiddlewareArgs}, {port, Port}]).

get_json_documentation() ->
    Callback = application:get_env(elli_swagger, elli_swagger_documentation_callback, undefined),
    Metadata = application:get_env(elli_swagger, swagger_metadata, #{openapi => <<"3.0.0">>}),
    Documentation = get_documentation(Callback, #{}),
    jsx:encode(maps:merge(Metadata, Documentation)).

get_documentation(undefined, Acc) ->
    Acc;
get_documentation([], Accumulator) ->
    Accumulator;
get_documentation([Callback|CallbackList], Accumulator) ->
    Documentation = Callback:documentation(),
    get_documentation(CallbackList, maps:merge(Documentation, Accumulator));
get_documentation(Callback, #{}) ->
    Callback:documentation().