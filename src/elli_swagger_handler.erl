-module(elli_swagger_handler).
-behaviour(elli_handler).

-define(NOT_FOUND_MESSAGE, "Resource Not Found").

-export([handle/2, handle_event/3]).

-callback elli_swagger_config() -> elli_swagger:elli_swagger_t().

%% elli
handle(Req, PathConfig) ->
    Method = elli_request:method(Req),
    Path = elli_request:path(Req),

    handle_docs_request(Method, Path, PathConfig).

handle_event(_Atom, _Req, _Config) ->
    ignore.

%% internal functions
handle_docs_request('GET', [<<"api-docs">>], _PathConfiguration) ->
    {302, [{<<"Location">>, <<"/api-docs/index.html">>}], <<"">>};
handle_docs_request('GET', [<<"api-docs">>, <<"swagger.json">>], PathConfiguration) ->
    {ok, [{<<"Content-Type">>, <<"application/json">>}], fetch_documentation_from_configuration(PathConfiguration)};
handle_docs_request('GET', [<<"api-docs">>, Filename], _PathConfiguration) ->
    StringFilename = binary_to_list(Filename),
    case elli_swagger_file:file_exists("swagger", StringFilename) of
        true ->
            {ok, [], {file, "swagger/"++StringFilename}};
        false ->
            {404, [], ?NOT_FOUND_MESSAGE}
    end;
handle_docs_request(_Method, _Path, _PathConfiguration) ->
    ignore. % Redirect to some other module

fetch_documentation_from_configuration(PathConfiguration) ->
    SwaggerMetadata = application:get_env(elli_swagger,
                                          swagger_metadata,
                                          #{openapi => <<"3.0.0">>}),
    BuiltDocumentation =
        maps:fold(fun(Key, #{metadata := Metadata}, Acc) ->
                    Path = [<<"/">> | lists:join(<<"/">>, Key)],
                    Acc#{list_to_binary(Path) => Metadata}
                  end,
                  #{}, PathConfiguration),
    jsx:encode(maps:merge(SwaggerMetadata, #{paths => BuiltDocumentation})).
