-module(elli_swagger_handler).
-behaviour(elli_handler).


-export([handle/2, handle_event/3]).

-callback elli_swagger_config() -> elli_swagger:elli_swagger_t().
-optional_callbacks([elli_swagger_config/0]).

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
    SwaggerMetadata = application:get_env(elli_swagger,
                                          swagger_metadata,
                                          #{openapi => <<"3.0.0">>}),
    Documentation = generate_documentation(PathConfiguration),
    {ok, [{<<"Content-Type">>, <<"application/json">>}],
     jsx:encode(maps:merge(SwaggerMetadata, Documentation))};
handle_docs_request('GET', [<<"api-docs">>, Filename], _PathConfiguration) ->
    StringFilename = binary_to_list(Filename),
    elli_swagger_file:get_file(StringFilename);
handle_docs_request(_Method, _Path, _PathConfiguration) ->
    ignore. % Redirect to some other module

generate_documentation(PathConfiguration) when map_size(PathConfiguration) =:= 0 ->
    elli_swagger_file:get_swagger_files();
generate_documentation(PathConfiguration) ->
    Documentation =
        maps:fold(fun(Key, Metadata, Acc) ->
                        Path = [<<"/">> | lists:join(<<"/">>, Key)],
                        Acc#{list_to_binary(Path) => Metadata}
                  end,
                  #{}, PathConfiguration),
    #{<<"paths">> => Documentation}.
