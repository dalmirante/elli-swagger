-module(elli_swagger_handler).
-behaviour(elli_handler).

-export([handle/2, handle_event/3]).

handle(Req, _Config) ->
    Method = elli_request:method(Req),
    Path = elli_request:path(Req),

    handle_docs_request(Method, Path).

handle_event(_Atom, _Req, _Config) ->
    ignore.

handle_docs_request('GET', [<<"api-docs">>]) ->
    {302, [{<<"Location">>, <<"/api-docs/index.html">>}], <<"">>};
handle_docs_request('GET', [<<"api-docs">>, <<"swagger.json">>]) ->
    {ok, [{<<"Content-Type">>, <<"application/json">>}], elli_swagger:get_json_documentation()};
handle_docs_request('GET', [<<"api-docs">>, Filename]) ->
    StringFilename = binary_to_list(Filename),
    case elli_swagger_file:file_exists("swagger", StringFilename) of
        true ->
            {ok, [], {file, "swagger/"++StringFilename}};
        false ->
            {404, [], "Resource Not Found"}
    end;
handle_docs_request(_Method, _Path) ->
    ignore.
