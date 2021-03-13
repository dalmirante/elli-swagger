-module(elli_swagger_documentation_callback).

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

-callback documentation() -> documentation().