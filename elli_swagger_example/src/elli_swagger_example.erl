-module(elli_swagger_example).
-behaviour(elli_handler).
-behaviour(elli_swagger_documentation_callback).

-export([start/0]).
-export([handle/2, handle_event/3]).
-export([documentation/0]).

start() ->
    {ok, _} = elli_swagger:start([{elli_swagger_example, []}], 8080).

documentation() ->
    Path = #{<<"/echo/{message}">> => #{get => #{summary => <<"Echoes a message back">>,
                                            description => <<"This call will return the message inserted as echo back to you">>,
                                            parameters => [#{in => path,
                                                            name => message,
                                                            required => true,
                                                            schema => #{
                                                                example => <<"hello">>,
                                                                type => <<"string">>
                                                            },
                                                            description => <<"The message you want to be echoed">>}],
                                            produces => [<<"text/html">>],
                                            responses => #{200 => #{description => <<"Ok">>,
                                                                    schema => #{type => <<"string">>}}}
                                            }}},
    #{paths => Path}.

handle(Req, _Config) ->
    Request = elli_request:method(Req),
    Path = elli_request:path(Req),
    handle_request(Request, Path).

handle_event(_Event, _, _) -> ignore.

handle_request('GET', [<<"echo">>, EchoMsg]) ->
    {ok, [], EchoMsg}.