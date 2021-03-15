-module(elli_swagger_example).
-behaviour(elli_handler).
-behaviour(elli_swagger_documentation_callback).

-export([start/0]).
-export([handle/2, handle_event/3]).
-export([documentation/0]).

start() ->
    CallbackArguments = [
                            {mods, [{elli_swagger_handler, []},
                                    {elli_swagger_example, []}]}
                    ],
    {ok, _} = elli:start_link([{callback, elli_middleware}, {callback_args, CallbackArguments}, {port, 8081}]).

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
                                            responses => #{200 => #{description => <<"Ok">>,
                                                                    content => #{<<"text/plain">> => #{
                                                                                    schema => #{type => <<"string">>}}}}},
                                            tags => [<<"message">>]}}},
    #{paths => Path}.

handle(Req, _Config) ->
    Request = elli_request:method(Req),
    Path = elli_request:path(Req),
    handle_request(Request, Path).

handle_event(_Event, _, _) -> ignore.

handle_request('GET', [<<"echo">>, EchoMsg]) ->
    {ok, [{<<"Content-Type">>, <<"text/plain">>}], EchoMsg}.