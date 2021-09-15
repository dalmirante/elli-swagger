-module(elli_swagger_example).
-behaviour(elli_handler).
-behaviour(elli_swagger_handler).

-export([start/0]).
-export([handle/2, handle_event/3]).
-export([elli_swagger_config/0]).

start() ->
    {ok, _} = elli_swagger:start_link([{callback, ?MODULE}, {port, 8080}]).

elli_swagger_config() ->
    Metadata =  #{get => #{summary => <<"Echoes a message back">>,
                           description => <<"This call will return the message inserted as echo back to you">>,
                           parameters => [#{in => path,
                                           name => echo_message,
                                           required => true,
                                           schema => #{
                                               example => <<"hello">>,
                                               type => <<"string">>
                                           },
                                           description => <<"The message you want to be echoed">>}],
                           produces => [<<"text/html">>],
                           responses => #{200 => #{description => <<"Ok">>,
                                                   schema => #{type => <<"string">>}}}
                        }},
    [{<<"/echo/{echo_message}">>, ?MODULE, [], Metadata}].

handle(Req, _Config) ->
    Request = elli_request:method(Req),
    Path = elli_request:path(Req),
    handle_request(Request, Path).

handle_event(_Event, _, _) -> ignore.

handle_request('GET', [<<"echo">>, EchoMsg]) ->
    {ok, [], EchoMsg}.
