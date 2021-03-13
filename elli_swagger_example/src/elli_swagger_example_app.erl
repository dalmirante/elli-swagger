%%%-------------------------------------------------------------------
%% @doc elli_swagger_example public API
%% @end
%%%-------------------------------------------------------------------

-module(elli_swagger_example_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    elli_swagger_example:start().

stop(_State) ->
    ok.

%% internal functions
