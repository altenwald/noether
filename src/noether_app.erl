-module(noether_app).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-behaviour(application).
-behaviour(supervisor).

-export([start/2, stop/1, init/1]).

start(_StartType, _StartArgs) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

stop(_State) ->
    ok.

init([]) ->
    {ok, {{one_for_all, 10, 1}, []} }.
