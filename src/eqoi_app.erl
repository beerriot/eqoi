%%%-------------------------------------------------------------------
%% @doc eqoi public API
%% @end
%%%-------------------------------------------------------------------

-module(eqoi_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    eqoi_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
