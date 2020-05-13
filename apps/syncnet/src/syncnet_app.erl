%%%-------------------------------------------------------------------
%% @doc syncnet public API
%% @end
%%%-------------------------------------------------------------------

-module(syncnet_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    syncnet_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
