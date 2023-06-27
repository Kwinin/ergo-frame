-module(master_app).

-behaviour(application).

-include("master.hrl").
%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    init_app(),
    Res = master_sup:start_link(),
    Res.

stop(_State) ->
    ok.

%% ===================================================================
%% inner function
%% ===================================================================
init_app() ->
    lib_config:load_config(),
    ok.
