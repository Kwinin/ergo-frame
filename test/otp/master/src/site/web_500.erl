%% -*- mode: nitrogen -*-
-module(web_500).
-export([main/0]).
-include_lib("../../../deps/nitrogen_core/include/wf.hrl").
-include("master.hrl").

main() ->
    #template{file = "./templates/common/web_500.html"}.
