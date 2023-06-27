%% -*- mode: nitrogen -*-
-module(web_404).
-export([main/0]).
-include_lib("../../../deps/nitrogen_core/include/wf.hrl").
-include("master.hrl").

main() ->
    #template{file = "./templates/common/web_404.html"}.
