%% -*- mode: nitrogen -*-
-module(index).
-export([main/0]).
-include_lib("wf.hrl").


main() ->
    common:try_template("./templates/index.html").
