-module(master_holder_server).
-behaviour(gen_server).
-include("master.hrl").
%% API
-export([start_link/0]).
%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).
-define(NODE_TIMEOUT, 30).
-define(SWEEP_TIME, 5000).
-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    process_flag(trap_exit, true),
    master_lib:init_nodes(),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    ?TODO_LOG(),
    {reply, ok, State}.

handle_cast(_Request, State) ->
    ?TODO_LOG(),
    {noreply, State}.

handle_info(_Info, State) ->
    ?TODO_LOG(),
    {noreply, State}.

terminate(_Reason, _State) ->
    ?TODO_LOG(),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
