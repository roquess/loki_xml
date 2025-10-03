%%%-------------------------------------------------------------------
%%% @doc loki_xml_sup - Top-level supervisor
%%% @end
%%%-------------------------------------------------------------------
-module(loki_xml_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/2]).
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Start the supervisor
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% @doc Dynamically start a child worker
-spec start_child(module(), [term()]) -> {ok, pid()} | {error, term()}.
start_child(Module, Args) ->
    ChildSpec = #{
        id => make_ref(),
        start => {Module, start_link, Args},
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [Module]
    },
    supervisor:start_child(?SERVER, ChildSpec).

%%%===================================================================
%%% Supervisor Callbacks
%%%===================================================================

%% @doc Initialize the supervisor with a one_for_one strategy
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },
    
    %% No static children - all workers are dynamic
    ChildSpecs = [],
    
    {ok, {SupFlags, ChildSpecs}}.
