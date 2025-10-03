%%%-------------------------------------------------------------------
%%% @doc loki_xml application module
%%% @end
%%%-------------------------------------------------------------------
-module(loki_xml_app).

-behaviour(application).

-export([start/2, stop/1]).

%%%===================================================================
%%% Application Callbacks
%%%===================================================================

%% @doc Start the loki_xml application
start(_StartType, _StartArgs) ->
    io:format("Starting loki_xml application~n"),
    loki_xml_sup:start_link().

%% @doc Stop the loki_xml application
stop(_State) ->
    io:format("Stopping loki_xml application~n"),
    ok.
