%%%-------------------------------------------------------------------
%%% @doc loki_xml_merger - Merges parsed chunks and handles errors
%%% @end
%%%-------------------------------------------------------------------
-module(loki_xml_merger).

-export([merge/1]).

-type parse_result() :: {ok, [term()]} | {error, term()}.

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Merge results from multiple chunk parsers
-spec merge([parse_result()]) -> {ok, [term()]} | {error, term()}.
merge(Results) ->
    {OkResults, Errors} = partition_results(Results),
    
    case {OkResults, Errors} of
        {[], []} ->
            {ok, []};
        {[], Errors} ->
            %% All failed
            {error, {all_chunks_failed, Errors}};
        {OkResults, []} ->
            %% All succeeded
            {ok, flatten_results(OkResults)};
        {OkResults, Errors} ->
            %% Partial success - log errors but return successful results
            log_errors(Errors),
            {ok, flatten_results(OkResults)}
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

partition_results(Results) ->
    partition_results(Results, [], []).

partition_results([], OkAcc, ErrAcc) ->
    {lists:reverse(OkAcc), lists:reverse(ErrAcc)};
partition_results([{ok, Data} | Rest], OkAcc, ErrAcc) ->
    partition_results(Rest, [Data | OkAcc], ErrAcc);
partition_results([{error, Reason} | Rest], OkAcc, ErrAcc) ->
    partition_results(Rest, OkAcc, [Reason | ErrAcc]);
partition_results([Other | Rest], OkAcc, ErrAcc) ->
    %% Handle unexpected results
    partition_results(Rest, OkAcc, [{unexpected_result, Other} | ErrAcc]).

flatten_results(Results) ->
    lists:flatten(Results).

log_errors([]) ->
    ok;
log_errors(Errors) ->
    io:format("[loki_xml_merger] Errors encountered during parsing:~n"),
    lists:foreach(fun(Error) ->
        io:format("  - ~p~n", [Error])
    end, Errors),
    ok.
