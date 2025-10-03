%%%-------------------------------------------------------------------
%%% @doc loki_xml - Main API for distributed XML parser
%%% @end
%%%-------------------------------------------------------------------
-module(loki_xml).

-export([
    parse/1, parse/2,
    query/2,
    to_map/1,
    start/0, stop/0
]).

-type xml_element() :: {element, binary(), [{binary(), binary()}], [xml_element() | binary()]}.
-type parse_opts() :: #{
    format => term | map,
    distributed => boolean(),
    nodes => [node()],
    chunk_size => pos_integer(),
    tag => binary(),
    max_retries => non_neg_integer()
}.

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Start the loki_xml application
-spec start() -> ok | {error, term()}.
start() ->
    application:ensure_all_started(loki_xml).

%% @doc Stop the loki_xml application
-spec stop() -> ok.
stop() ->
    application:stop(loki_xml).

%% @doc Parse XML file with default options
-spec parse(file:filename() | binary()) -> {ok, [xml_element()]} | {error, term()}.
parse(Source) ->
    parse(Source, #{}).

%% @doc Parse XML file with options
-spec parse(file:filename() | binary(), parse_opts()) -> {ok, [xml_element()]} | {error, term()}.
parse(Source, Opts) ->
    Format = maps:get(format, Opts, term),
    Distributed = maps:get(distributed, Opts, false),
    Nodes = maps:get(nodes, Opts, [node()]),
    ChunkSize = maps:get(chunk_size, Opts, 1048576), % 1MB default
    Tag = maps:get(tag, Opts, undefined),
    
    %% Read source
    Binary = case Source of
        B when is_binary(B) -> B;
        Filename when is_list(Filename) ->
            case file:read_file(Filename) of
                {ok, Data} -> Data;
                {error, Reason} -> throw({error, {read_file, Reason}})
            end
    end,
    
    %% Split into chunks
    {ok, Chunks} = case Tag of
        undefined ->
            loki_xml_splitter:split_fixed(Binary, ChunkSize);
        TagName ->
            loki_xml_splitter:split_by_tag(Binary, TagName)
    end,
    
    %% Parse chunks
    ParsedChunks = case Distributed of
        true ->
            loki_xml_dist:distribute(Nodes, Chunks);
        false ->
            parse_chunks_local(Chunks, Opts)
    end,
    
    %% Merge results
    {ok, Merged} = loki_xml_merger:merge(ParsedChunks),
    
    %% Format conversion
    Result = case Format of
        map -> lists:map(fun to_map/1, Merged);
        _ -> Merged
    end,
    
    {ok, Result}.

%% @doc Run XPath query on parsed data
-spec query([xml_element()], binary()) -> [binary() | xml_element()].
query(Parsed, XPath) ->
    loki_xml_query:query(Parsed, XPath).

%% @doc Convert XML element to map
-spec to_map(xml_element()) -> map().
to_map({element, Tag, Attrs, Children}) ->
    ChildMaps = lists:map(fun
        (C) when is_binary(C) -> C;
        (C) -> to_map(C)
    end, Children),
    #{
        tag => Tag,
        attributes => maps:from_list(Attrs),
        children => ChildMaps
    }.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

parse_chunks_local(Chunks, Opts) ->
    MaxRetries = maps:get(max_retries, Opts, 3),
    Parent = self(),
    
    %% Spawn workers for each chunk
    Workers = lists:map(fun(Chunk) ->
        spawn_link(fun() ->
            Result = parse_chunk_with_retry(Chunk, MaxRetries),
            Parent ! {self(), Result}
        end)
    end, Chunks),
    
    %% Collect results
    collect_results(Workers, []).

parse_chunk_with_retry(Chunk, 0) ->
    {error, {max_retries, Chunk}};
parse_chunk_with_retry(Chunk, Retries) ->
    case loki_xml_parser:parse(Chunk) of
        {ok, Parsed} -> {ok, Parsed};
        {error, _} -> parse_chunk_with_retry(Chunk, Retries - 1)
    end.

collect_results([], Acc) ->
    lists:reverse(Acc);
collect_results([Worker|Rest], Acc) ->
    receive
        {Worker, Result} ->
            collect_results(Rest, [Result|Acc])
    after 30000 ->
        {error, timeout}
    end.
