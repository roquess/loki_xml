%%%-------------------------------------------------------------------
%%% @doc loki_xml_splitter - Splits XML into parseable chunks
%%% @end
%%%-------------------------------------------------------------------
-module(loki_xml_splitter).

-export([
    split_by_tag/2,
    split_fixed/2,
    validate_chunk/1
]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Split XML by specific tag using regex
-spec split_by_tag(binary(), binary()) -> {ok, [binary()]} | {error, term()}.
split_by_tag(Binary, Tag) ->
    %% Build regex pattern: <tag[^>]*>.*?</tag>
    %% Use simple pattern without capturing groups for better compatibility
    OpenTag = <<"<", Tag/binary>>,
    CloseTag = <<"</", Tag/binary, ">">>,
    
    case find_tag_chunks(Binary, OpenTag, CloseTag, []) of
        [] ->
            %% Fallback to fixed-size splitting
            io:format("No matches for tag ~p, falling back to fixed-size~n", [Tag]),
            split_fixed(Binary, 1048576);
        Chunks ->
            ValidChunks = lists:filter(fun validate_chunk/1, Chunks),
            {ok, ValidChunks}
    end.

%% Helper to find chunks by tag
find_tag_chunks(<<>>, _OpenTag, _CloseTag, Acc) ->
    lists:reverse(Acc);
find_tag_chunks(Binary, OpenTag, CloseTag, Acc) ->
    case binary:split(Binary, OpenTag) of
        [_Before, Rest] ->
            case binary:split(Rest, CloseTag) of
                [Content, Remaining] ->
                    Chunk = <<OpenTag/binary, Content/binary, CloseTag/binary>>,
                    find_tag_chunks(Remaining, OpenTag, CloseTag, [Chunk | Acc]);
                [_] ->
                    lists:reverse(Acc)
            end;
        [_] ->
            lists:reverse(Acc)
    end.

%% @doc Split XML into fixed-size chunks with boundary reconstruction
-spec split_fixed(binary(), pos_integer()) -> {ok, [binary()]}.
split_fixed(Binary, ChunkSize) when byte_size(Binary) =< ChunkSize ->
    {ok, [Binary]};
split_fixed(Binary, ChunkSize) ->
    Chunks = split_fixed_impl(Binary, ChunkSize, []),
    ReconstructedChunks = reconstruct_boundaries(Chunks),
    {ok, ReconstructedChunks}.

%% @doc Validate that a chunk is well-formed XML
-spec validate_chunk(binary()) -> boolean().
validate_chunk(<<>>) -> 
    false;
validate_chunk(Chunk) ->
    %% Simple validation: check for balanced opening and closing tags
    OpenCount = count_open_tags(Chunk),
    CloseCount = count_close_tags(Chunk),
    OpenCount == CloseCount andalso OpenCount > 0.

count_open_tags(Binary) ->
    %% Count opening tags, excluding self-closing and special tags
    Parts = binary:split(Binary, <<"<">>, [global]),
    lists:foldl(fun(Part, Count) ->
        case Part of
            <<>> -> Count;
            <<"/", _/binary>> -> Count;  %% Closing tag
            <<"!", _/binary>> -> Count;  %% Comment/CDATA
            <<"?", _/binary>> -> Count;  %% Processing instruction
            _ ->
                %% Check if it's a self-closing tag (ends with />)
                case binary:match(Part, <<"/>">>) of
                    {Pos, _} when Pos < byte_size(Part) -> Count;  %% Self-closing
                    _ -> Count + 1  %% Regular opening tag
                end
        end
    end, 0, Parts).

count_close_tags(Binary) ->
    Matches = binary:matches(Binary, <<"</">>),
    length(Matches).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

split_fixed_impl(<<>>, _, Acc) ->
    lists:reverse(Acc);
split_fixed_impl(Binary, ChunkSize, Acc) when byte_size(Binary) =< ChunkSize ->
    lists:reverse([Binary | Acc]);
split_fixed_impl(Binary, ChunkSize, Acc) ->
    %% Try to split at tag boundary near ChunkSize
    case find_tag_boundary(Binary, ChunkSize) of
        {ok, SplitPos} ->
            <<Chunk:SplitPos/binary, Rest/binary>> = Binary,
            split_fixed_impl(Rest, ChunkSize, [Chunk | Acc]);
        not_found ->
            %% Force split at ChunkSize
            <<Chunk:ChunkSize/binary, Rest/binary>> = Binary,
            split_fixed_impl(Rest, ChunkSize, [Chunk | Acc])
    end.

%% @doc Find nearest tag boundary near target position
find_tag_boundary(Binary, TargetPos) ->
    SearchStart = max(0, TargetPos - 1000),
    SearchEnd = min(byte_size(Binary), TargetPos + 1000),
    SearchSize = SearchEnd - SearchStart,
    
    <<_:SearchStart/binary, SearchRegion:SearchSize/binary, _/binary>> = Binary,
    
    %% Look for closing tag pattern
    case binary:matches(SearchRegion, [<<">">>]) of
        [] -> not_found;
        Matches ->
            %% Find closest match to target
            Closest = find_closest_match(Matches, TargetPos - SearchStart),
            {ok, SearchStart + Closest + 1}
    end.

find_closest_match(Matches, Target) ->
    [{Pos, _} | _] = lists:sort(fun({P1, _}, {P2, _}) ->
        abs(P1 - Target) =< abs(P2 - Target)
    end, Matches),
    Pos.

%% @doc Reconstruct tag boundaries across chunks
reconstruct_boundaries([]) -> [];
reconstruct_boundaries([Chunk]) -> [Chunk];
reconstruct_boundaries([C1, C2 | Rest]) ->
    case has_open_tag_at_end(C1) of
        true ->
            %% Merge with next chunk
            reconstruct_boundaries([<<C1/binary, C2/binary>> | Rest]);
        false ->
            [C1 | reconstruct_boundaries([C2 | Rest])]
    end.

has_open_tag_at_end(Binary) ->
    Size = byte_size(Binary),
    CheckSize = min(100, Size),
    <<_:(Size-CheckSize)/binary, Tail:CheckSize/binary>> = Binary,
    
    %% Count < and > in tail
    OpenCount = length(binary:matches(Tail, <<"<">>)),
    CloseCount = length(binary:matches(Tail, <<">">>)),
    OpenCount > CloseCount.
