%%%-------------------------------------------------------------------
%%% @doc loki_xml_dist - Distributed chunk processing across nodes
%%% @end
%%%-------------------------------------------------------------------
-module(loki_xml_dist).

-export([
    distribute/2,
    parse_chunk_rpc/1
]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Distribute chunks across multiple Erlang nodes
-spec distribute([node()], [binary()]) -> [{ok, term()} | {error, term()}].
distribute(Nodes, Chunks) ->
    %% Ensure nodes are available
    ConnectedNodes = ensure_connected(Nodes),
    
    case ConnectedNodes of
        [] ->
            %% Fallback to local processing
            io:format("[loki_xml_dist] No nodes available, processing locally~n"),
            process_local(Chunks);
        _ ->
            %% Distribute chunks round-robin across nodes
            distribute_chunks(ConnectedNodes, Chunks)
    end.

%% @doc RPC endpoint for parsing a chunk on remote node
-spec parse_chunk_rpc(binary()) -> {ok, [term()]} | {error, term()}.
parse_chunk_rpc(Chunk) ->
    loki_xml_parser:parse(Chunk).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

ensure_connected(Nodes) ->
    lists:filter(fun(Node) ->
        case net_adm:ping(Node) of
            pong -> true;
            pang ->
                io:format("[loki_xml_dist] Node ~p not reachable~n", [Node]),
                false
        end
    end, Nodes).

distribute_chunks(Nodes, Chunks) ->
    %% Create node-chunk pairs using round-robin
    NodeChunkPairs = assign_chunks_to_nodes(Nodes, Chunks),
    
    %% Group chunks by node for batch processing
    ChunksByNode = group_by_node(NodeChunkPairs),
    
    %% Process on each node
    Results = lists:flatmap(fun({Node, NodeChunks}) ->
        process_on_node(Node, NodeChunks)
    end, maps:to_list(ChunksByNode)),
    
    Results.

assign_chunks_to_nodes(Nodes, Chunks) ->
    NodeCount = length(Nodes),
    lists:zip(Chunks, lists:flatten(lists:duplicate(length(Chunks) div NodeCount + 1, Nodes))).

group_by_node(NodeChunkPairs) ->
    lists:foldl(fun({Chunk, Node}, Acc) ->
        maps:update_with(Node, fun(Chunks) -> [Chunk | Chunks] end, [Chunk], Acc)
    end, #{}, NodeChunkPairs).

process_on_node(Node, Chunks) ->
    Parent = self(),
    
    %% Spawn a process for each chunk on the remote node
    Pids = lists:map(fun(Chunk) ->
        spawn(fun() ->
            Result = rpc:call(Node, ?MODULE, parse_chunk_rpc, [Chunk], 30000),
            Parent ! {self(), Result}
        end)
    end, Chunks),
    
    %% Collect results
    collect_results(Pids, []).

collect_results([], Acc) ->
    lists:reverse(Acc);
collect_results([Pid | Rest], Acc) ->
    receive
        {Pid, Result} ->
            collect_results(Rest, [Result | Acc])
    after 30000 ->
        %% Timeout - mark as error
        collect_results(Rest, [{error, {timeout, Pid}} | Acc])
    end.

process_local(Chunks) ->
    lists:map(fun(Chunk) ->
        loki_xml_parser:parse(Chunk)
    end, Chunks).
