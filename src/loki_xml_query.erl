%%%-------------------------------------------------------------------
%%% @doc loki_xml_query - XPath query engine
%%% @end
%%%-------------------------------------------------------------------
-module(loki_xml_query).

-export([query/2]).

-type xml_element() :: {element, binary(), [{binary(), binary()}], [xml_element() | binary()]}.

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Execute XPath query on parsed XML
-spec query([xml_element()] | xml_element(), binary()) -> [binary() | xml_element()].
query(Elements, XPath) when is_list(Elements) ->
    lists:flatmap(fun(E) -> query(E, XPath) end, Elements);
query(Element, XPath) ->
    case parse_xpath(XPath) of
        {ok, ParsedPath} ->
            execute_query(Element, ParsedPath);
        {error, _} ->
            []
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% Parse XPath into executable steps
parse_xpath(<<"//", Rest/binary>>) ->
    %% Descendant-or-self axis - search anywhere in tree
    case Rest of
        <<>> -> {ok, [{descendant_or_self}]};
        Path -> {ok, [{descendant_or_self}, {tag, Path}]}
    end;
parse_xpath(<<"/", Rest/binary>>) ->
    %% Child axis from root
    {ok, [{root} | parse_path_steps(Rest)]};
parse_xpath(<<"@", AttrName/binary>>) ->
    %% Attribute
    {ok, [{attribute, AttrName}]};
parse_xpath(Path) ->
    %% Direct path
    {ok, parse_path_steps(Path)}.

parse_path_steps(Binary) ->
    Steps = binary:split(Binary, <<"/">>, [global]),
    lists:map(fun(Step) ->
        case Step of
            <<"@", Attr/binary>> -> {attribute, Attr};
            <<>> -> {self};
            TagName -> {child, TagName}
        end
    end, Steps).

%% Execute parsed query
execute_query(Element, []) ->
    [Element];
execute_query(Elements, Steps) when is_list(Elements) ->
    lists:flatmap(fun(E) -> execute_query(E, Steps) end, Elements);
execute_query(Element, [{root} | Rest]) ->
    execute_query(Element, Rest);
execute_query({element, _, _, _} = Element, [{descendant_or_self}, {tag, Tag}]) ->
    %% Special case: //tag means find all descendants with that tag
    AllDescendants = get_all_descendants(Element),
    lists:filter(fun
        ({element, T, _, _}) -> T =:= Tag;
        (_) -> false
    end, AllDescendants);
execute_query({element, _, _, _} = Element, [{descendant_or_self}]) ->
    %% Just get all descendants
    [Element | get_all_descendants(Element)];
execute_query({element, Tag, Attrs, Children}, [{child, Tag} | Rest]) ->
    %% Current element matches
    case Rest of
        [] -> 
            %% Return just this element
            [{element, Tag, Attrs, Children}];
        [{attribute, AttrName}] ->
            %% Extract attribute from current element
            case lists:keyfind(AttrName, 1, Attrs) of
                {_, Value} -> [Value];
                false -> []
            end;
        _ -> 
            %% Continue with children (filter out text nodes)
            ElementChildren = lists:filter(fun
                ({element, _, _, _}) -> true;
                (_) -> false
            end, Children),
            lists:flatmap(fun(C) -> execute_query(C, Rest) end, ElementChildren)
    end;
execute_query({element, _, _, Children}, [{child, TargetTag} | Rest]) ->
    %% Search in children for matching elements only (not text nodes)
    MatchingChildren = lists:filter(fun
        ({element, T, _, _}) -> T =:= TargetTag;
        (_) -> false
    end, Children),
    case Rest of
        [] -> MatchingChildren;
        _ -> lists:flatmap(fun(C) -> execute_query(C, Rest) end, MatchingChildren)
    end;
execute_query({element, _, Attrs, _}, [{attribute, AttrName}]) ->
    %% Extract attribute value
    case lists:keyfind(AttrName, 1, Attrs) of
        {_, Value} -> [Value];
        false -> []
    end;
execute_query({element, _, _, Children}, Steps) ->
    %% Continue searching in children, but filter out text nodes
    ElementChildren = lists:filter(fun
        ({element, _, _, _}) -> true;
        (_) -> false
    end, Children),
    lists:flatmap(fun(C) -> execute_query(C, Steps) end, ElementChildren);
execute_query(_, _) ->
    [].

%% Get all descendants recursively (elements only, not text nodes)
get_all_descendants({element, _, _, Children}) ->
    ElementChildren = lists:filter(fun
        ({element, _, _, _}) -> true;
        (_) -> false
    end, Children),
    ElementChildren ++ lists:flatmap(fun get_all_descendants/1, ElementChildren);
get_all_descendants(_) ->
    [].
