%%%-------------------------------------------------------------------
%%% @doc loki_xml test suite
%%% @end
%%%-------------------------------------------------------------------
-module(loki_xml_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

simple_xml() ->
    <<"<book id=\"1\">
        <title>Erlang Programming</title>
        <author>Joe Armstrong</author>
      </book>">>.

multi_element_xml() ->
    <<"<library>
        <book id=\"1\">
          <title>Erlang Programming</title>
          <author>Joe Armstrong</author>
        </book>
        <book id=\"2\">
          <title>Learn You Some Erlang</title>
          <author>Fred Hebert</author>
        </book>
      </library>">>.

self_closing_xml() ->
    <<"<root>
        <item id=\"1\" />
        <item id=\"2\" />
      </root>">>.

nested_xml() ->
    <<"<company>
        <department name=\"Engineering\">
          <team name=\"Backend\">
            <member>Alice</member>
            <member>Bob</member>
          </team>
        </department>
      </company>">>.

%%%===================================================================
%%% Parser Tests
%%%===================================================================

parse_simple_test() ->
    {ok, Result} = loki_xml_parser:parse(simple_xml()),
    ?assertMatch([{element, <<"book">>, _, _}], Result).

parse_attributes_test() ->
    {ok, [{element, <<"book">>, Attrs, _}]} = loki_xml_parser:parse(simple_xml()),
    ?assertEqual([{<<"id">>, <<"1">>}], Attrs).

parse_children_test() ->
    {ok, [{element, <<"book">>, _, Children}]} = loki_xml_parser:parse(simple_xml()),
    ?assertEqual(2, length(Children)).

parse_self_closing_test() ->
    {ok, Result} = loki_xml_parser:parse(self_closing_xml()),
    ?assertMatch([{element, <<"root">>, [], _}], Result).

parse_nested_test() ->
    {ok, Result} = loki_xml_parser:parse(nested_xml()),
    ?assertMatch([{element, <<"company">>, [], _}], Result).

%%%===================================================================
%%% Splitter Tests
%%%===================================================================

split_by_tag_test() ->
    XML = <<"<record id=\"1\"><data>A</data></record><record id=\"2\"><data>B</data></record>">>,
    {ok, Chunks} = loki_xml_splitter:split_by_tag(XML, <<"record">>),
    ?assertEqual(2, length(Chunks)).

split_fixed_small_test() ->
    XML = <<"<root><item>test</item></root>">>,
    {ok, Chunks} = loki_xml_splitter:split_fixed(XML, 1000),
    ?assertEqual(1, length(Chunks)).

split_fixed_large_test() ->
    %% Create a large XML
    LargeXML = binary:copy(<<"<item>data</item>">>, 100),
    {ok, Chunks} = loki_xml_splitter:split_fixed(LargeXML, 100),
    ?assert(length(Chunks) > 1).

validate_chunk_test() ->
    ValidChunk = <<"<item>test</item>">>,
    InvalidChunk = <<"<item>test">>,
    ?assert(loki_xml_splitter:validate_chunk(ValidChunk)),
    ?assertNot(loki_xml_splitter:validate_chunk(InvalidChunk)).

%%%===================================================================
%%% Query Tests
%%%===================================================================

query_descendant_test() ->
    {ok, Parsed} = loki_xml_parser:parse(multi_element_xml()),
    Titles = loki_xml_query:query(Parsed, <<"//title">>),
    ?assertEqual(2, length(Titles)).

query_child_test() ->
    {ok, Parsed} = loki_xml_parser:parse(multi_element_xml()),
    Books = loki_xml_query:query(Parsed, <<"//book">>),
    ?assertEqual(2, length(Books)).

query_attribute_test() ->
    {ok, Parsed} = loki_xml_parser:parse(simple_xml()),
    Ids = loki_xml_query:query(Parsed, <<"@id">>),
    ?assertEqual([<<"1">>], Ids).

query_nested_path_test() ->
    {ok, Parsed} = loki_xml_parser:parse(nested_xml()),
    Members = loki_xml_query:query(Parsed, <<"//member">>),
    ?assertEqual(2, length(Members)).

%%%===================================================================
%%% Merger Tests
%%%===================================================================

merge_success_test() ->
    Results = [
        {ok, [{element, <<"item">>, [], []}]},
        {ok, [{element, <<"item">>, [], []}]}
    ],
    {ok, Merged} = loki_xml_merger:merge(Results),
    ?assertEqual(2, length(Merged)).

merge_partial_failure_test() ->
    Results = [
        {ok, [{element, <<"item">>, [], []}]},
        {error, parse_error},
        {ok, [{element, <<"item">>, [], []}]}
    ],
    {ok, Merged} = loki_xml_merger:merge(Results),
    ?assertEqual(2, length(Merged)).

merge_all_failure_test() ->
    Results = [
        {error, parse_error},
        {error, timeout}
    ],
    {error, {all_chunks_failed, _}} = loki_xml_merger:merge(Results).

%%%===================================================================
%%% Integration Tests
%%%===================================================================

parse_and_query_integration_test() ->
    {ok, Parsed} = loki_xml:parse(multi_element_xml()),
    Titles = loki_xml:query(Parsed, <<"//title">>),
    ?assertEqual(2, length(Titles)).

parse_with_map_format_test() ->
    {ok, [Map]} = loki_xml:parse(simple_xml(), #{format => map}),
    ?assertMatch(#{tag := <<"book">>, attributes := _, children := _}, Map).

to_map_conversion_test() ->
    {ok, [Element]} = loki_xml_parser:parse(simple_xml()),
    Map = loki_xml:to_map(Element),
    ?assertMatch(#{tag := <<"book">>}, Map),
    ?assertMatch(#{attributes := #{<<"id">> := <<"1">>}}, Map).

%%%===================================================================
%%% Performance Tests
%%%===================================================================

performance_large_file_test_() ->
    {timeout, 60, fun() ->
        %% Generate a large XML (10MB)
        Record = <<"<record id=\"test\"><data>", 
                   (binary:copy(<<"x">>, 1000))/binary, 
                   "</data></record>">>,
        LargeXML = binary:copy(Record, 1000), % ~1MB per copy
        
        StartTime = erlang:monotonic_time(millisecond),
        {ok, _Result} = loki_xml:parse(LargeXML, #{tag => <<"record">>}),
        EndTime = erlang:monotonic_time(millisecond),
        
        Duration = EndTime - StartTime,
        io:format("Parsed large XML in ~p ms~n", [Duration]),
        
        %% Should parse in reasonable time (< 10 seconds)
        ?assert(Duration < 10000)
    end}.

%%%===================================================================
%%% Error Handling Tests
%%%===================================================================

malformed_xml_test() ->
    MalformedXML = <<"<book><title>No closing tag">>,
    Result = loki_xml_parser:parse(MalformedXML),
    ?assertMatch({error, _}, Result).

empty_xml_test() ->
    {ok, Result} = loki_xml_parser:parse(<<>>),
    ?assertEqual([], Result).

unclosed_tag_test() ->
    UnclosedXML = <<"<book><title>Test</title>">>,
    Result = loki_xml_parser:parse(UnclosedXML),
    ?assertMatch({error, _}, Result).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%%% @doc Test parser with only one root element and large whitespace
parse_whitespace_only_test() ->
    XML = <<"  \n\t ">>,
    {ok, Result} = loki_xml_parser:parse(XML),
    ?assertEqual([], Result).   %% Should produce empty result

%%% @doc Test parsing XML with deeply nested elements right at maximal recursion (e.g., depth 50)
parse_max_depth_test() ->
    Depth = 50,
    DeepXml = lists:foldl(
        fun(_, Acc) ->
            <<"<tag>", Acc/binary, "</tag>">>
        end, <<"data">>, lists:seq(1, Depth)),
    {ok, Result} = loki_xml_parser:parse(DeepXml),
    %% Should succeed without stack overflow or crash
    ?assertMatch([{element, <<"tag">>, _, _}], Result).

%%% @doc Test handling of very large attribute values
parse_large_attribute_value_test() ->
    AttrVal = binary:copy(<<"a">>, 50000),  % 50k 'a' characters
    XML = <<"<item desc=\"", AttrVal/binary, "\">Test</item>">>,
    {ok, [{element, <<"item">>, Attrs, _}]} = loki_xml_parser:parse(XML),
    ?assertEqual([{<<"desc">>, AttrVal}], Attrs).

merge_mixed_empty_success_test() ->
    Results = [
        {ok, []},
        {error, parse_error},
        {ok, [{element, <<"foo">>, [], []}]}
    ],
    {ok, Merged} = loki_xml_merger:merge(Results),
    ?assertEqual(1, length(Merged)).

%%% @doc Test query with no match available (returns empty list)
query_no_match_test() ->
    {ok, Parsed} = loki_xml_parser:parse(simple_xml()),
    NoSuch = loki_xml_query:query(Parsed, <<"//doesnotexist">>),
    ?assertEqual([], NoSuch).

%%% @doc Test querying attribute that does not exist
query_missing_attribute_test() ->
    {ok, Parsed} = loki_xml_parser:parse(simple_xml()),
    Values = loki_xml_query:query(Parsed, <<"@notfound">>),
    ?assertEqual([], Values).

%%% @doc Test self-closing tag with attribute only (no children)
parse_self_closing_attr_test() ->
    XML = <<"<empty id=\"foo\"/>">>,
    {ok, [{element, <<"empty">>, Attrs, Children}]} = loki_xml_parser:parse(XML),
    ?assertEqual([{<<"id">>, <<"foo">>}], Attrs),
    ?assertEqual([], Children).

%%% @doc Test robust handling of badly-encoded UTF-8 sequences
parse_bad_utf8_test() ->
    BadBinary = <<"<foo>", 255, 255, "<bar>", 0, "</foo>">>,
    Result = catch loki_xml_parser:parse(BadBinary),
    %% Depending on implementation, this may error out or produce error tuple
    case Result of
        {error, _} -> ok;
        {'EXIT', _} -> ok
    end.

