%%%-------------------------------------------------------------------
%%% @doc loki_xml_parser - SAX-like XML parser
%%% @end
%%%-------------------------------------------------------------------
-module(loki_xml_parser).

-export([parse/1, start_link/1]).

%%%===================================================================
%%% API Functions
%%%===================================================================

parse(Binary) ->
    try
        case parse_elements(Binary, []) of
            {ok, Elements} -> {ok, Elements};
            {error, Reason} -> {error, Reason}
        end
    catch
        _:Error -> {error, {parse_error, Error}}
    end.

start_link(Chunk) ->
    Pid = spawn_link(fun() ->
        Result = parse(Chunk),
        receive
            {From, get_result} -> From ! {self(), Result}
        end
    end),
    {ok, Pid}.

%%%===================================================================
%%% Parse Elements
%%%===================================================================

parse_elements(<<>>, Acc) ->
    %% Filter out text-only nodes from root level (whitespace)
    Elements = lists:filter(fun
        ({element, _, _, _}) -> true;
        (Binary) when is_binary(Binary) ->
            %% Keep only non-whitespace text
            Trimmed = ws_skip(Binary),
            byte_size(Trimmed) > 0;
        (_) -> true
    end, lists:reverse(Acc)),
    {ok, Elements};
parse_elements(Binary, Acc) ->
    Trimmed = ws_skip(Binary),
    case Trimmed of
        <<>> -> 
            {ok, lists:reverse(Acc)};
        <<"<", Rest/binary>> ->
            case parse_element(Rest) of
                {ok, Element, Remaining} ->
                    parse_elements(Remaining, [Element | Acc]);
                {error, Reason} ->
                    {error, Reason}
            end;
        _ ->
            case text_extract(Trimmed) of
                {Text, Rest} when byte_size(Text) > 0 ->
                    parse_elements(Rest, [Text | Acc]);
                {<<>>, Rest} ->
                    parse_elements(Rest, Acc)
            end
    end.

parse_element(<<"!", "--", Rest/binary>>) ->
    case binary:split(Rest, <<"-->">>) of
        [_Comment, Remaining] -> {ok, {comment, <<>>}, Remaining};
        _ -> {error, unclosed_comment}
    end;
parse_element(<<"?", Rest/binary>>) ->
    case binary:split(Rest, <<"?>">>) of
        [_PI, Remaining] -> {ok, {pi, <<>>}, Remaining};
        _ -> {error, unclosed_pi}
    end;
parse_element(<<"/", _/binary>>) ->
    {error, unexpected_closing_tag};
parse_element(Binary) ->
    case tag_name_extract(Binary) of
        {ok, TagName, AfterName} ->
            case attrs_parse(AfterName) of
                {ok, Attrs, AfterAttrs} ->
                    case AfterAttrs of
                        <<"/>", Rest/binary>> ->
                            {ok, {element, TagName, Attrs, []}, Rest};
                        <<">", Rest/binary>> ->
                            case children_parse(Rest, TagName) of
                                {ok, Children, Remaining} ->
                                    {ok, {element, TagName, Attrs, Children}, Remaining};
                                {error, Reason} ->
                                    {error, Reason}
                            end;
                        _ ->
                            {error, invalid_tag_end}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%%===================================================================
%%% Tag Name
%%%===================================================================

tag_name_extract(Binary) ->
    case until_delim(Binary, <<>>) of
        {<<>>, _} -> {error, empty_tag_name};
        {TagName, Rest} -> {ok, TagName, Rest}
    end.

until_delim(<<>>, Acc) ->
    {Acc, <<>>};
until_delim(<<C, Rest/binary>>, Acc) when C =:= $\s; C =:= $\t; C =:= $\n; C =:= $\r; C =:= $>; C =:= $/ ->
    {Acc, <<C, Rest/binary>>};
until_delim(<<C, Rest/binary>>, Acc) ->
    until_delim(Rest, <<Acc/binary, C>>).

%%%===================================================================
%%% Attributes
%%%===================================================================

attrs_parse(Binary) ->
    attrs_loop(ws_skip(Binary), []).

attrs_loop(<<">", _/binary>> = Binary, Acc) ->
    {ok, lists:reverse(Acc), Binary};
attrs_loop(<<"/>", _/binary>> = Binary, Acc) ->
    {ok, lists:reverse(Acc), Binary};
attrs_loop(<<>>, Acc) ->
    {ok, lists:reverse(Acc), <<>>};
attrs_loop(Binary, Acc) ->
    case attr_single(Binary) of
        {ok, Attr, Rest} ->
            attrs_loop(ws_skip(Rest), [Attr | Acc]);
        {error, Reason} ->
            {error, Reason}
    end.

attr_single(Binary) ->
    case until_char(Binary, $=, <<>>) of
        {<<>>, _} -> 
            {error, invalid_attribute};
        {AttrName, <<"=", Rest/binary>>} ->
            case attr_value(ws_skip(Rest)) of
                {ok, Value, Remaining} ->
                    {ok, {AttrName, Value}, Remaining};
                {error, Reason} ->
                    {error, Reason}
            end;
        _ -> 
            {error, missing_equals}
    end.

until_char(<<>>, _Char, Acc) ->
    {Acc, <<>>};
until_char(<<C, Rest/binary>>, C, Acc) ->
    {Acc, <<C, Rest/binary>>};
until_char(<<C, Rest/binary>>, Char, Acc) when C =/= $\s, C =/= $\t ->
    until_char(Rest, Char, <<Acc/binary, C>>);
until_char(<<_C, Rest/binary>>, Char, Acc) ->
    until_char(Rest, Char, Acc).

attr_value(<<34, Rest/binary>>) ->
    case binary:split(Rest, <<34>>) of
        [Value, Remaining] -> {ok, Value, Remaining};
        _ -> {error, unclosed_quote}
    end;
attr_value(<<39, Rest/binary>>) ->
    case binary:split(Rest, <<39>>) of
        [Value, Remaining] -> {ok, Value, Remaining};
        _ -> {error, unclosed_quote}
    end;
attr_value(_) ->
    {error, missing_quote}.

%%%===================================================================
%%% Children
%%%===================================================================

children_parse(Binary, ParentTag) ->
    children_loop(Binary, ParentTag, []).

children_loop(Binary, ParentTag, Acc) ->
    Trimmed = ws_skip(Binary),
    case Trimmed of
        <<"</", Rest/binary>> ->
            case closing_verify(Rest, ParentTag) of
                {ok, Remaining} ->
                    {ok, lists:reverse(Acc), Remaining};
                {error, Reason} ->
                    {error, Reason}
            end;
        <<"<", RestAfterLt/binary>> ->
            case parse_element(RestAfterLt) of
                {ok, Element, Remaining} ->
                    children_loop(Remaining, ParentTag, [Element | Acc]);
                {error, Reason} ->
                    {error, Reason}
            end;
        <<>> ->
            {error, {unclosed_tag, ParentTag}};
        _ ->
            case text_until_tag(Trimmed) of
                {Text, Rest} when byte_size(Text) > 0 ->
                    children_loop(Rest, ParentTag, [Text | Acc]);
                {<<>>, Rest} ->
                    children_loop(Rest, ParentTag, Acc)
            end
    end.

closing_verify(Binary, ExpectedTag) ->
    case until_char(Binary, $>, <<>>) of
        {TagName, <<">", Rest/binary>>} ->
            case TagName of
                ExpectedTag -> {ok, Rest};
                _ -> {error, {tag_mismatch, ExpectedTag, TagName}}
            end;
        _ -> 
            {error, invalid_closing_tag}
    end.

%%%===================================================================
%%% Text Extraction
%%%===================================================================

text_extract(Binary) ->
    text_until_tag(Binary).

text_until_tag(Binary) ->
    case binary:split(Binary, <<60>>) of
        [Text, Rest] -> {Text, <<60, Rest/binary>>};
        [Text] -> {Text, <<>>}
    end.

%%%===================================================================
%%% Whitespace
%%%===================================================================

ws_skip(<<C, Rest/binary>>) when C =:= $\s; C =:= $\t; C =:= $\n; C =:= $\r ->
    ws_skip(Rest);
ws_skip(Binary) ->
    Binary.
