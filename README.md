# loki_xml

**Effortless XML parsing for Erlang, simple, fast, and neat.**

A high-performance XML parser designed for processing large XML files using chunked, parallel, and distributed processing with built-in fault tolerance.

[![Hex.pm](https://img.shields.io/hexpm/v/loki_xml.svg)](https://hex.pm/packages/loki_xml)
[![Hex Docs](https://img.shields.io/badge/hex-docs-blue.svg)](https://hexdocs.pm/loki_xml)
[![License](https://img.shields.io/badge/license-Apache%202.0-blue.svg)](LICENSE)

## Features

- **High Performance**: Streaming SAX-like parsing without loading entire files into memory
- **Chunked Processing**: Intelligent XML splitting using regex or fixed-size chunks
- **Parallel Parsing**: Process multiple chunks concurrently
- **Distributed**: Scale across multiple Erlang nodes
- **Fault Tolerant**: OTP supervision with automatic recovery
- **XPath Queries**: Basic XPath support (`//`, `/`, `@attribute`)
- **Flexible Output**: Convert to Erlang terms or maps

## Architecture

```
Master Node
│
├── File Supervisor (1 per XML file)
│   ├── Splitter Process (splits XML into chunks)
│   ├── Chunk Supervisor (1 per chunk)
│   │   ├── Parser Worker (parses chunk)
│   │   └── XPath Worker (optional query execution)
│   └── Merger Process (combines results)
│
└── Distributor (optional, sends chunks to remote nodes)
```

## Quick Start

```erlang
%% Start the application
loki_xml:start().

%% Parse a simple XML binary
XML = <<"<book id=\"1\">
          <title>Erlang Programming</title>
          <author>Joe Armstrong</author>
        </book>">>,

{ok, Parsed} = loki_xml:parse(XML).
%% Returns: [{element, <<"book">>, [{<<"id">>, <<"1">>}], [...]}]

%% Parse a file
{ok, Parsed} = loki_xml:parse("large.xml").

%% Parse with options
{ok, Parsed} = loki_xml:parse("huge.xml", #{
    tag => <<"record">>,           % Split by <record> tags
    chunk_size => 1048576,          % 1MB chunks (fallback)
    format => map,                  % Convert to maps
    max_retries => 3                % Retry failed chunks
}).
```

## XPath Queries

```erlang
{ok, Parsed} = loki_xml:parse("library.xml"),

%% Query all titles
Titles = loki_xml:query(Parsed, <<"//title">>).
%% Returns: [<<"Erlang Programming">>, <<"Learn You Some Erlang">>]

%% Query attributes
Ids = loki_xml:query(Parsed, <<"@id">>).
%% Returns: [<<"1">>, <<"2">>]

%% Nested queries
Authors = loki_xml:query(Parsed, <<"//book/author">>).
```

## Distributed Parsing

Process large XML files across multiple Erlang nodes:

```erlang
%% Start additional nodes
%% Terminal 1: erl -sname node1@localhost
%% Terminal 2: erl -sname node2@localhost

%% On master node
Nodes = ['node1@localhost', 'node2@localhost'],

{ok, Parsed} = loki_xml:parse("huge.xml", #{
    distributed => true,
    nodes => Nodes,
    tag => <<"record">>
}).
```

## Map Conversion

```erlang
%% Parse directly to maps
{ok, [Book]} = loki_xml:parse(XML, #{format => map}).

%% Access fields
#{tag := <<"book">>,
  attributes := #{<<"id">> := <<"1">>},
  children := [
      #{tag := <<"title">>, children := [<<"Erlang Programming">>]},
      ...
  ]} = Book.

%% Or convert manually
{ok, [Element]} = loki_xml:parse(XML),
Map = loki_xml:to_map(Element).
```

## API Reference

### Main API (`loki_xml`)

#### `parse/1`
```erlang
-spec parse(binary() | file:filename()) -> {ok, [xml_element()]} | {error, term()}.
```
Parse XML from binary or file with default options.

#### `parse/2`
```erlang
-spec parse(binary() | file:filename(), parse_opts()) -> {ok, [xml_element()]} | {error, term()}.
```

Options:
- `format`: `term` (default) or `map`
- `distributed`: `boolean()` - Enable distributed parsing
- `nodes`: `[node()]` - List of nodes for distribution
- `chunk_size`: `pos_integer()` - Chunk size in bytes (default: 1MB)
- `tag`: `binary()` - Tag name for regex-based splitting
- `max_retries`: `non_neg_integer()` - Max retry attempts (default: 3)

#### `query/2`
```erlang
-spec query([xml_element()], binary()) -> [binary() | xml_element()].
```
Execute XPath query on parsed XML.

#### `to_map/1`
```erlang
-spec to_map(xml_element()) -> map().
```
Convert XML element to Erlang map.

### Parser Module (`loki_xml_parser`)

#### `parse/1`
```erlang
-spec parse(binary()) -> {ok, [xml_element()]} | {error, term()}.
```
Low-level parser for XML chunks.

### Splitter Module (`loki_xml_splitter`)

#### `split_by_tag/2`
```erlang
-spec split_by_tag(binary(), binary()) -> {ok, [binary()]} | {error, term()}.
```
Split XML by specific tag using regex.

#### `split_fixed/2`
```erlang
-spec split_fixed(binary(), pos_integer()) -> {ok, [binary()]}.
```
Split XML into fixed-size chunks with boundary reconstruction.

#### `validate_chunk/1`
```erlang
-spec validate_chunk(binary()) -> boolean().
```
Validate that a chunk is well-formed XML.

### Query Module (`loki_xml_query`)

Supported XPath expressions:
- `//tag` - Descendant-or-self axis (all descendants)
- `/tag` - Child axis from root
- `//tag/subtag` - Nested paths
- `@attribute` - Attribute values

## Error Handling

The parser is designed for fault tolerance:

```erlang
%% Partial success - some chunks fail
{ok, Parsed} = loki_xml:parse("file.xml", #{max_retries => 3}).
%% Returns successfully parsed chunks, logs errors

%% Complete failure
{error, {all_chunks_failed, Reasons}} = loki_xml:parse("bad.xml").

%% Individual chunk errors
{error, {parse_error, Reason}} = loki_xml_parser:parse(<<"<unclosed">>).
```

Error types:
- `{parse_error, Reason}` - Malformed XML
- `{unclosed_tag, Tag}` - Missing closing tag
- `{tag_mismatch, Expected, Actual}` - Mismatched tags
- `{timeout, Pid}` - Chunk parsing timeout
- `{max_retries, Chunk}` - Exceeded retry limit

**Optimization tips:**
- Use `tag` option for structured XML (faster than fixed-size)
- Enable distribution for files > 100MB
- Adjust `chunk_size` based on record size
- Use streaming for very large files

## Testing

```bash
# Run all tests
rebar3 eunit

# Run specific test module
rebar3 eunit --module=loki_xml_tests

# Run examples
erl -pa _build/default/lib/*/ebin
1> loki_xml_examples:run_all_examples().
```

## Design Decisions

### Why Regex-Based Splitting?

Regex splitting allows:
- Fast chunk identification without full parsing
- Preservation of logical record boundaries
- Better parallelization for structured XML

Fallback to fixed-size ensures robustness for unstructured XML.

### Why SAX-Like Parsing?

- Memory efficient for large files
- Stream processing without loading entire document
- Better performance for selective queries

### Why OTP Supervision?

- Automatic recovery from parser crashes
- Isolated failures (one bad chunk doesn't fail entire parse)
- Production-ready fault tolerance

## Limitations

- **XPath**: Basic support only (`//`, `/`, `@`) - no predicates or functions
- **Namespaces**: Not currently supported
- **DTD/Schema**: No validation support
- **Entities**: Limited entity reference support

## Contributing

Contributions welcome! Please:

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Ensure all tests pass: `rebar3 eunit`
5. Submit a pull request

## License

Apache License 2.0

## Acknowledgments

- Inspired by Erlang's robust concurrency model
- Built with OTP supervision principles
- Thanks to the Erlang community

## Support

- Issues: https://github.com/roquess/loki_xml/issues
- Documentation: https://hexdocs.pm/loki_xml
- Discussions: https://github.com/roquess/loki_xml/discussions

## Related Projects

- `xmerl` - Erlang's standard XML parser
- `erlsom` - SAX-based XML parser for Erlang
- `fast_xml` - Fast XML parser (C NIF-based)

---

**Star ⭐ this project if you find it useful!**
