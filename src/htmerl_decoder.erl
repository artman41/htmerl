%%%-------------------------------------------------------------------
%%% @author artman41
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% Decodes HTML into HTML Nodes
%%% @end
%%% Created : 01. Nov 2020 12:51 AM
%%%-------------------------------------------------------------------
-module(htmerl_decoder).
-author("artman41").

-include("htmerl.hrl").

-define(is_whitespace(C), C =:= $\n orelse C =:= $\s orelse C =:= $\t).

%% API
-export([
    decode/1,
    decode_file/1
]).

-spec decode(HTML :: iolist()) -> list(html_node()).
decode(HTML) ->
    FlattenedHTML = flatten_data(HTML),
    do_decode(FlattenedHTML).

-spec decode_file(FileLoc :: file:name_all()) -> list(html_node()).
decode_file(FileLoc) ->
    {ok, HTML} = file:read_file(FileLoc),
    decode(HTML).

%%%--------------------
%%% Internal Functions
%%%--------------------

-spec flatten_data(Data :: iolist()) -> list().
flatten_data(Data) when is_list(Data) orelse is_binary(Data) ->
    %% Because we're accepting IOData, we might 
    %%  as well just do an iolist->binary->list
    %%  to get a flat list rather than trying to
    %%  do a lists:flatten/1 for a list and having
    %%  cases for any potential other datatype
    Binary = iolist_to_binary(Data),
    binary_to_list(Binary).

do_decode([]) ->
    [];
do_decode("<!--" ++ Tail) ->
    [InsideComment, OutsideComment] = string:split(Tail, "-->"),
    [#html_node{
        tag        = comment,
        attributes = [],
        elements   = [InsideComment]
    } | do_decode(OutsideComment)];
do_decode("<!" ++ Tail) ->
    {Doctype, Rest} = lists:split(7, Tail),
    {Tag, IOList} = 
        case string:lowercase(Doctype) of
            "doctype" ->
                {doctype, Rest};
            _ ->
                {unknown, Tail}
        end,
    [Inside, Outside] = string:split(IOList, ">"),
    [#html_node{
        tag        = Tag,
        attributes = htmerl_lib:get_attributes(Inside),
        elements   = []
    } | do_decode(Outside)];
do_decode([$< | Tail]) ->
    {TagData, After} = htmerl_lib:dig(Tail, $>),
    HTMLNode0 = htmerl_lib:extract_html_node(TagData),
    {HTMLNode1, Rest} = 
        case string:split(After, "</" ++ HTMLNode0#html_node.tag ++ ">") of
            [InsideTags, T_] ->
                {HTMLNode0#html_node{elements = do_decode(InsideTags)}, T_};
            [""] ->
                {HTMLNode0, ""};
            [After] ->
                {HTMLNode0#html_node{elements = []}, After}
        end,
    [HTMLNode1 | do_decode(Rest)];
do_decode([C | Tail]) when ?is_whitespace(C) ->
    do_decode(Tail);
do_decode(Data) ->
    [#html_node{
        tag        = text,
        attributes = [],
        elements   = [Data]
    }].

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

'<p> _test'() ->
    HTML = "<p>Some Text</p>",
    Expected = [#html_node{
        tag = "p",
        attributes = [],
        elements = [
            #html_node{tag = text, elements = ["Some Text"]}
        ]
    }],
    Actual = decode(HTML),
    ?assertEqual(Expected, Actual).

'comment _test'() ->
    HTML = "<!--This is a comment-->",
    Expected = [#html_node{
        tag = comment,
        attributes = [],
        elements = ["This is a comment"]
    }],
    Actual = decode(HTML),
    ?assertEqual(Expected, Actual).

'<div> _test'() ->
    HTML = "<div></div>",
    Expected = [#html_node{
        tag = "div",
        attributes = [],
        elements = []
    }],
    Actual = decode(HTML),
    ?assertEqual(Expected, Actual).

'<div> with <p> child _test'() ->
    HTML = "<div><p>Hey There</p></div>",
    Expected = [#html_node{
        tag = "div",
        attributes = [],
        elements = [
            #html_node{
                tag = "p",
                attributes = [],
                elements = [
                    #html_node{tag = text, elements = ["Hey There"]}
                ]
            }
        ]
    }],
    Actual = decode(HTML),
    ?assertEqual(Expected, Actual).

'<div> with 2 <p> child _test'() ->
    HTML = "<div><p>Hey There</p><p>Boo</p></div>",
    Expected = [#html_node{
        tag = "div",
        attributes = [],
        elements = [
            #html_node{
                tag = "p",
                attributes = [],
                elements = [
                    #html_node{tag = text, elements = ["Hey There"]}
                ]
            },
            #html_node{
                tag = "p",
                attributes = [],
                elements = [
                    #html_node{tag = text, elements = ["Boo"]}
                ]
            }
        ]
    }],
    Actual = decode(HTML),
    ?assertEqual(Expected, Actual).

'<div> with Single Attr _test'() ->
    HTML = "<div disabled></div>",
    Expected = [#html_node{
        tag = "div",
        attributes = [
            "disabled"
        ],
        elements = []
    }],
    Actual = decode(HTML),
    ?assertEqual(Expected, Actual).

'<div> with KV Attr _test'() ->
    HTML = "<div class=\"wow\"></div>",
    Expected = [#html_node{
        tag = "div",
        attributes = [
            {"class", "wow"}
        ],
        elements = []
    }],
    Actual = decode(HTML),
    ?assertEqual(Expected, Actual).

'<div> with Mixed Attr _test'() ->
    HTML = "<div class=\"wow\" disabled></div>",
    Expected = [#html_node{
        tag = "div",
        attributes = [
            {"class", "wow"},
            "disabled"
        ],
        elements = []
    }],
    Actual = decode(HTML),
    ?assertEqual(Expected, Actual).

-endif.