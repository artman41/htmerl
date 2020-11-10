%%%-------------------------------------------------------------------
%%% @author artman41
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% Encodes HTML Nodes into HTML
%%% @end
%%% Created : 01. Nov 2020 12:50 AM
%%%-------------------------------------------------------------------
-module(htmerl_encoder).
-author("artman41").

-include("htmerl.hrl").

%% API
-export([encode/1]).

encode(HTMLNode) when ?is_html_node(HTMLNode) ->
    build_html([HTMLNode]);
encode(HTMLNodes) ->
    build_html(HTMLNodes).

%%%--------------------
%%% Internal Functions
%%%--------------------

build_html([]) ->
    [];
build_html([HTMLNode | Tail]) ->
    [build_html_(HTMLNode) | build_html(Tail)].
    
build_html_(HTMLNode = #html_node{tag = Tag, elements = Elements}) ->
    {TagStart, TagMid, TagEnd} =
        case Tag of
            text ->
                {"", "", ""};
            comment ->
                {"<!--", "", "-->"};
            doctype ->
                {"<!DOCTYPE", "", ">"};
            unknown ->
                {["<!", Tag], "", ">"};
            _ when Elements =:= [] -> 
                {["<", Tag], "", ["/>"]};
            _ -> 
                {["<", Tag], ">", ["</", Tag, ">"]} 
        end,
    Middle =
        build_attributes(HTMLNode#html_node.attributes),
    End =
        if
            Elements =:= [] -> TagEnd;
            true -> [TagMid, build_html(Elements), TagEnd]
        end,
    [TagStart, Middle, End];
build_html_(Value) ->
    case htmerl_lib:is_iolist(Value) of
        true -> 
            Value;
        false ->
            erlang:error(badarg, [Value])
    end.

build_attributes([]) ->
    [];
build_attributes([X = {Key, Value} | Tail]) ->
    [" ", Key, $=, $", Value, $" | build_attributes(Tail)];
build_attributes([Value | Tail]) ->
    [" ", Value | build_attributes(Tail)].


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

'<p> _test'() ->
    Expected = "<p>Some Text</p>",
    HTMLNodes = [#html_node{
        tag = "p",
        attributes = [],
        elements = [
            #html_node{tag = text, elements = ["Some Text"]}
        ]
    }],
    Encoded = encode(HTMLNodes),
    Actual = lists:flatten(Encoded),?assertEqual(Expected, Actual).

'comment _test'() ->
    Expected = "<!--This is a comment-->",
    HTMLNodes = [#html_node{
        tag = comment,
        attributes = [],
        elements = ["This is a comment"]
    }],
    Encoded = encode(HTMLNodes),
    Actual = lists:flatten(Encoded),
    ?assertEqual(Expected, Actual).

'<div> _test'() ->
    Expected = "<div/>",
    HTMLNodes = [#html_node{
        tag = "div",
        attributes = [],
        elements = []
    }],
    Encoded = encode(HTMLNodes),
    Actual = lists:flatten(Encoded),
    ?assertEqual(Expected, Actual).

'<div> with <p> child _test'() ->
    Expected = "<div><p>Hey There</p></div>",
    HTMLNodes = [#html_node{
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
    Encoded = encode(HTMLNodes),
    Actual = lists:flatten(Encoded),
    ?assertEqual(Expected, Actual).

'<div> with 2 <p> child _test'() ->
    Expected = "<div><p>Hey There</p><p>Boo</p></div>",
    HTMLNodes = [#html_node{
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
    Encoded = encode(HTMLNodes),
    Actual = lists:flatten(Encoded),
    ?assertEqual(Expected, Actual).

'<div> with Single Attr _test'() ->
    Expected = "<div disabled/>",
    HTMLNodes = [#html_node{
        tag = "div",
        attributes = [
            "disabled"
        ],
        elements = []
    }],
    Encoded = encode(HTMLNodes),
    Actual = lists:flatten(Encoded),
    ?assertEqual(Expected, Actual).

'<div> with KV Attr _test'() ->
    Expected = "<div class=\"wow\"/>",
    HTMLNodes = [#html_node{
        tag = "div",
        attributes = [
            {"class", "wow"}
        ],
        elements = []
    }],
    Encoded = encode(HTMLNodes),
    Actual = lists:flatten(Encoded),
    ?assertEqual(Expected, Actual).

'<div> with Mixed Attr _test'() ->
    Expected = "<div class=\"wow\" disabled/>",
    HTMLNodes = [#html_node{
        tag = "div",
        attributes = [
            {"class", "wow"},
            "disabled"
        ],
        elements = []
    }],
    Encoded = encode(HTMLNodes),
    Actual = lists:flatten(Encoded),
    ?assertEqual(Expected, Actual).

-endif.
