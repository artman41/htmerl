%%%-------------------------------------------------------------------
%%% @author artman41
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Nov 2020 9:47 PM
%%%-------------------------------------------------------------------
-module(end_to_end_SUITE).
-author("artman41").

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("htmerl.hrl").

%% API
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1
]).

%% Tests
-export([
    decode_to_encode/1,
    encode_to_decode/1
]).

all() -> [
    decode_to_encode,
    encode_to_decode
].

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    ok.

decode_to_encode(Config) ->
    HTML = 
        "<!DOCTYPE html>"
        "<html>"
            "<head>"
                "<!--Simple Document-->"
                "<title>Simple</title>"
            "</head>"
            "<body>"
                "<h1>Heading</h1>"
                "<p>Paragraph</p>"
            "</body>"
        "</html>",
    Decoded = htmerl:decode(HTML),
    Encoded = htmerl:encode(Decoded),
    
    ?assertEqual(HTML, lists:flatten(Encoded)).

encode_to_decode(Config) ->
    HTMLNodes = [
        #html_node{tag = doctype, attributes = ["html"], elements = []},
        #html_node{tag = "html", attributes = [], elements = [
            #html_node{tag = "head", attributes = [], elements = [
                #html_node{tag = comment, attributes = [], elements = ["Simple Document"]},
                #html_node{tag = "title", attributes = [], elements = [
                        #html_node{tag = text, attributes = [], elements = ["Simple"]}
                ]}
            ]},
            #html_node{tag = "body", attributes = [], elements = [
                #html_node{tag = "h1", attributes = [], elements = [
                    #html_node{tag = text, attributes = [], elements = ["Heading"]}
                ]},
                #html_node{tag = "p", attributes = [], elements = [
                    #html_node{tag = text, attributes = [], elements = ["Paragraph"]}
                ]}
            ]}
        ]}
    ],
    Encoded = htmerl:encode(HTMLNodes),
    Decoded = htmerl:decode(Encoded),

    ?assertEqual(HTMLNodes, Decoded).