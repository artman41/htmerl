%%%-------------------------------------------------------------------
%%% @author artman41
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Nov 2020 6:30 PM
%%%-------------------------------------------------------------------
-module(decode_document_SUITE).
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
    simple_html_document/1
]).

all() -> [
    simple_html_document
].

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    ok.

simple_html_document(Config) ->
    {ok, SimpleHTMLPath} = htmerl_test:get_html_path(Config, "simple.html"),
    
    Expected = [
        #html_node{
            tag = doctype,
            attributes = ["html"],
            elements = []
        },
        #html_node{
            tag = "html",
            attributes = [],
            elements = [
                #html_node{
                    tag = "head",
                    attributes = [],
                    elements = [
                        #html_node{
                            tag = comment,
                            attributes = [],
                            elements = ["Simple Document"]
                        },
                        #html_node{
                            tag = "title",
                            attributes = [],
                            elements = [
                                #html_node{tag = text, attributes = [], elements = ["Simple"]}
                            ]
                        }
                    ]
                },
                #html_node{
                    tag = "body",
                    attributes = [],
                    elements = [
                        #html_node{
                            tag = "h1",
                            attributes = [],
                            elements = [
                                #html_node{tag = text, attributes = [], elements = ["Heading"]}
                            ]
                        },
                        #html_node{
                            tag = "p",
                            attributes = [],
                            elements = [
                                #html_node{tag = text, attributes = [], elements = ["Paragraph"]}
                            ]
                        }
                    ]
                }
            ]
        }
    ],
    
    ?assertEqual(Expected, htmerl_decoder:decode_file(SimpleHTMLPath)).