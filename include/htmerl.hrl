%%%-------------------------------------------------------------------
%%% @author artman41
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Nov 2020 12:47 AM
%%%-------------------------------------------------------------------
-author("artman41").

-record(html_node, {
    tag        = text :: html_tag(),
    attributes = []   :: list(attribute()),
    elements   = []   :: list(html_node() | iolist())
}).

-define(is_html_node(X), is_record(X, html_node)).

-type html_tag() :: text | comment | doctype | unknown | iolist().
-type attribute() :: iolist() | {iolist(), iolist()}.
-type html_node() :: #html_node{}.