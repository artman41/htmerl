%%%-------------------------------------------------------------------
%%% @author artman41
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Nov 2020 12:45 AM
%%%-------------------------------------------------------------------
-module(htmerl).
-author("artman41").

%% API
-export([
    encode/1,
    decode/1
]).

encode(HTMLNodes) ->
    htmerl_encoder:encode(HTMLNodes).

decode(HTML) ->
    htmerl_decoder:decode(HTML).