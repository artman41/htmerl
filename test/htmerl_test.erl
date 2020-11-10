%%%-------------------------------------------------------------------
%%% @author artman41
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Nov 2020 6:54 PM
%%%-------------------------------------------------------------------
-module(htmerl_test).
-author("artman41").

-include_lib("common_test/include/ct.hrl").

%% API
-export([
    get_html_path/2
]).

-spec get_html_path(Config :: list(), HTMLName :: file:name_all()) -> {ok, file:filename_all()} | {error, no_file}.
get_html_path(Config, HTMLName) ->
    FilePath = filename:join(?config(data_dir, Config), HTMLName),
    case file:read_file_info(FilePath) of
        {error, enoent} -> {error, no_file};
        {ok, _FileInfo} -> {ok, FilePath} 
    end.
