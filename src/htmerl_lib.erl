%%%-------------------------------------------------------------------
%%% @author artman41
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Nov 2020 12:09 AM
%%%-------------------------------------------------------------------
-module(htmerl_lib).
-author("artman41").

-include("htmerl.hrl").

-define(is_charcode1(C), (is_integer(C) andalso C >= 0      andalso C <  16#D800)).
-define(is_charcode2(C), (is_integer(C) andalso C > 16#DFFF andalso C <  16#FFFE)).
-define(is_charcode3(C), (is_integer(C) andalso C > 16#FFFF andalso C =< 16#10FFFF)).
-define(is_charcode(C), ?is_charcode1(C) orelse ?is_charcode2(C) orelse ?is_charcode3(C)).

%% API
-export([
    dig/2,
    extract_html_node/1,
    get_attributes/1,
    is_iolist/1
]).

dig(Binary, Terminator) when is_integer(Terminator) ->
    dig_(Binary, Terminator, []).

extract_html_node(TagData) ->
    lager:debug("Converting ~p to #html_node", [TagData]),
    [Tag | Attributes] = string:split(TagData, " "),
    lager:debug("Tag: ~p, Attributes: ~p", [Tag, Attributes]),
    #html_node{
        tag = Tag,
        attributes = get_attributes(Attributes),
        elements = []
    }.

get_attributes(IOList) ->
    case io_lib:deep_char_list(IOList) of
        true ->
            Attributes = string:split(IOList, " ", all),
            get_attributes_(Attributes);
        false ->
            lager:debug("~p is NOT a printable list", [IOList]),
            erlang:error(not_iodata, [IOList])
    end.

is_iolist(Value) ->
    is_iolist_(Value).

%%%--------------------
%%% Internal Functions
%%%--------------------

get_attributes_([]) ->
    [];
get_attributes_([""|Tail]) ->
    get_attributes_(Tail);
get_attributes_([Attribute|Tail]) ->
    AttributeFlattened = lists:flatten(Attribute),
    Attr =
        case string:split(AttributeFlattened, "=") of
            [Single] -> 
                Single;
            [Key, Value0] ->
                Value1 =
                    case string:split(Value0, [$"], all) of
                        [[], Value, []] -> Value;
                        [Value] -> Value
                    end,
                {Key, Value1}
        end,
    [Attr | get_attributes_(Tail)].

dig_([], _Terminator, Acc)  ->
    {Acc, []};
dig_([Terminator | Tail], Terminator, Acc)->
    {Acc, Tail};
dig_([Char | Tail], Terminator, Acc) ->
    dig_(Tail, Terminator, Acc ++ [Char]).

is_iolist_([]) -> true;
is_iolist_(<<>>) -> true;
is_iolist_([C|Cs]) when ?is_charcode(C) ->
    is_iolist_(Cs);
is_iolist_(<<C, Tail/binary>>) when ?is_charcode(C) ->
    is_iolist_(Tail);
is_iolist_([B|Cs]) when is_binary(B) ->
    is_iolist_(B) andalso is_iolist_(Cs);
is_iolist_([L|Cs]) when is_list(L) ->
    is_iolist_(L) andalso is_iolist_(Cs);
is_iolist_(_) -> false.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

'is_iolist #1 _test'() ->
    IOList = "word",
    ?assert(is_iolist(IOList)).

'is_iolist #2 _test'() ->
    IOList = <<"word">>,
    ?assert(is_iolist(IOList)).

'is_iolist #3 _test'() ->
    IOList = ["word"],
    ?assert(is_iolist(IOList)).

'is_iolist #4 _test'() ->
    IOList = [<<"word">>],
    ?assert(is_iolist(IOList)).

'is_iolist #5 _test'() ->
    IOList = ["word", <<"word">>],
    ?assert(is_iolist(IOList)).

'is_iolist #6 _test'() ->
    IOList = ["word", <<"word">>, ["word", <<"word">>]],
    ?assert(is_iolist(IOList)).

-endif.