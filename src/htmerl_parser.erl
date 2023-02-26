-module(htmerl_parser).
-export([parse/1]).

-record(tag, {
    name = [] :: 'TEXT' | iodata(),
    attributes = [] :: list({iodata(), iodata()} | iodata()),
    data = [] :: list(#tag{}),
    state = 'begin_tag_opening' :: undefined | 'begin_tag_opening' | 'end_tag_opening' | 'begin_tag_closing' | 'end_tag_closing'
}).

-define(IS_WHITESPACE(C), C =:= $\s orelse C =:= $\t).

parse(IOList) ->
    parse_(IOList, []).

%% INTERNAL

parse_(IOList, Acc) ->
    case htmerl_ioutils:take_next_char(IOList) of
        false ->
            lists:reverse(Acc);
        {$<, _} ->
            {Tag, Rest} = parse_tag(IOList, #tag{}),
            parse_(Rest, [Tag | Acc]);
        {C, Rest} when ?IS_WHITESPACE(C) ->
            parse_(Rest, Acc);
        _ ->
            lists:reverse([#tag{
                name = 'TEXT',
                attributes = [],
                data = IOList,
                state = undefined
            }|Acc])
    end.

parse_tag(IOList, Acc) ->
    {TagData, Rest1} = htmerl_ioutils:take_until_char(IOList, $>),
    parse_tag_(TagData, Rest1, Acc).

parse_tag_("!doctype html", IOList, Tag) ->
    {Tag#tag{
        name = 'DOCTYPE',
        attributes = [],
        data = "html",
        state = undefined
    }, IOList};
parse_tag_([$< | TagData], IOList0, Tag) ->
    case lists:reverse(TagData) of
        [$/ | TagDataReversed] ->
            [TagName | Attrs] = htmerl_ioutils:split("\s", lists:reverse(TagDataReversed), [global]),
            {Tag#tag{name = TagName, attributes = parse_attributes(Attrs), state = 'end_tag_closing'}, IOList0};
        _ ->
            [TagName | Attrs] = htmerl_ioutils:split("\s", TagData, [global]),
            Acc1 = Tag#tag{name = TagName, attributes = parse_attributes(Attrs), state = 'begin_tag_closing'},
            {Internal, IOList1} = 
                (fun 
                    F(IOData) ->
                        case htmerl_ioutils:starts_with(["/", TagName], IOData) of
                            true ->
                                {"", IOData};
                            false ->
                                {Data, Rest} = htmerl_ioutils:take_until_char(IOData, $<),
                                case F(Rest) of
                                    {"", Tail} ->
                                        {Data, Tail};
                                    {Acc, Tail} ->
                                        {[Data, "<", Acc], Tail}
                                end
                        end
                end)(IOList0),
            io:format("Internal: ~p~n", [Internal]),
            Acc2 = Acc1#tag{data = parse(Internal), state = 'end_tag_opening'},
            case htmerl_ioutils:take_next_char(IOList1) of
                false ->
                    {Acc2, IOList1};
                {$/, IOList2} ->
                    case htmerl_ioutils:take_until_char(IOList2, $>) of
                        {TagName, IOList3} ->
                            {Acc2#tag{state = 'end_tag_closing'}, IOList3};
                        _ ->
                            {Acc2#tag{state = 'end_tag_closing'}, IOList2}
                    end;
                _ ->
                    {Acc2, [$<, IOList1]}
            end
    end.

parse_attributes([]) ->
    [];
parse_attributes(ListOfIOData) ->
    Hunted = hunt_for_quotes(ListOfIOData, [""]),
    Split = [begin [H|T] = htmerl_ioutils:split("=", Elem), [H, T] end || Elem <- Hunted],
    parse_attributes_(Split).

parse_attributes_([]) ->
    [];
parse_attributes_([[Key, Value] | Tail]) ->
    [{Key, Value} | parse_attributes_(Tail)];
parse_attributes_([[Elem] | Tail]) ->
    [Elem | parse_attributes_(Tail)].

hunt_for_quotes([], ["" | Acc]) ->
    lists:reverse(Acc);
hunt_for_quotes([], Acc) ->
    lists:reverse(Acc);
hunt_for_quotes([Head|Tail], [H|T]) ->
    Joined =
        case H =:= "" of
            true ->
                Head;
            false ->
                [H, " ", Head]
        end,
    Acc1 = 
        case hunt_for_quotes_(Joined, []) of
            true -> [Joined | T];
            false -> ["", Joined | T]
        end,
    hunt_for_quotes(Tail, Acc1).

hunt_for_quotes_(IOData, NeedsMatching0) ->
    case htmerl_ioutils:take_next_char(IOData) of
        false ->
            NeedsMatching0 =/= [];
        {C, Rest} when C =:= $" orelse C =:= $' ->
            NeedsMatching1 = 
                case lists:member(C, NeedsMatching0) of
                    true ->
                        NeedsMatching0 -- [C];
                    false ->
                        [C | NeedsMatching0]
                end,
            hunt_for_quotes_(Rest, NeedsMatching1);
        {_, Rest} ->
            hunt_for_quotes_(Rest, NeedsMatching0)
    end.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

take_until_char_test_() ->
    Test = 
        fun(String, Expected) ->
            ?_test(try 
                ?assertEqual(Expected, parse_tag(String, #tag{}))
            catch E:R:S ->
                Text = 
                    case R of
                        {Atom, List} when is_atom(Atom) andalso is_list(List) ->
                            [
                                io_lib:format("~p:~p ==>~n", [E, Atom]), 
                                [["\t", lists:join("=", [
                                    if 
                                        is_atom(Elem) -> 
                                            atom_to_list(Elem); 
                                        is_integer(Elem) -> 
                                            integer_to_list(Elem);
                                        is_binary(Elem) -> 
                                            Elem;
                                        is_list(Elem) -> 
                                            try iolist_to_binary(Elem) 
                                            catch error:badarg -> 
                                                io_lib:format("~p", [Elem]) 
                                            end;
                                        true -> 
                                            io_lib:format("~p", [Elem])
                                    end || Elem <- tuple_to_list(Tuple)
                                ]), "\n"] || Tuple <- List, is_tuple(Tuple)]
                            ];
                        _ ->
                            io_lib:format("~p", [{E, R}])
                    end,
                ?debugFmt("~s~n", [Text]),
                erlang:raise(E, R, S)
            end)
        end,
    [
        Test("<div></div>", {#tag{
            name = "div",
            attributes = [], 
            data = [],
            state = 'end_tag_closing'
        }, ""}),
        Test("<div class=\"my-class\"></div>", {#tag{
            name = "div",   
            attributes = [{"class", ["\"my-class\""]}], 
            data = [],
            state = 'end_tag_closing'
        }, ""}),
        Test("<div enabled></div>", {#tag{
            name = "div",   
            attributes = [{"enabled", ""}], 
            data = [],
            state = 'end_tag_closing'
        }, ""}),
        Test("<div class=\"my-class\" enabled></div>", {#tag{
            name = "div",   
            attributes = [{"class", ["\"my-class\""]}, {"enabled", ""}], 
            data = [],
            state = 'end_tag_closing'
        }, ""}),
        Test("<div enabled class=\"my-class\"></div>", {#tag{
            name = "div",   
            attributes = [{"enabled", ""}, {"class", ["\"my-class\""]}], 
            data = [],
            state = 'end_tag_closing'
        }, ""}),
        Test("<div enabled class=\"my-class 'mine'\" jimbo='bob'></div>", {#tag{
            name = "div",   
            attributes = [{"enabled", ""}, {"class", ["\"my-class 'mine'\""]}, {"jimbo", ["'bob'"]}], 
            data = [],
            state = 'end_tag_closing'
        }, ""}),
        Test("<input/>", {#tag{
            name = "input", 
            attributes = [], 
            data = [],
            state = 'end_tag_closing'
        }, ""}),
        Test("<p>Hello World!</p>", {#tag{
            name = "p",     
            attributes = [], 
            data = [
                #tag{
                    name = 'TEXT',
                    attributes = [],
                    data = "Hello World!",
                    state = undefined
                }
            ],
            state = 'end_tag_closing'
        }, ""}),
        Test("<script> console.log(1 < 2); </script>", {#tag{
            name = "script", 
            attributes = [], 
            data = [
                #tag{
                    name = 'TEXT',
                    attributes = [],
                    data = ["console.log(1 ", "<", " 2); "],
                    state = undefined
                }
            ],
            state = 'end_tag_closing'
        }, ""})
    ].

-endif.