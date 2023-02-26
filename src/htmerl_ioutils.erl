-module(htmerl_ioutils).
-export([
    take_next_char/1,
    take_n_chars/2,
    take_until_char/2,
    split/2, split/3,
    to_lower/1, to_upper/1,
    starts_with/2
]).

-compile({inline, [
    take_n_chars_/3, 
    take_until_char_/3
]}).

-spec take_next_char(iolist()) -> {Char, Rest} | false
    when Char :: non_neg_integer(),
         Rest :: iolist().
take_next_char([]) ->
    false;
take_next_char(<<>>) ->
    false;
take_next_char([H|T]) when is_list(H) orelse is_binary(H) ->
    case take_next_char(H) of
        false ->
            take_next_char(T);
        {C, Rest} ->
            {C, [Rest | T]}
    end;
take_next_char([C|T]) when is_integer(C)->
    {C, T};
take_next_char(<<C:8, Rest/binary>>) when is_integer(C)->
    {C, Rest}.

-spec take_n_chars(iodata(), non_neg_integer()) -> {{list(byte()), non_neg_integer()}, iodata()}.
take_n_chars(IOList, N) ->
    take_n_chars_(IOList, N, {[], 0}).

-spec take_until_char(iodata(), non_neg_integer()) -> {iodata(), iodata()}.
take_until_char(IOList, Char) ->
    take_until_char_(IOList, Char, []).

split(Chars, Subject) ->
    split(Chars, Subject, []).

split(_Chars, "", _Opts) ->
    [];
split(Chars, Subject, Opts) ->
    {Elem, Rest} = split_(Chars, Subject, Opts, ""),
    case lists:member(global, Opts) of
        true ->
            [Elem|split(Chars, Rest, Opts)];
        false ->
            [Elem, Rest]
    end.

to_lower(IOData) ->
    case take_next_char(IOData) of 
        false -> []; 
        {$A, Rest} -> [$a|to_lower(Rest)];
        {$B, Rest} -> [$b|to_lower(Rest)];
        {$C, Rest} -> [$c|to_lower(Rest)];
        {$D, Rest} -> [$d|to_lower(Rest)];
        {$E, Rest} -> [$e|to_lower(Rest)];
        {$F, Rest} -> [$f|to_lower(Rest)];
        {$G, Rest} -> [$g|to_lower(Rest)];
        {$H, Rest} -> [$h|to_lower(Rest)];
        {$I, Rest} -> [$i|to_lower(Rest)];
        {$J, Rest} -> [$j|to_lower(Rest)];
        {$K, Rest} -> [$k|to_lower(Rest)];
        {$L, Rest} -> [$l|to_lower(Rest)];
        {$M, Rest} -> [$m|to_lower(Rest)];
        {$N, Rest} -> [$n|to_lower(Rest)];
        {$O, Rest} -> [$o|to_lower(Rest)];
        {$P, Rest} -> [$p|to_lower(Rest)];
        {$Q, Rest} -> [$q|to_lower(Rest)];
        {$R, Rest} -> [$r|to_lower(Rest)];
        {$S, Rest} -> [$s|to_lower(Rest)];
        {$T, Rest} -> [$t|to_lower(Rest)];
        {$U, Rest} -> [$u|to_lower(Rest)];
        {$V, Rest} -> [$v|to_lower(Rest)];
        {$W, Rest} -> [$w|to_lower(Rest)];
        {$X, Rest} -> [$x|to_lower(Rest)];
        {$Y, Rest} -> [$y|to_lower(Rest)];
        {$Z, Rest} -> [$z|to_lower(Rest)];
        {$a, Rest} -> [$a|to_lower(Rest)];
        {$b, Rest} -> [$b|to_lower(Rest)];
        {$c, Rest} -> [$c|to_lower(Rest)];
        {$d, Rest} -> [$d|to_lower(Rest)];
        {$e, Rest} -> [$e|to_lower(Rest)];
        {$f, Rest} -> [$f|to_lower(Rest)];
        {$g, Rest} -> [$g|to_lower(Rest)];
        {$h, Rest} -> [$h|to_lower(Rest)];
        {$i, Rest} -> [$i|to_lower(Rest)];
        {$j, Rest} -> [$j|to_lower(Rest)];
        {$k, Rest} -> [$k|to_lower(Rest)];
        {$l, Rest} -> [$l|to_lower(Rest)];
        {$m, Rest} -> [$m|to_lower(Rest)];
        {$n, Rest} -> [$n|to_lower(Rest)];
        {$o, Rest} -> [$o|to_lower(Rest)];
        {$p, Rest} -> [$p|to_lower(Rest)];
        {$q, Rest} -> [$q|to_lower(Rest)];
        {$r, Rest} -> [$r|to_lower(Rest)];
        {$s, Rest} -> [$s|to_lower(Rest)];
        {$t, Rest} -> [$t|to_lower(Rest)];
        {$u, Rest} -> [$u|to_lower(Rest)];
        {$v, Rest} -> [$v|to_lower(Rest)];
        {$w, Rest} -> [$w|to_lower(Rest)];
        {$x, Rest} -> [$x|to_lower(Rest)];
        {$y, Rest} -> [$y|to_lower(Rest)];
        {$z, Rest} -> [$z|to_lower(Rest)];
        {C,  Rest} -> [C |to_lower(Rest)]
    end.

to_upper(IOData) ->
    case take_next_char(IOData) of 
        false -> []; 
        {$a, Rest} -> [$A|to_upper(Rest)];
        {$b, Rest} -> [$B|to_upper(Rest)];
        {$c, Rest} -> [$C|to_upper(Rest)];
        {$d, Rest} -> [$D|to_upper(Rest)];
        {$e, Rest} -> [$E|to_upper(Rest)];
        {$f, Rest} -> [$F|to_upper(Rest)];
        {$g, Rest} -> [$G|to_upper(Rest)];
        {$h, Rest} -> [$H|to_upper(Rest)];
        {$i, Rest} -> [$I|to_upper(Rest)];
        {$j, Rest} -> [$J|to_upper(Rest)];
        {$k, Rest} -> [$K|to_upper(Rest)];
        {$l, Rest} -> [$L|to_upper(Rest)];
        {$m, Rest} -> [$M|to_upper(Rest)];
        {$n, Rest} -> [$N|to_upper(Rest)];
        {$o, Rest} -> [$O|to_upper(Rest)];
        {$p, Rest} -> [$P|to_upper(Rest)];
        {$q, Rest} -> [$Q|to_upper(Rest)];
        {$r, Rest} -> [$R|to_upper(Rest)];
        {$s, Rest} -> [$S|to_upper(Rest)];
        {$t, Rest} -> [$T|to_upper(Rest)];
        {$u, Rest} -> [$U|to_upper(Rest)];
        {$v, Rest} -> [$V|to_upper(Rest)];
        {$w, Rest} -> [$W|to_upper(Rest)];
        {$x, Rest} -> [$X|to_upper(Rest)];
        {$y, Rest} -> [$Y|to_upper(Rest)];
        {$z, Rest} -> [$Z|to_upper(Rest)];
        {$A, Rest} -> [$A|to_upper(Rest)];
        {$B, Rest} -> [$B|to_upper(Rest)];
        {$C, Rest} -> [$C|to_upper(Rest)];
        {$D, Rest} -> [$D|to_upper(Rest)];
        {$E, Rest} -> [$E|to_upper(Rest)];
        {$F, Rest} -> [$F|to_upper(Rest)];
        {$G, Rest} -> [$G|to_upper(Rest)];
        {$H, Rest} -> [$H|to_upper(Rest)];
        {$I, Rest} -> [$I|to_upper(Rest)];
        {$J, Rest} -> [$J|to_upper(Rest)];
        {$K, Rest} -> [$K|to_upper(Rest)];
        {$L, Rest} -> [$L|to_upper(Rest)];
        {$M, Rest} -> [$M|to_upper(Rest)];
        {$N, Rest} -> [$N|to_upper(Rest)];
        {$O, Rest} -> [$O|to_upper(Rest)];
        {$P, Rest} -> [$P|to_upper(Rest)];
        {$Q, Rest} -> [$Q|to_upper(Rest)];
        {$R, Rest} -> [$R|to_upper(Rest)];
        {$S, Rest} -> [$S|to_upper(Rest)];
        {$T, Rest} -> [$T|to_upper(Rest)];
        {$U, Rest} -> [$U|to_upper(Rest)];
        {$V, Rest} -> [$V|to_upper(Rest)];
        {$W, Rest} -> [$W|to_upper(Rest)];
        {$X, Rest} -> [$X|to_upper(Rest)];
        {$Y, Rest} -> [$Y|to_upper(Rest)];
        {$Z, Rest} -> [$Z|to_upper(Rest)];
        {C,  Rest} -> [C |to_upper(Rest)]
    end.

starts_with(StartsWith, IOData) ->
    case {take_next_char(StartsWith), take_next_char(IOData)} of
        {false, _} ->
            true;
        {{Char, SWRest}, {Char, IORest}} ->
            starts_with(SWRest, IORest);
        _ ->
            false
    end.

%% Internal

take_n_chars_(IOList, 0, {Chars, Count}) ->
    {{lists:reverse(Chars), Count}, IOList};
take_n_chars_(IOList, N, Acc = {Chars, Count}) when is_integer(N) andalso N > 0 ->
    case take_next_char(IOList) of
        false ->
            take_n_chars_(IOList, 0, Acc);
        {C, Rest} ->
            Acc1 = {[C|Chars], Count+1},
            take_n_chars_(Rest, N-1, Acc1)
    end.

take_until_char_(IOList, Char, Acc) ->
    case take_next_char(IOList) of
        false ->
            {lists:reverse(Acc), ""};
        {Char, Rest} ->
            {lists:reverse(Acc), Rest};
        {C, Rest} ->
            take_until_char_(Rest, Char, [C | Acc])
    end.

split_(Chars, IOData, Opts, Acc0) ->
    case take_next_char(IOData) of
        false ->
            {lists:reverse(Acc0), ""};
        {C, Rest} ->
            case lists:member(C, Chars) of
                true ->
                    {lists:reverse(Acc0), Rest};
                false ->
                    split_(Chars, Rest, Opts, [C|Acc0])
            end
    end.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(_test_(Expr), ?_test(begin ?debugFmt("==> Running Test ~p:~p <~b>", [?MODULE, ?FUNCTION_NAME, ?LINE]), Expr end)).

take_next_char_test_() ->
    [
        ?_test_(?assertEqual({$a, "bc"}, take_next_char("abc"))),
        ?_test_(?assertEqual({$a, <<"bc">>}, take_next_char(<<"abc">>))),
        ?_test_(?assertEqual({$a, ["b", <<"c">>]}, take_next_char(["ab", <<"c">>]))),
        ?_test_(?assertEqual({$a, [$b | "c"]}, take_next_char([$a | [ $b | "c"]]))),
        ?_test_(?assertEqual({$a, ["b", <<"c">>]}, take_next_char([$a, "b", <<"c">>])))
    ].

take_n_chars_test_() ->
    SetupFun = 
        fun() -> 
            [
                {iodata, generate_ascii_alphas(50)}
            ]
        end,
    InstantiatorFun =
        fun(Config) ->
            {iodata, IOData} = lists:keyfind(iodata, 1, Config),
            [
                'take_n_chars - N less'(IOData),
                'take_n_chars - N equal'(IOData),
                'take_n_chars - N more'(IOData)
            ]
        end,
    eunit_gen_utils:n_generator(3, SetupFun, InstantiatorFun).

take_until_char_test_() ->
    [
        ?_test_(?assertEqual({"abc", "def.ghi"}, take_until_char("abc.def.ghi", $.))),
        ?_test_(?assertEqual({"abc", <<"def.ghi">>}, take_until_char(<<"abc.def.ghi">>, $.))),
        ?_test_(?assertEqual({"abc", [<<"def.ghi">>]}, take_until_char(["abc", <<".def.ghi">>], $.))),
        ?_test_(?assertEqual({"abc", "def.ghi"}, take_until_char([$a, $b, $c | [$., $d, $e, $f | ".ghi"]], $.))),
        ?_test_(?assertEqual({"abc", ["def", <<".ghi">>]}, take_until_char([$a, $b, $c, ".def", <<".ghi">>], $.)))
    ].

split_test_() ->
    [
        ?_test_(?assertEqual(["a", "b c"], split(" ", "a b c"))),
        ?_test_(?assertEqual(["a", <<"b c">>], split(" ", <<"a b c">>))),
        ?_test_(?assertEqual(["a", ["b", <<" c">>]], split(" ", ["a b", <<" c">>]))),
        ?_test_(?assertEqual(["a", [$b | " c"]], split(" ", [$a, $\s | [ $b | " c"]]))),
        ?_test_(?assertEqual(["a", ["b", <<" c">>]], split(" ", [$a, " b", <<" c">>]))),

        ?_test_(?assertEqual(["a", "b", "c"], split(" ", "a b c", [global]))),
        ?_test_(?assertEqual(["a", "b", "c"], split(" ", <<"a b c">>, [global]))),
        ?_test_(?assertEqual(["a", "b", "c"], split(" ", ["a b", <<" c">>], [global]))),
        ?_test_(?assertEqual(["a", "b", "c"], split(" ", [$a, $\s | [ $b | " c"]], [global]))),
        ?_test_(?assertEqual(["a", "b", "c"], split(" ", [$a, " b", <<" c">>], [global])))
    ].

bit_shift_left_test_() ->
    [
        ?_test_(?assertEqual(100, bit_shift_left([100]))),
        ?_test_(?assertEqual((100 bsl 16) bor (100 bsl 8) bor 100, bit_shift_left([100, 100, 100]))),
        ?_test_(?assertException(error, function_clause, bit_shift_left([])))
    ].

'take_n_chars - N less'(IOData) ->
    ?_test_(begin
        ?assertMatch(N when N > 0, iolist_size(IOData)-1),
        N = iolist_size(IOData)-1,
        <<Head:N/binary, _/binary>> = iolist_to_binary(IOData),
        'take_n_chars - assertions'(IOData, N, binary_to_list(Head))
    end).

'take_n_chars - N equal'(IOData) ->
    ?_test_(begin
        N = iolist_size(IOData),
        'take_n_chars - assertions'(IOData, N, binary_to_list(iolist_to_binary(IOData)))
    end).

'take_n_chars - N more'(IOData) ->
    ?_test_(begin
        ?assertMatch(N when N > 0, iolist_size(IOData)+1),
        N = iolist_size(IOData)+1,
        ?assertNotMatch(<<_:N/binary>>, iolist_to_binary(IOData)),
        'take_n_chars - assertions'(IOData, N, binary_to_list(iolist_to_binary(IOData)))
    end).

'take_n_chars - assertions'(IOData, N, ExpectedChars) ->
    ?_test_(begin
        {{Chars, NumChars}, Rest} = take_n_chars(IOData, N),
        RestBin = iolist_to_binary(Rest),
        ExpectedNumChars = length(ExpectedChars),
        ?assertEqual(ExpectedNumChars, NumChars),
        ?assertMatch(<<_:ExpectedNumChars/binary, Tail/binary>> when Tail =:= RestBin, iolist_to_binary(IOData)),
        ?assertEqual(ExpectedChars, Chars)
    end).

get_ascii_alphas() ->
    case erlang:get(?FUNCTION_NAME) of
        undefined ->
            AsciiAlphas = list_to_tuple(lists:seq($a, $z) ++ lists:seq($A, $Z)),
            erlang:put(?FUNCTION_NAME, AsciiAlphas),
            AsciiAlphas;
        AsciiAlphas ->
            AsciiAlphas
    end.

generate_ascii_alphas(N) ->
    (fun
        F(CharN, Acc) when CharN =< 0 ->
            Acc;
        F(CharN, Acc0) ->
            CharPos = rand:uniform(tuple_size(get_ascii_alphas())),
            Char = element(CharPos, get_ascii_alphas()),
            IOData = 
                case rand:uniform(3) of
                    1 ->
                        Char;
                    2 ->
                        [Char];
                    3 ->
                        <<Char>>
                end,
            Acc1 =
                case rand:uniform(2) of
                    1 ->
                        [IOData | Acc0];
                    2 ->
                        [IOData, Acc0]
                end,
            F(CharN-1, Acc1)
    end)(N, []).

-endif.