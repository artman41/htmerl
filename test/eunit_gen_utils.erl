-module(eunit_gen_utils).
-include("eunit_primitives.hrl").
-export([
    n_generator/3, n_generator/4,
    seq_generator/1
]).

n_generator(N, SetupFun, InstantiatorFun) ->
    n_generator(N, SetupFun, InstantiatorFun, []).

n_generator(N, SetupFun, InstantiatorFun, CommandAcc) when N > 0 
                                          andalso is_function(SetupFun) 
                                          andalso is_function(InstantiatorFun, 1) ->
    ?EP_GENERATOR(fun() ->
        n_generator_(N, SetupFun, InstantiatorFun, CommandAcc)
    end).

seq_generator(List = [_|_]) ->
    seq_generator_(List).


%% INTERNAL

n_generator_(0, _SetupFun, _InstantiatorFun, CommandAcc) ->
    CommandAcc;
n_generator_(N, SetupFun, InstantiatorFun, CommandAcc) ->
    SetupFun_ = 
        case is_function(SetupFun, 1) of
            true -> 
                fun() -> SetupFun(N) end;
            false -> 
                SetupFun
        end,
    [{setup, SetupFun_, InstantiatorFun} | n_generator_(N-1, SetupFun, InstantiatorFun, CommandAcc)].

seq_generator_([]) ->
    [];
seq_generator_([H|T]) when is_tuple(H) ->
    ?EP_GENERATOR(fun() ->
        [H | seq_generator_(T)]
    end).