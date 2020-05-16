%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%%
%%% @end
%%% Created : 16. May 2020 3:51 PM
%%%-------------------------------------------------------------------
-module(round_tests).
-author("Aaron Lelevier").
-compile(nowarn_export_all).
-include_lib("eunit/include/eunit.hrl").

do_round_increments_round_count_test() ->
  {ok, _Pid} = round:start_link(),

  {count, Count} = round:do_round(),
  ?assertEqual(1, Count),

  {count, Count2} = round:do_round(),
  ?assertEqual(2, Count2).
