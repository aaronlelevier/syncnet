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

round_setup() ->
  {ok, _Pid} = round:start_link().

round_cleanup(_) ->
  process_flag(trap_exit, true),
  exit(whereis(round), shutdown).

can_get_handler_list_test_() ->
  {setup,
    fun round_setup/0,
    fun round_cleanup/1,
    [
      fun do_round_increments_round_count/0,
      fun add_and_list_vertices/0
    ]
  }.

do_round_increments_round_count() ->
  {count, Count} = round:do_round(),
  ?assertEqual(1, Count),

  {count, Count2} = round:do_round(),
  ?assertEqual(2, Count2).

add_and_list_vertices() ->
  {ok, Pid} = vertex:start_link(),
  {ok, Pid2} = vertex:start_link(),

  ?assertEqual(ok, round:add_vertex(Pid)),
  ?assertEqual([Pid], round:list_vertices()),

  ?assertEqual(ok, round:add_vertex(Pid2)),
  ?assertEqual([Pid2, Pid], round:list_vertices()).
