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
      fun link_vertices_to_next/0
    ]
  }.

do_round_increments_round_count() ->
  {count, Count} = round:do_round(),
  ?assertEqual(1, Count),

  {count, Count2} = round:do_round(),
  ?assertEqual(2, Count2).

link_vertices_to_next() ->
  {ok, Pid1} = vertex:start_link(),
  {ok, Pid2} = vertex:start_link(),
  {ok, Pid3} = vertex:start_link(),
  % add vertices to round
  ?assertEqual(ok, round:add_vertex(Pid1)),
  ?assertEqual(ok, round:add_vertex(Pid2)),
  ?assertEqual(ok, round:add_vertex(Pid3)),
  % each Vertex doesn't have a Next until 'link_vertices' is called below
  _ = [
    ?assertEqual(undefined, vertex:get_next(X)) ||
    X <- [Pid3, Pid2, Pid1]],

  ?assertEqual(ok, round:link_vertices()),

  _ = [
    ?assertEqual(true, is_pid(vertex:get_next(X))) ||
    X <- [Pid3, Pid2, Pid1]].

link_vertices_test() ->
  {ok, Pid1} = vertex:start_link(),
  {ok, Pid2} = vertex:start_link(),
  {ok, Pid3} = vertex:start_link(),
  _ = [
    ?assertEqual(undefined, vertex:get_next(X)) ||
    X <- [Pid3, Pid2, Pid1]],

  Ret = round:link_vertices(#{vertices => [Pid3, Pid2, Pid1]}),

  ?assertEqual(true, is_map(Ret)),
  % order is maintained
  ?assertEqual([Pid3, Pid2, Pid1], maps:get(vertices, Ret)),
  % next is set for each vertex
  ?assertEqual(Pid2, vertex:get_next(Pid3)),
  ?assertEqual(Pid1, vertex:get_next(Pid2)),
  ?assertEqual(Pid3, vertex:get_next(Pid1)).
