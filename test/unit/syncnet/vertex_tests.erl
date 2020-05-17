%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%%
%%% @end
%%% Created : 15. May 2020 7:03 AM
%%%-------------------------------------------------------------------
-module(vertex_tests).
-author("Aaron Lelevier").
-compile(nowarn_export_all).
-include_lib("eunit/include/eunit.hrl").

initialized_as_idle_with_default_state_test() ->
  {ok, Pid} = vertex:start_link(),
  Ret = vertex:get_state(Pid),
  ?assertEqual(idle, Ret).

wakeup_msg_from_env_module_changes_state_from_idle_to_running_test() ->
  {ok, Pid} = vertex:start_link(),
  {ok, _Pid} = env:start_link(),
  % pre-test - initial state is idle
  ?assertEqual(idle, vertex:get_state(Pid)),

  % replies that state is running
  ?assertEqual({state, running}, env:wakeup(Pid)),

  % state is now running
  ?assertEqual(running, vertex:get_state(Pid)).

wakeup_msg_from_non_env_does_not_change_state_test() ->
  {ok, Pid} = vertex:start_link(),
  % pre-test - initial state is idle
  ?assertEqual(idle, vertex:get_state(Pid)),

  % replies that state is running
  ?assertEqual({state, idle}, vertex:wakeup(Pid)),

  % state is now running
  ?assertEqual(idle, vertex:get_state(Pid)).

trans_vid_gt_uid_so_replace_uid_test() ->
  Uid = 1,
  Vid = 2,
  State = #{uid => Uid, vid => Uid, status => unknown},
  Ret = vertex:trans(Vid, State),
  ?assertEqual(#{uid => Uid, vid => Vid, status => unknown}, Ret).

trans_vid_lt_uid_so_do_nothing_test() ->
  Uid = 2,
  Vid = 1,
  State = #{uid => Uid, vid => Uid, status => unknown},
  Ret = vertex:trans(Vid, State),
  ?assertEqual(State, Ret).

trans_vid_eq_uid_so_elect_leader_test() ->
  Uid = 2,
  Vid = 2,
  State = #{uid => Uid, vid => Uid, status => unknown},
  Ret = vertex:trans(Vid, State),
  ?assertEqual(#{uid => Uid, vid => Vid, status => leader}, Ret).
