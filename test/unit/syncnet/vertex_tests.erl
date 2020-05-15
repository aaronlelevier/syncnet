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
  ?assertEqual(#{state => idle, data => #{}}, Ret).

%%nonnull_message_changes_state_from_idle_to_running() ->
%%  {ok, Pid} = vertex:start_link(),
