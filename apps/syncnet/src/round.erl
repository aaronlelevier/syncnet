%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @doc
%%% @end
%%%--------------------------------------------------------------------module(round).
-module(round).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([do_round/0]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

%% Macros
-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc performs a round and increments the round count
-spec do_round() -> {count, integer()}.
do_round() ->
  gen_server:call(?SERVER, do_round).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, #{count => 0}}.

handle_call(do_round, _From, State) ->
  Count = maps:get(count, State) + 1,
  {reply, {count, Count}, State#{count := Count}};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc signals to Edges to put all outgoing Messages in Vertices
apply_msg_gen_func() -> 0.

%% @doc signals to Edges to apply their State Transition function
%% based upon their Current State and the Incoming Message
apply_state_trans_func() -> 0.