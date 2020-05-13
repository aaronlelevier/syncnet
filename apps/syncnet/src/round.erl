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

-define(SERVER, ?MODULE).

-record(round_state, {
  count = 0
}).

%%%===================================================================
%%% API
%%%===================================================================

do_round() ->
  gen_server:call(?SERVER, do_round).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, #round_state{}}.

handle_call(do_round, _From, State) ->
  NewState = State#round_state{count = State#round_state.count + 1},
  Count = NewState#round_state.count,
  {reply, Count, NewState};
handle_call(_Request, _From, State = #round_state{}) ->
  {reply, ok, State}.

handle_cast(_Request, State = #round_state{}) ->
  {noreply, State}.

handle_info(_Info, State = #round_state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #round_state{}) ->
  ok.

code_change(_OldVsn, State = #round_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc signals to Edges to put all outgoing Messages in Vertices
apply_msg_gen_func() -> 0.

%% @doc signals to Edges to apply their State Transition function
%% based upon their Current State and the Incoming Message
apply_state_trans_func() -> 0.