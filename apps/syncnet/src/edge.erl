%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(edge).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(edge_state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, #edge_state{}}.

handle_call(_Request, _From, State = #edge_state{}) ->
  {reply, ok, State}.

handle_cast(_Request, State = #edge_state{}) ->
  {noreply, State}.

handle_info(_Info, State = #edge_state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #edge_state{}) ->
  ok.

code_change(_OldVsn, State = #edge_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
