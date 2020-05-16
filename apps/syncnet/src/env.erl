%%%-------------------------------------------------------------------
%%% @author Aaron Lelevier
%%% @doc This is the Environment (Env) Node - there is only one of these
%%% Nodes in the network.
%%%
%%% The Env Node is thought of to always be 'running', so it doesn't
%%% have states
%%% @end
%%%-------------------------------------------------------------------
-module(env).
-behaviour(gen_server).
-include_lib("syncnet/include/macros.hrl").

%% API
-export([start_link/0]).
-export([wakeup/1]).
%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

%% Macros
-define(SERVER, ?MODULE).


%%%===================================================================
%%% API
%%%===================================================================

-spec wakeup(pid()) -> {state, atom()}.
wakeup(NodePid) ->
  gen_server:call(?SERVER, {wakeup, NodePid}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

%% @doc - this is a named process, only 1 in the network
-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, #{}}.

handle_call({wakeup, Pid}, _From, State) ->
  ?DEBUG({{wakeup, Pid}, _From, State}),
  {reply, vertex:wakeup(Pid), State};
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
