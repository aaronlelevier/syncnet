%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @doc
%%% A "vertex" is a Node in the network. These are it's states:
%%% - idle
%%% - running
%%% - halted
%%% @end
%%%--------------------------------------------------------------------module(vertex).
-module(vertex).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([get_state/1]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-define(DEFAULT_STATE, #{
  state => idle,
  data => #{}
}).

%%%===================================================================
%%% API
%%%===================================================================

get_state(Pid) ->
  gen_server:call(Pid, get_state).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link(?MODULE, [], []).

init([]) ->
  {ok, ?DEFAULT_STATE}.

handle_call(get_state, _From, State) ->
  Reply = State,
  {reply, Reply, State};
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
