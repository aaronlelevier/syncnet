%%%-------------------------------------------------------------------
%%% @author aaron lelevier
%%% @doc
%%% @end
%%%--------------------------------------------------------------------module(round).
-module(round).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([do_round/0, add_vertex/1, list_vertices/0, link_vertices/1]).

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

%% adds a vertex to the list of vertices
-spec add_vertex(Pid::pid()) -> ok.
add_vertex(Pid) ->
  gen_server:call(?SERVER, {add_vertex, Pid}).

%% lists all vertices
-spec list_vertices() -> [pid()].
list_vertices() ->
  gen_server:call(?SERVER, list_vertices).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  State = #{
    count => 0,
    vertices => []
  },
  {ok, State}.

handle_call(do_round, _From, State) ->
  Count = maps:get(count, State) + 1,
  % tell each Pid which Pid is next in the Ring
  State2 = link_vertices(State),

  {reply, {count, Count}, State2#{count := Count}};

handle_call({add_vertex, Pid}, _From, State) ->
  Vertices = [Pid | maps:get(vertices, State)],
  {reply, ok, State#{vertices := Vertices}};

handle_call(list_vertices, _From, State) ->
  {reply, maps:get(vertices, State), State};

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

link_vertices(State) ->
  case maps:get(vertices, State) of
    [H|T] ->
      L = link_vertices(H, T, []),
      State#{vertices := L};
    [] -> State
  end.

link_vertices(First, [], Acc) ->
  [H|_] = tl(Acc),
  vertex:link_next(First, H),
  Acc;
link_vertices(First, [H|T], Acc) ->
  vertex:link_next(First, H),
  link_vertices(H, T, [First|Acc]).
