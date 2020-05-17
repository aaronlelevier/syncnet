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
-include_lib("syncnet/include/macros.hrl").

%% API
-export([start_link/0]).
-export([get_state/1, get_status/1, get_uid/1, wakeup/1, get_next/1,
  link_next/2]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

%% Macros
-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

get_state(Pid) -> gen_server:call(Pid, get_state).

get_status(Pid) -> gen_server:call(Pid, get_status).

get_uid(Pid) -> gen_server:call(Pid, get_uid).

get_next(Pid) -> gen_server:call(Pid, get_next).

wakeup(Pid) ->
  gen_server:call(Pid, wakeup).

link_next(Pid, Next) ->
  gen_server:call(Pid, {link_next, Next}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
  Uid = rand:uniform(),
  gen_server:start_link(?MODULE, Uid, []).

init(Uid) ->
  State = #{
    state => idle,
    status => unknown,
    uid => Uid,
    next => undefined
  },
  {ok, State}.

%% getters
handle_call(get_state, _From, State) -> {reply, maps:get(state, State), State};
handle_call(get_status, _From, State) -> {reply, maps:get(status, State), State};
handle_call(get_uid, _From, State) -> {reply, maps:get(uid, State), State};
handle_call(get_next, _From, State) -> {reply, maps:get(next, State), State};

%% wakeup - only works if called from "env" process
handle_call(wakeup, {FromPid, _Ref}, State) ->
  NewState = case process_info(FromPid, registered_name) of
    {registered_name, Name} ->
      if Name == env -> State#{state := running};
        true -> State
      end;
    [] -> State
  end,
  Reply = {state, maps:get(state, NewState)},
  {reply, Reply, NewState};

handle_call({link_next, Next}, _From, State) ->
  {reply, ok, State#{next := Next}};

%% catch all
handle_call(_Request, _From, State) ->
  ?DEBUG({_Request, _From, State}),
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
