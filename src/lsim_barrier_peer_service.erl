%%
%% Copyright (c) 2016 SyncFree Consortium.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-module(lsim_barrier_peer_service).
-author("Vitor Enes Duarte <vitorenesduarte@gmail.com").

-include("lsim.hrl").

-behaviour(gen_server).

%% peer service callbacks
-export([start_link/0,
         members/0,
         join/1,
         forward_message/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {connected :: orddict:orddict()}).

-define(LOG_INTERVAL, 10000).

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec members() -> {ok, [ldb_node_id()]}.
members() ->
    gen_server:call(?MODULE, members, infinity).

-spec join(node_spec()) -> ok | error().
join(NodeSpec) ->
    gen_server:call(?MODULE, {join, NodeSpec}, infinity).

-spec forward_message(ldb_node_id(), handler(), message()) ->
    ok | error().
forward_message(LDBId, Handler, Message) ->
    gen_server:call(?MODULE, {forward_message, LDBId, Handler, Message}, infinity).

%% gen_server callbacks
init([]) ->
    {ok, _} = lsim_barrier_peer_service_server:start_link(?BARRIER_PORT),
    ?LOG("lsim_barrier_peer_service initialized!"),
    {ok, #state{connected=orddict:new()}}.

handle_call(members, _From, #state{connected=Connected}=State) ->
    Result = {ok, orddict:fetch_keys(Connected)},
    {reply, Result, State};

handle_call({join, {LDBId, {_, _, _, _}=Ip, Port}=NodeSpec}, _From,
            #state{connected=Connected0}=State) ->
    {Result, Connected1} = case orddict:find(LDBId, Connected0) of
        {ok, _} ->
            {ok, Connected0};
        error ->
            case gen_tcp:connect(Ip, Port, ?TCP_OPTIONS) of
                {ok, Socket} ->
                    {ok, Pid} = lsim_barrier_peer_service_client:start_link(Socket),
                    gen_tcp:controlling_process(Socket, Pid),
                    {ok, orddict:store(LDBId, Pid, Connected0)};
                Error ->
                    ?LOG("Error handling join call on node ~p to node ~p. Reason ~p", [ldb_config:id(), NodeSpec, Error]),
                    {Error, Connected0}
            end
    end,
    {reply, Result, State#state{connected=Connected1}};

handle_call({forward_message, LDBId, Handler, Message}, _From, #state{connected=Connected}=State) ->
    Result = case orddict:find(LDBId, Connected) of
        {ok, Pid} ->
            Pid ! {forward_message, Handler, Message},
            ok;
        error ->
            {error, not_connected}
    end,

    {reply, Result, State};

handle_call(Msg, _From, State) ->
    lager:warning("Unhandled call message: ~p", [Msg]),
    {noreply, State}.

handle_cast(Msg, State) ->
    lager:warning("Unhandled cast message: ~p", [Msg]),
    {noreply, State}.

handle_info(Msg, State) ->
    lager:warning("Unhandled info message: ~p", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
