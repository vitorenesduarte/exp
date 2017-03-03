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

-module(lsim_rsg_master).
-author("Vitor Enes Duarte <vitorenesduarte@gmail.com").

-include("lsim.hrl").

-behaviour(gen_server).

%% lsim_rsg_master callbacks
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {nodes_ready :: ordsets:ordset(ldb_node_id()),
                nodes_done :: ordsets:ordset(ldb_node_id())}).

-define(BARRIER_PEER_SERVICE, lsim_barrier_peer_service).
-define(INTERVAL, 3000).

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server callbacks
init([]) ->
    schedule_create_barrier(),
    ?LOG("lsim_rsg_master initialized"),
    {ok, #state{nodes_ready=ordsets:new(),
                nodes_done=ordsets:new()}}.

handle_call(Msg, _From, State) ->
    lager:warning("Unhandled call message: ~p", [Msg]),
    {noreply, State}.

handle_cast({ready, NodeName},
            #state{nodes_ready=NodesReady0}=State) ->

    ?LOG("Received READY from ~p", [NodeName]),

    NodesReady1 = ordsets:add_element(NodeName, NodesReady0),

    case ordsets:size(NodesReady1) == node_number() of
        true ->
            ?LOG("Everyone is ready. GO!"),
            tell(go);
        false ->
            ok
    end,

    {noreply, State#state{nodes_ready=NodesReady1}};

handle_cast({done, NodeName},
            #state{nodes_done=NodesDone0}=State) ->

    ?LOG("Received DONE from ~p", [NodeName]),

    NodesDone1 = ordsets:add_element(NodeName, NodesDone0),

    case ordsets:size(NodesDone1) == node_number() of
        true ->
            ?LOG("Everyone is done. STOP!"),
            lsim_orchestration:stop();
        false ->
            ok
    end,

    {noreply, State#state{nodes_done=NodesDone1}};

handle_cast(Msg, State) ->
    lager:warning("Unhandled cast message: ~p", [Msg]),
    {noreply, State}.

handle_info(create_barrier, State) ->
    Nodes = lsim_orchestration:nodes(?BARRIER_PORT),

    case length(Nodes) == node_number() of
        true ->
            ok = connect(Nodes);
        false ->
            schedule_create_barrier()
    end,
    {noreply, State};

handle_info(Msg, State) ->
    lager:warning("Unhandled info message: ~p", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
node_number() ->
    lsim_config:get(lsim_node_number).

%% @private
schedule_create_barrier() ->
    timer:send_after(?INTERVAL, create_barrier).

%% @private
connect([]) ->
    ok;
connect([Node|Rest]=All) ->
    case ?BARRIER_PEER_SERVICE:join(Node) of
        ok ->
            connect(Rest);
        Error ->
            ?LOG("Couldn't connect to ~p. Reason ~p. Will try again in ~p ms",
                 [Node, Error, ?INTERVAL]),
            timer:sleep(?INTERVAL),
            connect(All)
    end.

%% @private
tell(Msg) ->
    {ok, Members} = ?BARRIER_PEER_SERVICE:members(),
    lists:foreach(
        fun(Peer) ->
            ?BARRIER_PEER_SERVICE:forward_message(
               Peer,
               lsim_rsg,
               Msg
            )
        end,
        without_me(Members)
     ).

%% @private
without_me(Members) ->
    Members -- [ldb_config:id()].
