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

-record(state, {nodes :: list(node_spec()),
                connect_done:: ordsets:ordset(ldb_node_id()),
                sim_done :: ordsets:ordset(ldb_node_id()),
                metrics_done :: ordsets:ordset(ldb_node_id()),
                start_time :: timestamp()}).

-define(BARRIER_PEER_SERVICE, lsim_barrier_peer_service).
-define(INTERVAL, 3000).

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server callbacks
init([]) ->
    schedule_create_barrier(),
    lager:info("lsim_rsg_master initialized"),
    {ok, #state{nodes=[],
                connect_done=ordsets:new(),
                sim_done=ordsets:new(),
                metrics_done=ordsets:new(),
                start_time=0}}.

handle_call(Msg, _From, State) ->
    lager:warning("Unhandled call message: ~p", [Msg]),
    {noreply, State}.

handle_cast({connect_done, NodeName},
            #state{connect_done=ConnectDone0,
                   start_time=T0}=State) ->

    lager:info("Received CONNECT DONE from ~p", [NodeName]),

    ConnectDone1 = ordsets:add_element(NodeName, ConnectDone0),

    T1 = case ordsets:size(ConnectDone1) == node_number() of
        true ->
            lager:info("Everyone is CONNECT DONE. SIM GO!"),
            tell(sim_go),
            schedule_create_partitions(),
            ldb_util:unix_timestamp();
        false ->
            T0
    end,

    {noreply, State#state{connect_done=ConnectDone1,
                          start_time=T1}};

handle_cast({sim_done, NodeName},
            #state{sim_done=SimDone0}=State) ->

    lager:info("Received SIM DONE from ~p", [NodeName]),

    SimDone1 = ordsets:add_element(NodeName, SimDone0),

    case ordsets:size(SimDone1) == node_number() of
        true ->
            lager:info("Everyone is SIM DONE. METRICS GO!"),
            tell(metrics_go);
        false ->
            ok
    end,

    {noreply, State#state{sim_done=SimDone1}};

handle_cast({metrics_done, NodeName},
            #state{metrics_done=MetricsDone0,
                   start_time=StartTime}=State) ->

    lager:info("Received METRICS DONE from ~p", [NodeName]),

    MetricsDone1 = ordsets:add_element(NodeName, MetricsDone0),

    case ordsets:size(MetricsDone1) == node_number() of
        true ->
            lager:info("Everyone is METRICS DONE. STOP!!!"),
            lsim_simulations_support:push_lsim_metrics(StartTime),
            lsim_orchestration:stop_tasks([lsim, rsg]);
        false ->
            ok
    end,

    {noreply, State#state{metrics_done=MetricsDone1}};

handle_cast(Msg, State) ->
    lager:warning("Unhandled cast message: ~p", [Msg]),
    {noreply, State}.

handle_info(create_barrier, State) ->
    Nodes = lsim_orchestration:get_tasks(lsim, ?BARRIER_PORT, true),

    case length(Nodes) == node_number() of
        true ->
            ok = connect(Nodes);
        false ->
            schedule_create_barrier()
    end,
    {noreply, State#state{nodes=Nodes}};

handle_info(create_partitions, #state{nodes=Nodes}=State) ->
    PartitionNumber = lsim_config:get(lsim_partition_number),

    case PartitionNumber > 1 of
        true ->
            {PartitionToIPs, IPToPartition} = lsim_overlay:partitions(Nodes, PartitionNumber),
            lager:info("PARTITIONS ~p\n~p\n\n", [PartitionToIPs, IPToPartition]),

            lists:foreach(
                fun({Name, IP, _}) ->
                    Partition = orddict:fetch(IP, IPToPartition),

                    %% calculate the list of ips to reject
                    IPs = lists:foldl(
                        fun({P, I}, Acc) ->
                            %% each partition blocks
                            %% the partitions with higher ids
                            case P > Partition of
                                true ->
                                    lists:append(Acc, I);
                                _ ->
                                    Acc
                            end
                        end,
                        [],
                        PartitionToIPs
                    ),

                    lager:info("NODE ~p REJECTS IPS ~p\n", [Name, IPs]),

                    %% tell this node to reject these ips
                    tell({reject_ips, IPs}, [Name])
                end,
                Nodes
            ),

            schedule_heal_partitions();
        false ->
            ok
    end,

    {noreply, State};

handle_info(heal_partitions, State) ->
    tell(heal_partitions),
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
schedule_create_partitions() ->
    NodeEventNumber = lsim_config:get(lsim_node_event_number),
    %% wait ~50% of simulation time before creating partitions
    Seconds = NodeEventNumber div 2,
    timer:send_after(Seconds * 1000, create_partitions).

%% @private
schedule_heal_partitions() ->
    NodeEventNumber = lsim_config:get(lsim_node_event_number),
    %% wait ~25% of simulation time before healing partitions
    Seconds = NodeEventNumber div 4,
    timer:send_after(Seconds * 1000, heal_partitions).

%% @private
connect([]) ->
    ok;
connect([Node|Rest]=All) ->
    case ?BARRIER_PEER_SERVICE:join(Node) of
        ok ->
            connect(Rest);
        Error ->
            lager:info("Couldn't connect to ~p. Reason ~p. Will try again in ~p ms",
                       [Node, Error, ?INTERVAL]),
            timer:sleep(?INTERVAL),
            connect(All)
    end.

%% @private send to all
tell(Msg) ->
    {ok, Members} = ?BARRIER_PEER_SERVICE:members(),
    tell(Msg, without_me(Members)).

%% @private send to some
tell(Msg, Peers) ->
    lists:foreach(
        fun(Peer) ->
            ?BARRIER_PEER_SERVICE:forward_message(
               Peer,
               lsim_rsg,
               Msg
            )
        end,
        Peers
     ).

%% @private
without_me(Members) ->
    Members -- [ldb_config:id()].
