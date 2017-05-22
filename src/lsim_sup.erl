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

-module(lsim_sup).
-author("Vitor Enes Duarte <vitorenesduarte@gmail.com").

-include("lsim.hrl").

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    configure_peer_service(),
    {Simulation, Orchestration, RSG} = configure(),

    Children = lsim_specs(Simulation, Orchestration, RSG),

    ?LOG("lsim_sup initialized!"),
    RestartStrategy = {one_for_one, 10, 10},
    {ok, {RestartStrategy, Children}}.

%% @private
configure_peer_service() ->
    %% configure lsim overlay
    Overlay = configure_var("OVERLAY",
                            lsim_overlay,
                            ?DEFAULT_OVERLAY),

    PeerService = case Overlay of
        hyparview ->
            partisan_hyparview_peer_service_manager;
        _ ->
            partisan_static_peer_service_manager
    end,


    %% configure ldb peer service
    ldb_config:set(ldb_peer_service, PeerService),

    %% configure partisan manager
    partisan_config:set(partisan_peer_service_manager,
                        PeerService).

%% @private
configure() ->
    %% configure lsim simulation
    Simulation = configure_var("SIMULATION",
                               lsim_simulation,
                               undefined),

    %% configure node number
    configure_int("NODE_NUMBER",
                  lsim_node_number,
                  1),

    %% configure node event number
    configure_int("NODE_EVENT_NUMBER",
                  lsim_node_event_number,
                  30),

    %% configure unique simulation timestamp
    configure_int("TIMESTAMP",
                  lsim_timestamp,
                  0),

    %% configure api server
    configure_str("APISERVER",
                  lsim_api_server,
                  undefined),

    %% configure auth token
    configure_str("TOKEN",
                  lsim_token,
                  undefined),

    %% configure orchestration
    Orchestration = configure_var("ORCHESTRATION",
                                  lsim_orchestration,
                                  undefined),

    %% configure rsg master
    RSG = configure_var("RSG",
                        lsim_rsg,
                        false),

    %% configure metrics store
    configure_var("METRICS_STORE",
                  lsim_metrics_store,
                  undefined),


    %% configure partition number
    _PartitionNumber = configure_int("PARTITION_NUMBER",
                              lsim_partition_number,
                              1),

    %% configure tcp keepalive defaults
    %lsim_iptables:configure_tcp_keepalive(PartitionNumber, RSG),

    {Simulation, Orchestration, RSG}.

%% @private
lsim_specs(Simulation, Orchestration, RSG) ->
    SimulationSpecs = lsim_simulations:get_specs(Simulation),

    OrchestrationSpecs = case Orchestration of
        undefined ->
            [];
        _ ->
            BarrierPeerServiceSpecs = [{lsim_barrier_peer_service,
                                        {lsim_barrier_peer_service,
                                         start_link, []},
                                        permanent, 5000, worker,
                                        [lsim_barrier_peer_service]}],

            Store = [{lsim_metrics_store,
                      {lsim_metrics_store, start_link, []},
                      permanent, 5000, worker,
                      [lsim_metrics_store]}],

            RSGSpecs = case RSG of
                true ->
                    [{lsim_rsg_master,
                      {lsim_rsg_master, start_link, []},
                      permanent, 5000, worker,
                      [lsim_rsg_master]}];
                false ->
                    [{lsim_rsg,
                      {lsim_rsg, start_link, []},
                      permanent, 5000, worker,
                      [lsim_rsg]}]
            end,

            HTTPSpecs = case RSG of
                true ->
                    [];
                false ->
                    %% configure membership callback
                    MembershipFun = fun(Membership) ->
                        lsim_resource:update_membership(Membership)
                    end,
                    partisan_peer_service:add_sup_callback(MembershipFun),

                    [{lsim_resource,
                      {lsim_resource, start_link, []},
                      permanent, 5000, worker,
                      [lsim_resource]}]
            end,

            BarrierPeerServiceSpecs ++ Store ++ RSGSpecs ++ HTTPSpecs
    end,

    SimulationSpecs ++ OrchestrationSpecs.

%% @private
configure_var(Env, Var, Default) ->
    To = fun(V) -> atom_to_list(V) end,
    From = fun(V) -> list_to_atom(V) end,
    configure(Env, Var, Default, To, From).

%% @private
configure_str(Env, Var, Default) ->
    F = fun(V) -> V end,
    configure(Env, Var, Default, F, F).

%% @private
configure_int(Env, Var, Default) ->
    To = fun(V) -> integer_to_list(V) end,
    From = fun(V) -> list_to_integer(V) end,
    configure(Env, Var, Default, To, From).

%% @private
configure(Env, Var, Default, To, From) ->
    Current = lsim_config:get(Var, Default),
    Val = From(
        os:getenv(Env, To(Current))
    ),
    lsim_config:set(Var, Val),
    Val.
