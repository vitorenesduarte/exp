%%
%% Copyright (c) 2018 Vitor Enes.  All Rights Reserved.
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

-module(exp_sup).
-author("Vitor Enes <vitorenesduarte@gmail.com").

-include("exp.hrl").

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(CHILD(I, Type, Timeout),
        {I, {I, start_link, []}, permanent, Timeout, Type, [I]}).
-define(CHILD(I), ?CHILD(I, worker, 5000)).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    configure_peer_service(),
    {Simulation, Orchestration, RSG} = configure(),

    Children = exp_specs(Simulation, Orchestration, RSG),

    lager:info("exp_sup initialized!"),
    RestartStrategy = {one_for_one, 10, 10},
    {ok, {RestartStrategy, Children}}.

%% @private
configure_peer_service() ->
    %% configure exp overlay
    configure_var("OVERLAY",
                  exp_overlay,
                  ?DEFAULT_OVERLAY),

    %% configure partisan manager
    PeerService = partisan_static_peer_service_manager,
    partisan_config:set(partisan_peer_service_manager,
                        PeerService).

%% @private
configure() ->
    %% configure exp simulation
    Simulation = configure_var("SIMULATION",
                               exp_simulation,
                               undefined),

    %% configure node number
    configure_int("NODE_NUMBER",
                  exp_node_number,
                  1),

    %% configure node event number
    configure_int("NODE_EVENT_NUMBER",
                  exp_node_event_number,
                  30),

    %% configure unique simulation timestamp
    configure_int("TIMESTAMP",
                  exp_timestamp,
                  0),

    %% configure api server
    configure_str("APISERVER",
                  exp_api_server,
                  undefined),

    %% configure auth token
    configure_str("TOKEN",
                  exp_token,
                  undefined),

    %% configure orchestration
    Orchestration = configure_var("ORCHESTRATION",
                                  exp_orchestration,
                                  undefined),

    %% configure rsg master
    RSG = configure_var("RSG",
                        exp_rsg,
                        false),

    %% configure break links
    configure_var("BREAK_LINKS",
                  exp_break_links,
                  none),

    %% configure gmap simulation key percentage
    configure_int("GMAP_SIMULATION_KEY_PERCENTAGE",
                  exp_gmap_simulation_key_percentage,
                  100),

    {Simulation, Orchestration, RSG}.

%% @private
exp_specs(Simulation, Orchestration, RSG) ->
    SimulationSpecs = exp_simulations:get_specs(Simulation),

    OrchestrationSpecs = case Orchestration of
        undefined ->
            [];
        _ ->
            BarrierPeerServiceSpecs = [?CHILD(exp_barrier_peer_service)],
            Store = [?CHILD(exp_redis_metrics_store)],

            RSGSpecs = case RSG of
                true ->
                    [?CHILD(exp_rsg_master)];
                false ->
                    [?CHILD(exp_rsg)]
            end,

            HTTPSpecs = case RSG of
                true ->
                    [];
                false ->
                    [?CHILD(exp_resource)]
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
    Current = exp_config:get(Var, Default),
    Val = From(
        os:getenv(Env, To(Current))
    ),
    exp_config:set(Var, Val),
    Val.
