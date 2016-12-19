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
    {PeerService, PeerServiceSpecs} = peer_service_specs(),
    LDBSpecs = ldb_specs(PeerService),
    SimSpecs = sim_specs(),
    Children = PeerServiceSpecs ++
               LDBSpecs ++
               SimSpecs,

    ldb_log:info("lsim_sup initialized!"),
    RestartStrategy = {one_for_one, 10, 10},
    {ok, {RestartStrategy, Children}}.

%% @private
peer_service_specs() ->
    %% os env vars override possible application env vars
    %% configure lsim peer service
    PeerServiceDefault = lsim_config:get(lsim_peer_service, ?DEFAULT_PEER_SERVICE),
    PeerService = list_to_atom(
        os:getenv("PEER_SERVICE", atom_to_list(PeerServiceDefault))
    ),
    lsim_config:set(lsim_peer_service, PeerService),

    %% get ip and port
    {Ip, Port} = ip_and_port(),

    Specs = case PeerService of
        lsim_static_peer_service ->
            %% configure lsim ip and port
            lsim_config:set(lsim_peer_ip, Ip),
            lsim_config:set(lsim_peer_port, Port),

            %% specs
            [{lsim_static_peer_service,
              {lsim_static_peer_service, start_link, []},
              permanent, 5000, worker, [lsim_static_peer_service]}];

        partisan_hyparview_peer_service_manager ->
            %% configure partisan manager, ip and port
            partisan_config:set(partisan_peer_service_manager,
                                partisan_hyparview_peer_service_manager),
            partisan_config:set(peer_ip, Ip),
            partisan_config:set(peer_port, Port),

            %% specs
            [{partisan_sup,
              {partisan_sup, start_link, []},
              permanent, infinity, supervisor, [partisan_sup]}]
    end,

    {PeerService, Specs}.

%% @private
ldb_specs(PeerService) ->
    %% os env vars override possible application env vars
    %% configure ldb mode
    LDBModeDefault = ldb_config:get(ldb_mode, ?DEFAULT_MODE),
    LDBMode = list_to_atom(
        os:getenv("LDB_MODE", atom_to_list(LDBModeDefault))
    ),
    ldb_config:set(ldb_mode, LDBMode),

    %% configure join decompositions
    JDDefault = ldb_config:get(ldb_join_decompositions, false),
    JD = list_to_atom(
        os:getenv("LDB_JOIN_DECOMPOSITIONS", atom_to_list(JDDefault))
    ),
    ldb_config:set(ldb_join_decompositions, JD),

    %% configure ldb peer service
    ldb_config:set(ldb_peer_service, PeerService),

    %% specs
    [{ldb_sup,
      {ldb_sup, start_link, []},
      permanent, infinity, supervisor, [ldb_sup]}].

%% @private
sim_specs() ->
    %% os env vars override possible application env vars
    %% configure lsim simulation
    SimulationDefault = lsim_config:get(lsim_simulation, undefined),
    Simulation = list_to_atom(
        os:getenv("SIMULATION", atom_to_list(SimulationDefault))
    ),
    lsim_config:set(lsim_simulation, Simulation),

    %% specs
    lsim_simulations:get_specs(Simulation).

%% @private
ip_and_port() ->
    Ip = case os:getenv("PEER_IP", "undefined") of
        "undefined" ->
            {127, 0, 0, 1};
        PeerIp ->
            {ok, IPAddress} = inet_parse:address(PeerIp),
            IPAddress
    end,
    Port = case os:getenv("PEER_PORT", "undefined") of
        "undefined" ->
            random_port();
        PeerPort ->
            list_to_integer(PeerPort)
    end,
    {Ip, Port}.

%% @private
random_port() ->
    rand_compat:seed(erlang:phash2([node()]),
                     erlang:monotonic_time(),
                     erlang:unique_integer()),
    rand_compat:uniform(10000) + 3000.
