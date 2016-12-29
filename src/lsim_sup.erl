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
    PeerServiceSpecs = peer_service_specs(),
    LDBSpecs = ldb_specs(),
    LSimSpecs = lsim_specs(),
    SimSpecs = sim_specs(),
    Children = PeerServiceSpecs ++
               LDBSpecs ++
               LSimSpecs ++
               SimSpecs,

    ldb_log:info("lsim_sup initialized!"),
    RestartStrategy = {one_for_one, 10, 10},
    {ok, {RestartStrategy, Children}}.

%% @private
%% os env vars override possible application env vars
peer_service_specs() ->
    %% configure lsim overlay
    Overlay = lsim_configure_var("LSIM_OVERLAY",
                                 lsim_overlay,
                                 ?DEFAULT_OVERLAY),

    %% get ip and port
    {Ip, Port} = ip_and_port(),

    case Overlay of
        hyparview ->
            %% configure ldb peer service
            PeerService = partisan_hyparview_peer_service_manager,
            ldb_config:set(ldb_peer_service, PeerService),

            %% configure partisan manager, ip and port
            partisan_config:set(partisan_peer_service_manager,
                                PeerService),
            partisan_config:set(peer_ip, Ip),
            partisan_config:set(peer_port, Port),

            %% specs
            [{partisan_sup,
              {partisan_sup, start_link, []},
              permanent, infinity, supervisor, [partisan_sup]}];

				_ ->
            %% configure ldb peer service
            PeerService = lsim_static_peer_service,
            ldb_config:set(ldb_peer_service, PeerService),

            %% configure lsim ip and port
            lsim_config:set(lsim_peer_ip, Ip),
            lsim_config:set(lsim_peer_port, Port),

            %% specs
            [{PeerService,
              {PeerService, start_link, []},
              permanent, 5000, worker, [PeerService]}]
    end.

%% @private
%% os env vars override possible application env vars
ldb_specs() ->
    %% configure ldb mode
    ldb_configure_var("LDB_MODE",
                      ldb_mode,
                      ?DEFAULT_MODE),

    %% configure join decompositions
    ldb_configure_var("LDB_JOIN_DECOMPOSITIONS",
                      ldb_join_decompositions,
                      false),

    %% specs
    [{ldb_sup,
      {ldb_sup, start_link, []},
      permanent, infinity, supervisor, [ldb_sup]}].

%% @private
%% os env vars override possible application env vars
lsim_specs() ->
    [{lsim_intrumentation,
      {lsim_instrumentation, start_link, []},
      permanent, 5000, worker, [lsim_instrumentation]}].

%% @private
%% os env vars override possible application env vars
sim_specs() ->
    %% configure lsim simulation
    Simulation = lsim_configure_var("LSIM_SIMULATION",
                                    lsim_simulation,
                                    undefined),

    %% configure node number
    lsim_configure_int("LSIM_NODE_NUMBER",
                       lsim_node_number,
                       1),

    %% configure node event number
    lsim_configure_int("LSIM_NODE_EVENT_NUMBER",
                       lsim_node_event_number,
                       30),

    %% configure unique simulation timestamp
    lsim_configure_int("LSIM_SIMULATION_TS",
                       lsim_simulation_ts,
                       0),

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

%% @private
ldb_configure_var(Env, Var, Default) ->
    configure_var(ldb, Env, Var, Default).

%% @private
lsim_configure_var(Env, Var, Default) ->
    configure_var(lsim, Env, Var, Default).

%% @private
lsim_configure_int(Env, Var, Default) ->
    configure_int(lsim, Env, Var, Default).

%% @private
configure_var(App, Env, Var, Default) ->
    To = fun(V) -> atom_to_list(V) end,
    From = fun(V) -> list_to_atom(V) end,
    configure(App, Env, Var, Default, To, From).

%% @private
configure_int(App, Env, Var, Default) ->
    To = fun(V) -> integer_to_list(V) end,
    From = fun(V) -> list_to_integer(V) end,
    configure(App, Env, Var, Default, To, From).

%% @private
configure(App, Env, Var, Default, To, From) ->
    Current = case App of
        ldb ->
            ldb_config:get(Var, Default);
        lsim ->
            lsim_config:get(Var, Default)
    end,

    Val = From(
        os:getenv(Env, To(Current))
    ),

    case App of
        ldb ->
            ldb_config:set(Var, Val);
        lsim ->
            lsim_config:set(Var, Val)
    end,

    Val.
