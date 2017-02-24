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
    LDBSpecs = ldb_specs(),
    LSimSpecs = lsim_specs(),
    SimSpecs = sim_specs(),

    Children = LDBSpecs ++
               LSimSpecs ++
               SimSpecs,

    ldb_log:info("lsim_sup initialized!"),
    RestartStrategy = {one_for_one, 10, 10},
    {ok, {RestartStrategy, Children}}.

%% @private
configure_peer_service() ->
    %% configure lsim overlay
    Overlay = lsim_configure_var("LSIM_OVERLAY",
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
