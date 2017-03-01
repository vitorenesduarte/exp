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
    configure_ldb(),
    Simulation = configure_lsim(),

    Children = lsim_specs(Simulation),

    ?LOG("lsim_sup initialized!"),
    RestartStrategy = {one_for_one, 10, 10},
    {ok, {RestartStrategy, Children}}.

%% @private
configure_peer_service() ->
    %% configure lsim overlay
    Overlay = lsim_configure_var("OVERLAY",
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
configure_ldb() ->
    %% configure ldb mode
    ldb_configure_var("LDB_MODE",
                      ldb_mode,
                      ?DEFAULT_MODE),

    %% configure join decompositions
    ldb_configure_var("LDB_JOIN_DECOMPOSITIONS",
                      ldb_join_decompositions,
                      false).

%% @private
configure_lsim() ->
    %% configure lsim simulation
    Simulation = lsim_configure_var("SIMULATION",
                                    lsim_simulation,
                                    undefined),

    %% configure node number
    lsim_configure_int("NODE_NUMBER",
                       lsim_node_number,
                       1),

    %% configure node event number
    lsim_configure_int("NODE_EVENT_NUMBER",
                       lsim_node_event_number,
                       30),

    %% configure unique simulation timestamp
    lsim_configure_int("TIMESTAMP",
                       lsim_timestamp,
                       0),

    %% configure api server
    lsim_configure_var("APISERVER",
                       lsim_api_server,
                       undefined),

    %% configure auth token
    lsim_configure_var("TOKEN",
                       lsim_token,
                       undefined),

    %% configure orchestration
    lsim_configure_var("ORCHESTRATION",
                       lsim_orchestration,
                       undefined),

    Simulation.

%% @private
lsim_specs(Simulation) ->
    InstrumentationSpecs = [{lsim_intrumentation,
                             {lsim_instrumentation, start_link, []},
                             permanent, 5000, worker,
                             [lsim_instrumentation]}],
    SimulationSpecs = lsim_simulations:get_specs(Simulation),

    InstrumentationSpecs ++ SimulationSpecs.

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
