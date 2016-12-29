%% -------------------------------------------------------------------
%%
%% Copyright (c) 2016 SyncFree Consortium.  All Rights Reserved.
%% Copyright (c) 2016 Christopher Meiklejohn.  All Rights Reserved.
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
%%

-module(lsim_basic_simulation_SUITE).
-author("Vitor Enes Duarte <vitorenesduarte@gmail.com>").

%% common_test callbacks
-export([%% suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         all/0]).

%% tests
-compile([export_all]).

-include("lsim.hrl").

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/inet.hrl").

-define(NODE_NUMBER, 13).

%% ===================================================================
%% common_test callbacks
%% ===================================================================

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

init_per_testcase(Case, Config) ->
    ct:pal("Beginning test case: ~p", [Case]),
    Config.

end_per_testcase(Case, Config) ->
    ct:pal("Ending test case: ~p", [Case]),
    Config.

all() ->
    [
     state_based_static_test,
     state_based_partisan_test,
     delta_based_static_test,
     delta_based_partisan_test,
     join_decompositions_static_test,
     join_decompositions_partisan_test,
     pure_op_based_static_test,
     pure_op_based_partisan_test
    ].

%% ===================================================================
%% tests
%% ===================================================================

state_based_static_test(_Config) ->
    run(state_based, line).

state_based_partisan_test(_Config) ->
    run(state_based, hyparview).

delta_based_static_test(_Config) ->
    run(delta_based, line).

delta_based_partisan_test(_Config) ->
    run(delta_based, hyparview).

join_decompositions_static_test(_Config) ->
    run(join_decompositions, line).

join_decompositions_partisan_test(_Config) ->
    run(join_decompositions, hyparview).

pure_op_based_static_test(_Config) ->
    run(pure_op_based, line).

pure_op_based_partisan_test(_Config) ->
    run(pure_op_based, hyparview).

%% @private
run(Evaluation, Overlay) ->
    lists:foreach(
        fun(DT) ->
            PeerService = get_peer_service(Overlay),
            {Mode, JoinDecompositions} = get_mode_and_join_decompositions(Evaluation),

            Options = [{node_number, ?NODE_NUMBER},
                       {overlay, Overlay},
                       {lsim_settings,
                        [{lsim_peer_service, PeerService},
                         {lsim_simulation, basic},
                         {lsim_node_number, ?NODE_NUMBER}]},
                       {ldb_settings,
                        [{ldb_mode, Mode},
                         {ldb_join_decompositions, JoinDecompositions},
                         {ldb_extended_logging, true}]}],

            lsim_local_simulations_support:run(Options)
        end,
        [gcounter, gset]
    ).

%% @private
get_peer_service(hyparview) ->
    partisan_hyparview_peer_service_manager;
get_peer_service(_StaticOverlay) ->
    lsim_static_peer_service.

%% @private
get_mode_and_join_decompositions(state_based) ->
    {state_based, false};
get_mode_and_join_decompositions(delta_based) ->
    {delta_based, false};
get_mode_and_join_decompositions(join_decompositions) ->
    {delta_based, true};
get_mode_and_join_decompositions(pure_op_based) ->
    {pure_op_based, false}.
