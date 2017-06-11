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

-module(lsim_driven_based_modes_SUITE).
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

-define(NODE_NUMBER, 3).
-define(EVENT_NUMBER, 50).
-define(SIMULATION, awset).
-define(OVERLAY, line).

%% ===================================================================
%% common_test callbacks
%% ===================================================================

suite() ->
    [{timetrap, {hours, 1}}].

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
     state_based_state_driven_test,
     state_based_digest_driven_test,
     delta_based_state_driven_test,
     delta_based_digest_driven_test,
     delta_based_revisited_state_driven_test,
     delta_based_revisited_digest_driven_test
    ].

%% ===================================================================
%% tests
%% ===================================================================

state_based_state_driven_test(_Config) ->
    run(state_based_state_driven).

state_based_digest_driven_test(_Config) ->
    run(state_based_digest_driven).

delta_based_state_driven_test(_Config) ->
    run(delta_based_state_driven).

delta_based_digest_driven_test(_Config) ->
    run(delta_based_digest_driven).

delta_based_revisited_state_driven_test(_Config) ->
    run(delta_based_revisited_state_driven).

delta_based_revisited_digest_driven_test(_Config) ->
    run(delta_based_revisited_digest_driven).

%% @private
run(Evaluation) ->
    {Mode, Redundant, BackPropagation, DrivenMode} = get_config(Evaluation),

    Options = [{node_number, ?NODE_NUMBER},
               {lsim_settings,
                [{lsim_overlay, ?OVERLAY},
                 {lsim_simulation, ?SIMULATION},
                 {lsim_node_number, ?NODE_NUMBER},
                 {lsim_node_event_number, ?EVENT_NUMBER}]},
               {ldb_settings,
                [{ldb_mode, Mode},
                 {ldb_driven_mode, DrivenMode},
                 {ldb_redundant_dgroups, Redundant},
                 {ldb_dgroup_back_propagation, BackPropagation}]}],

    lsim_local_simulations_support:run(Options).

%% @private
get_config(state_based_state_driven) ->
    {state_based, false, false, state_driven};
get_config(state_based_digest_driven) ->
    {state_based, false, false, digest_driven};
get_config(delta_based_state_driven) ->
    {delta_based, false, false, state_driven};
get_config(delta_based_digest_driven) ->
    {delta_based, false, false, digest_driven};
get_config(delta_based_revisited_state_driven) ->
    {delta_based, true, true, state_driven};
get_config(delta_based_revisited_digest_driven) ->
    {delta_based, true, true, digest_driven}.
