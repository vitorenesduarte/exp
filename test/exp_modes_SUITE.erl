%%
%% Copyright (c) 2018 Vitor Enes.  All Rights Reserved.
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

-module(exp_modes_SUITE).
-author("Vitor Enes <vitorenesduarte@gmail.com>").

%% common_test callbacks
-export([%% suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         all/0]).

%% tests
-compile([export_all, nowarn_export_all]).

-include("exp.hrl").

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/inet.hrl").

-define(NODE_NUMBER, 3).
-define(EVENT_NUMBER, 5).
-define(SIMULATION, gcounter).

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
     state_based_ring_test,
     delta_based_ring_test,
     delta_based_revisited_ring_test,
     scuttlebutt_ring_test,
     scuttlebutt_gc_ring_test
    ].

%% ===================================================================
%% tests
%% ===================================================================

state_based_ring_test(_Config) ->
    run(state_based, ring).

delta_based_ring_test(_Config) ->
    run(delta_based, ring).

delta_based_revisited_ring_test(_Config) ->
    run(delta_based_revisited, ring).

scuttlebutt_ring_test(_Config) ->
    run(scuttlebutt, ring).

scuttlebutt_gc_ring_test(_Config) ->
    run(scuttlebutt_gc, ring).

%% @private
run(Evaluation, Overlay) ->
    {Mode, Redundant, BackPropagation, GC} = get_config(Evaluation),

    Options = [{node_number, ?NODE_NUMBER},
               {exp_settings,
                [{exp_overlay, Overlay},
                 {exp_simulation, ?SIMULATION},
                 {exp_node_number, ?NODE_NUMBER},
                 {exp_node_event_number, ?EVENT_NUMBER}]},
               {ldb_settings,
                [{ldb_mode, Mode},
                 {ldb_redundant_dgroups, Redundant},
                 {ldb_dgroup_back_propagation, BackPropagation},
                 {ldb_scuttlebutt_gc, GC}]}],

    exp_local_simulations_support:run(Options).

%% @private
get_config(state_based) ->
    {state_based, false, false, false};
get_config(delta_based) ->
    {delta_based, false, false, false};
get_config(delta_based_revisited) ->
    {delta_based, true, true, false};
get_config(scuttlebutt) ->
    {scuttlebutt, false, false, false};
get_config(scuttlebutt_gc) ->
    {scuttlebutt, false, false, true}.
