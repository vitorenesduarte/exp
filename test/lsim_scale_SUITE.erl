%% -------------------------------------------------------------------
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
%%

-module(lsim_scale_SUITE).
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

-define(NODE_NUMBER, 5).
-define(EVENT_NUMBER, 10).

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
     gcounter_test
    ].

%% ===================================================================
%% tests
%% ===================================================================

gcounter_test(_Config) ->
    Overlay = group,
    Simulation = group,
    Mode = state_based,
    JoinDecompositions = false,

    Options = [{node_number, ?NODE_NUMBER},
               {lsim_settings,
                [{lsim_overlay, Overlay},
                 {lsim_simulation, Simulation},
                 {lsim_node_number, ?NODE_NUMBER},
                 {lsim_node_event_number, ?EVENT_NUMBER}]},
               {ldb_settings,
                [{ldb_mode, Mode},
                 {ldb_join_decompositions, JoinDecompositions},
                 {ldb_extended_logging, true}]}],

    lsim_local_simulations_support:run(Options).
