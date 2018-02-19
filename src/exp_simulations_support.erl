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

-module(exp_simulations_support).
-author("Vitor Enes <vitorenesduarte@gmail.com").

-include("exp.hrl").

-export([push_exp_metrics/1,
         push_ldb_metrics/0]).

-define(LDB_METRICS, ldb_metrics).
-define(STORE, exp_metrics_store).
-define(SEP, ",").

-spec push_exp_metrics(timestamp()) -> ok.
push_exp_metrics(StartTime) ->
    LDBVars = [ldb_mode,
               ldb_driven_mode,
               ldb_state_sync_interval,
               ldb_redundant_dgroups,
               ldb_dgroup_back_propagation],
    LDBConfigs = get_configs(ldb, LDBVars),

    LSimVars = [exp_overlay,
                exp_node_number,
                exp_simulation,
                exp_node_event_number,
                exp_break_links,
                exp_gmap_simulation_key_percentage],
    LSimConfigs = get_configs(exp, LSimVars),

    All = [{start_time, StartTime}]
       ++ LDBConfigs
       ++ LSimConfigs,

    FilePath = file_path(rsg),
    File = ldb_json:encode(All),

    store(FilePath, File),
    ok.

-spec push_ldb_metrics() -> ok.
push_ldb_metrics() ->
    TimeSeries = ?LDB_METRICS:get_time_series(),
    Latency = ?LDB_METRICS:get_latency(),
    TransmissionTS = filter_by_ts_class(transmission, TimeSeries),
    MemoryTS = filter_by_ts_class(memory, TimeSeries),

    %% process transmission
    All0 = lists:foldl(
        fun({Timestamp, transmission, {MSize, PSize}}, Acc0) ->
            V = [{ts, Timestamp},
                 {size, [MSize, PSize]}],
            orddict:append(transmission, V, Acc0)
        end,
        orddict:new(),
        TransmissionTS
    ),

    %% process memory
    All1 = lists:foldl(
        fun({Timestamp, memory, {{MCRDTSize, PCRDTSize}, {MRestSize, PRestSize}}}, Acc0) ->
            V = [{ts, Timestamp},
                 {size, [MCRDTSize, PCRDTSize, MRestSize, PRestSize]}],
            orddict:append(memory, V, Acc0)
        end,
        All0,
        MemoryTS
    ),

    %% process latency
    All2 = orddict:store(latency, Latency, All1),

    FilePath = file_path(ldb_config:id()),
    File = ldb_json:encode(All2),

    store(FilePath, File),
    ok.

%% @private
filter_by_ts_class(Class, TS) ->
    lists:filter(
        fun({_, MClass, _}) ->
                MClass == Class
        end,
        TS
    ).

%% @private
file_path(Name) ->
    Timestamp = exp_config:get(exp_timestamp),
    Filename = str(Timestamp) ++ "/"
            ++ str(Name) ++ ".json",
    Filename.

%% @private
get_configs(App, Vars) ->
    lists:map(
        fun(Var) ->
            Mod = case App of
                ldb ->
                    ldb_config;
                exp ->
                    exp_config
            end,
            {Var, Mod:get(Var)}
        end,
        Vars
    ).

%% @private
str(V) when is_atom(V) ->
    atom_to_list(V);
str(V) when is_integer(V) ->
    integer_to_list(V).

%% @private
store(FilePath, File) ->
    ok = ?STORE:put(FilePath, File).
