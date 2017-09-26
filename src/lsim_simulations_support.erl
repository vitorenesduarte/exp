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

-module(lsim_simulations_support).
-author("Vitor Enes Duarte <vitorenesduarte@gmail.com").

-include("lsim.hrl").

-export([push_lsim_metrics/1,
         push_lmetrics/0]).

-define(LMETRICS, lmetrics).
-define(STORE, lsim_metrics_store).
-define(SEP, ",").

-spec push_lsim_metrics(timestamp()) -> ok.
push_lsim_metrics(StartTime) ->
    LDBVars = [ldb_mode,
               ldb_driven_mode,
               ldb_state_sync_interval,
               ldb_redundant_dgroups,
               ldb_dgroup_back_propagation],
    LDBConfigs = get_configs(ldb, LDBVars),

    LSimVars = [lsim_overlay,
                lsim_node_number,
                lsim_simulation,
                lsim_node_event_number,
                lsim_element_node_ratio,
                lsim_partition_number],
    LSimConfigs = get_configs(lsim, LSimVars),

    All = [{start_time, StartTime}]
       ++ LDBConfigs
       ++ LSimConfigs,

    FilePath = file_path(rsg),
    File = encode(All),

    store(FilePath, File),
    ok.

-spec push_lmetrics() -> ok.
push_lmetrics() ->
    TimeSeries = ?LMETRICS:get_time_series(),
    Latency = ?LMETRICS:get_latency(),
    TransmissionTS = filter_by_ts_class(transmission, TimeSeries),
    MemoryTS = filter_by_ts_class(memory, TimeSeries),

    %% process transmission
    PerMessageType = lists:foldl(
        fun({Timestamp, transmission, {MessageType, Size}}, Acc0) ->
            orddict:append(MessageType, {Timestamp, Size}, Acc0)
        end,
        orddict:new(),
        TransmissionTS
    ),

    All0 = orddict:fold(
        fun(MessageType, Metrics, Acc0) ->
            lists:foldl(
                fun({Timestamp, Size}, Acc1) ->
                    V = [{ts, Timestamp},
                         {size, [Size]}],
                    orddict:append(MessageType, V, Acc1)
                end,
                Acc0,
                Metrics
            )
        end,
        orddict:new(),
        PerMessageType
    ),

    %% process memory
    All1 = lists:foldl(
        fun({Timestamp, memory, {CRDTSize, RestSize}}, Acc0) ->
            V = [{ts, Timestamp},
                 {size, [CRDTSize, RestSize]}],
            orddict:append(memory, V, Acc0)
        end,
        All0,
        MemoryTS
    ),

    %% process latency
    All2 = orddict:store(latency, Latency, All1),

    FilePath = file_path(node()),
    File = encode(All2),

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
    Timestamp = lsim_config:get(lsim_timestamp),
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
                lsim ->
                    lsim_config
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

%% @doc
encode(D) ->
    jsx:encode(D).
