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
         push_ldb_metrics/0]).

-define(LDB_METRICS, ldb_metrics).
-define(STORE, lsim_metrics_store).
-define(SEP, ",").

-spec push_lsim_metrics(timestamp()) -> ok.
push_lsim_metrics(StartTime) ->
    LDBVars = [ldb_mode,
               ldb_join_decompositions],
    LDBConfigs = get_configs(ldb, LDBVars),

    LSimVars = [lsim_overlay,
                lsim_node_number,
                lsim_simulation,
                lsim_node_event_number],
    LSimConfigs = get_configs(lsim, LSimVars),

    All = [{start_time, StartTime}]
       ++ LDBConfigs
       ++ LSimConfigs,

    FilePath = file_path(rsg),
    Header = csv_line(["config", "value"]),
    File = lists:foldl(
        fun({K, V}, Acc) ->
            List = [str(K),
                    str(V)],
            Acc ++ csv_line(List)
        end,
        "",
        All
    ),

    store(FilePath, Header, File),
    ok.

-spec push_ldb_metrics() -> ok.
push_ldb_metrics() ->
    TimeSeries = ?LDB_METRICS:get_time_series(),

    PerMessageType = lists:foldl(
        fun({Timestamp, message, Metrics}, Acc0) ->
            lists:foldl(
                fun({MessageType, Size}, Acc1) ->
                    orddict:append(MessageType, {Timestamp, Size}, Acc1)
                end,
                Acc0,
                Metrics
            )
        end,
        orddict:new(),
        TimeSeries
    ),

    FilePath = file_path(ldb_config:id()),
    Header = csv_line(["timestamp", "type", "size"]),
    File = orddict:fold(
        fun(MessageType, Metrics, Acc0) ->
            lists:foldl(
                fun({Timestamp, Size}, Acc1) ->
                    List = [str(Timestamp),
                            str(MessageType),
                            str(Size)],
                    Acc1 ++ csv_line(List)
                end,
                Acc0,
                Metrics
            )
        end,
        "",
        PerMessageType
    ),

    store(FilePath, Header, File),
    ok.

%% @private
file_path(Name) ->
    Timestamp = lsim_config:get(lsim_timestamp),
    Filename = str(Timestamp) ++ "/"
            ++ str(Name) ++ ".csv",
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
csv_line(List) ->
    lists:flatten(lists:join(?SEP, List)) ++ "\n".

%% @private
store(FilePath, Header, File) ->
    Binary = list_to_binary(Header ++ File),
    ok = ?STORE:put(FilePath, Binary).
