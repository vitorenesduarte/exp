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

-spec push_lsim_metrics(timestamp()) -> ok.
push_lsim_metrics(StartTime) ->
    FilePath = file_path(rsg),
    File = "start," ++ integer_to_list(StartTime),
    store(FilePath, File),
    ok.

-spec push_ldb_metrics() -> ok.
push_ldb_metrics() ->
    FilePath = file_path(ldb_config:id()),
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

    File = orddict:fold(
        fun(MessageType, Metrics, Acc0) ->
            lists:foldl(
                fun({Timestamp, Size}, Acc1) ->
                    Acc1 ++ integer_to_list(Timestamp) ++ ","
                         ++ atom_to_list(MessageType) ++ ","
                         ++ integer_to_list(Size)
                         ++ "\n"
                end,
                Acc0,
                Metrics
            )
        end,
        "",
        PerMessageType
    ),

    store(FilePath, File),
    ok.

%% @private
file_path(Name) ->
    Simulation = lsim_config:get(lsim_simulation),
    Timestamp = lsim_config:get(lsim_timestamp),
    Filename = atom_to_list(Simulation) ++ "/" 
            ++ integer_to_list(Timestamp) ++ "/"
            ++ atom_to_list(Name) ++ ".csv",
    Filename.

%% @private
store(FilePath, File) ->
    lager:info("FilePath ~p~n | File ~p~n", [File, FilePath]),
    Binary = list_to_binary(File),
    ok = ?STORE:put(FilePath, Binary).
