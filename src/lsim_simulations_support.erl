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

-export([push_metrics/0,
         push_metrics/1]).

-define(LDB_METRICS, ldb_metrics).
-define(STORE, lsim_metrics_store).

-spec push_metrics() -> ok.
push_metrics() ->
    push_metrics([]).

-spec push_metrics(list()) -> ok.
push_metrics(LSimTS) ->
    LDBTS = ?LDB_METRICS:get_time_series(),
    ?LOG("LSIM ~p~n~nLDB ~p~n~n", [LSimTS, LDBTS]),
    ok.
