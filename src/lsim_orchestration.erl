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

-module(lsim_orchestration).
-author("Vitor Enes Duarte <vitorenesduarte@gmail.com").

-include("lsim.hrl").

-export([rsg/1,
         nodes/1,
         stop/0]).

%% @doc Returns the specs of the rsg master, given a port.
-callback rsg(node_port()) ->
    {ok, node_spec()} | {error, not_connected}.

%% @doc Returns the specs of the running nodes, given a port.
-callback nodes(node_port()) ->
    [node_spec()].

-spec rsg(node_port()) ->
    {ok, node_spec()} | {error, not_connected}.
rsg(Port) ->
    do(rsg, [Port]).

-spec nodes(node_port()) ->
    [node_spec()].
nodes(Port) ->
    do(nodes, [Port]).

-spec stop() ->
    ok.
stop() ->
    do(stop, []).

%% @private
do(Function, Args) ->
    Orchestration = lsim_config:get(lsim_orchestration),
    case Orchestration of
        kubernetes ->
            erlang:apply(lsim_kube_orchestration, Function, Args)
    end.
