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

-module(lsim_config).
-author("Vitor Enes Duarte <vitorenesduarte@gmail.com").

-include("lsim.hrl").

-export([node_number/0,
         simulation/0,
         simulation_identifier/0,
         simulation_timestamp/0,
         dcos/0,
         dcos_url/0,
         dcos_token/0]).

%% @doc Returns the node number.
-spec node_number() -> non_neg_integer().
node_number() ->
    {ok, NodeNumber} = application:get_env(?APP, lsim_node_number),
    NodeNumber.

%% @doc Returns the current simulation.
-spec simulation() -> atom().
simulation() ->
    {ok, Simulation} = application:get_env(?APP, lsim_simulation),
    Simulation.

%% @doc Returns the simulation identifier.
-spec simulation_identifier() -> atom().
simulation_identifier() ->
    {ok, SimulationIdentifier} =
        application:get_env(?APP, lsim_simulation_identifier),
    SimulationIdentifier.

%% @doc Returns the simulation timestamp.
-spec simulation_timestamp() -> atom().
simulation_timestamp() ->
    {ok, SimulationTimestamp} =
        application:get_env(?APP, lsim_simulation_timestamp),
    SimulationTimestamp.

%% @doc Returns true if running in DCOS.
-spec dcos() -> boolean().
dcos() ->
    dcos_url() /= "undefined".

%% @doc Returns the DCOS Url.
-spec dcos_url() -> string().
dcos_url() ->
    {ok, DCOSUrl} =
        application:get_env(?APP, lsim_dcos_url),
    DCOSUrl.

%% @doc Returns the DCOS Authentication Token.
-spec dcos_token() -> string().
dcos_token() ->
    {ok, DCOSToken} =
        application:get_env(?APP, lsim_dcos_token),
    DCOSToken.
