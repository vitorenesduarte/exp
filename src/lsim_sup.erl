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

-module(lsim_sup).
-author("Vitor Enes Duarte <vitorenesduarte@gmail.com").

-include("lsim.hrl").

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    %% Start LDB
    {ok, _} = ldb_sup:start_link(),

    %% Configure node number
    configure_int(lsim_node_number,
                  "NODE_NUMBER",
                  "1"),

    %% Configure DCOS url
    configure_str(lsim_dcos_url,
                  "DCOS",
                  "undefined"),

    %% If running in DCOS, create overlay
    case lsim_config:dcos() of
        true ->
            %% Configure DCOS token
            configure_str(lsim_dcos_token,
                          "TOKEN",
                          "undefined"),

            %% Configure overlay
            Overlay = configure_var(lsim_overlay,
                                    "OVERLAY",
                                    "undefined"),
            lsim_dcos:create_overlay(Overlay);
        false ->
            ok
    end,

    %% Configure simulation
    Simulation = configure_var(lsim_simulation,
                               "SIMULATION",
                               "undefined"),
    case Simulation of
        undefined ->
            ok
    end,

    %% Configure simulation identifier
    configure_var(lsim_simulation_identifier,
                  "SIMULATION_IDENTIFIER",
                  "undefined"),

    %% Configure simulation timestamp
    configure_var(lsim_simulation_timestamp,
                  "SIMULATION_TIMESTAMP",
                  "undefined"),

    %% Start instrumentation
    {ok, _} = lsim_instrumentation:start_link(),

    ldb_log:info("lsim_sup initialized!"),
    RestartStrategy = {one_for_one, 10, 10},
    {ok, {RestartStrategy, []}}.


%% @private
configure(LDBVariable, EnvironmentVariable, EnvironmentDefault, ParseFun) ->
    Default = ParseFun(
        os:getenv(EnvironmentVariable, EnvironmentDefault)
    ),
    Value = application:get_env(?APP,
                                LDBVariable,
                                Default),
    application:set_env(?APP,
                        LDBVariable,
                        Value),
    Value.
configure_var(LDBVariable, EnvironmentVariable, EnvironmentDefault) ->
    configure(LDBVariable, EnvironmentVariable, EnvironmentDefault, fun(V) -> list_to_atom(V) end).
configure_str(LDBVariable, EnvironmentVariable, EnvironmentDefault) ->
    configure(LDBVariable, EnvironmentVariable, EnvironmentDefault, fun(V) -> V end).
configure_int(LDBVariable, EnvironmentVariable, EnvironmentDefault) ->
    configure(LDBVariable, EnvironmentVariable, EnvironmentDefault, fun(V) -> list_to_integer(V) end).
