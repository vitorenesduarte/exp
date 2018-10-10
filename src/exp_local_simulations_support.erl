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

-module(exp_local_simulations_support).
-author("Vitor Enes <vitorenesduarte@gmail.com").

-include("exp.hrl").

-export([run/1]).

run(Options) ->
    IdToNode = start(Options),
    construct_overlay(Options, IdToNode),
    start_experiment(IdToNode),
    wait_for_completion(IdToNode),
    stop(IdToNode).

%% @private
start_experiment(IdToNode) ->
    %% wait for connectedness
    timer:sleep(5000),
    lists:foreach(
        fun({_Id, Node}) ->
            ok = rpc:call(Node, exp_simulation_runner, start_simulation, [])
        end,
        IdToNode
    ).

%% @private Start nodes.
start(Options) ->
    ok = start_erlang_distribution(),
    NodeNumber = proplists:get_value(node_number, Options),

    InitializerFun = fun(I, Acc) ->
        %ct:pal("Starting node: ~p", [I]),

        %% Start node
        Config = [{monitor_master, true},
                  {startup_functions, [{code, set_path, [codepath()]}]}],

        Name = get_node_name(I),
        case ct_slave:start(Name, Config) of
            {ok, Node} ->
                orddict:store(I, Node, Acc);
            Error ->
                ct:fail(Error)
        end
    end,

    IdToNode = lists:foldl(InitializerFun,
                           orddict:new(),
                           lists:seq(0, NodeNumber - 1)),

    LoaderFun = fun({_Id, Node}) ->
        %% Load ldb
        ok = rpc:call(Node, application, load, [ldb]),

        %% Load exp
        ok = rpc:call(Node, application, load, [?APP]),

        %% Set lager log dir
        PrivDir = code:priv_dir(?APP),
        NodeDir = filename:join([PrivDir, "lager", Node]),
        ok = rpc:call(Node,
                      application,
                      set_env,
                      [lager, log_root, NodeDir])
    end,
    lists:foreach(LoaderFun, IdToNode),

    ConfigureFun = fun({Id, Node}) ->
        %% Configure exp
        LSimSettings0 = proplists:get_value(exp_settings, Options),
        LSimSettings1 = LSimSettings0
                     ++ [{exp_timestamp, timestamp()},
                         {exp_numerical_id, Id}],

        lists:foreach(
            fun({Property, Value}) ->
                ok = rpc:call(Node,
                              exp_config,
                              set,
                              [Property, Value])
            end,
            LSimSettings1
        ),

        %% Configure ldb
        LDBSettings = proplists:get_value(ldb_settings, Options),
        lists:foreach(
            fun({Property, Value}) ->
                ok = rpc:call(Node,
                              ldb_config,
                              set,
                              [Property, Value])
            end,
            [{node_number, NodeNumber} | LDBSettings]
        )
    end,
    lists:foreach(ConfigureFun, IdToNode),

    StartFun = fun({_Id, Node}) ->
        {ok, _} = rpc:call(Node,
                           application,
                           ensure_all_started,
                           [?APP])
    end,
    lists:foreach(StartFun, IdToNode),

    IdToNode.

%% @private Connect each node to its peers.
construct_overlay(Options, IdToNode) ->
    Overlay = proplists:get_value(
        exp_overlay,
        proplists:get_value(
            exp_settings,
            Options
        )
    ),

    IdToNodeSpec = lists:map(
        fun({Id, Node}) ->
            Spec = rpc:call(Node, ldb_peer_service, myself, []),
            {Id, Spec}
        end,
        IdToNode
    ),

    NodeNumber = orddict:size(IdToNode),
    Graph = exp_overlay:get(Overlay, NodeNumber),

    lists:foreach(
        fun({I, Peers}) ->
            Node = orddict:fetch(I, IdToNode),

            lists:foreach(
                fun(Peer) ->
                    PeerSpec = orddict:fetch(Peer, IdToNodeSpec),

                    ok = rpc:call(Node,
                                  ldb_peer_service,
                                  join,
                                  [PeerSpec])
                end,
                Peers
            )
        end,
        Graph
    ).

%% @private Poll nodes to see if simulation is ended.
wait_for_completion(IdToNode) ->
    ct:pal("Waiting for simulation to end"),

    NodeNumber = length(IdToNode),

    Result = wait_until(
        fun() ->
            Ended = lists:foldl(
                fun({_Id, Node}, Acc) ->
                    SimulationEnd = rpc:call(Node,
                                             exp_config,
                                             get,
                                             [exp_simulation_end,
                                              false],
                                             infinity),

                    case SimulationEnd of
                        true ->
                            Acc + 1;
                        false ->
                            Acc
                    end
                end,
                0,
                IdToNode
            ),

            %ct:pal("~p of ~p with simulation as true", [Ended, NodeNumber]),

            Ended == NodeNumber
        end,
        100,      %% 100 retries
        10 * 1000 %% every 10 seconds
    ),

    case Result of
        ok ->
            ct:pal("Simulation ended with success");
        fail ->
            ct:fail("Simulation failed")
    end.

%% @private Stop nodes.
stop(IdToNode) ->
    StopFun = fun({I, _Node}) ->
        Name = get_node_name(I),
        case ct_slave:stop(Name) of
            {ok, _} ->
                ok;
            Error ->
                ct:fail(Error)
        end
    end,
    lists:foreach(StopFun, IdToNode).

%% @private Start erlang distribution.
start_erlang_distribution() ->
    os:cmd(os:find_executable("epmd") ++ " -daemon"),
    {ok, Hostname} = inet:gethostname(),
    case net_kernel:start([list_to_atom("runner@" ++ Hostname), shortnames]) of
        {ok, _} ->
            ok;
        {error, {already_started, _}} ->
            ok
    end.

%% @private
codepath() ->
    lists:filter(fun filelib:is_dir/1, code:get_path()).

%% @private
get_node_name(I) ->
    list_to_atom("n" ++ integer_to_list(I)).

%% @private
timestamp() ->
    erlang:system_time(microsecond).

%% @doc Wait until `Fun' returns true or `Retry' reaches 0.
%%      The sleep time between retries is `Delay'.
wait_until(_Fun, 0, _Delay) ->
    fail;
wait_until(Fun, Retry, Delay) when Retry > 0 ->
    case Fun() of
        true ->
            ok;
        _ ->
            timer:sleep(Delay),
            wait_until(Fun, Retry - 1, Delay)
    end.
