%%
%% Copyright (c) 2016 SyncFree Consortium.  All Rights Reserved.
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

-module(lsim_local_simulations_support).
-author("Vitor Enes Duarte <vitorenesduarte@gmail.com").

-include("lsim.hrl").

-export([run/1]).

run(Options) ->
    {IToNode, Nodes} = start(Options),
    construct_overlay(Options, IToNode),
    start_experiment(Nodes),
    wait_for_completion(Nodes),
    stop(IToNode).

%% @private
start_experiment(Nodes) ->
    %% wait for connectedness
    timer:sleep(5000),
    lists:foreach(
        fun(Node) ->
            ok = rpc:call(Node, lsim_simulation_runner, start, [])
        end,
        Nodes
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

    IToNode = lists:foldl(InitializerFun,
                          orddict:new(),
                          lists:seq(0, NodeNumber - 1)),
    Nodes = [Node || {_I, Node} <- IToNode],

    LoaderFun = fun(Node) ->
        %ct:pal("Loading lsim on node: ~p", [Node]),

        %% Load ldb
        ok = rpc:call(Node, application, load, [ldb]),

        %% Load lsim
        ok = rpc:call(Node, application, load, [?APP]),

        %% Set lager log dir
        PrivDir = code:priv_dir(?APP),
        NodeDir = filename:join([PrivDir, "lager", Node]),
        ok = rpc:call(Node,
                      application,
                      set_env,
                      [lager, log_root, NodeDir])
    end,
    lists:foreach(LoaderFun, Nodes),

    ConfigureFun = fun(Node) ->
        %ct:pal("Configuring node: ~p", [Node]),

        %% Configure lsim
        LSimSettings0 = proplists:get_value(lsim_settings, Options),
        LSimSettings1 = LSimSettings0
                     ++ [{lsim_timestamp, timestamp()}],

        lists:foreach(
            fun({Property, Value}) ->
                ok = rpc:call(Node,
                              lsim_config,
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
            LDBSettings
        )
    end,
    lists:foreach(ConfigureFun, Nodes),

    StartFun = fun(Node) ->
        {ok, _} = rpc:call(Node,
                           application,
                           ensure_all_started,
                           [?APP])
    end,
    lists:foreach(StartFun, Nodes),

    {IToNode, Nodes}.

%% @private Connect each node to its peers.
%%          If `Overlay' is hyparview, it's enough all peers
%%          connected to the same node.
%%          Otherwise use `lsim_overlay' to decide to which
%%          nodes a node should connect.
construct_overlay(Options, IToNode) ->
    Overlay = proplists:get_value(
        lsim_overlay,
        proplists:get_value(
            lsim_settings,
            Options
        )
    ),

    IToNodeSpec = lists:map(
        fun({I, Node}) ->
            Spec = rpc:call(Node, ldb_peer_service, myself, []),
            {I, Spec}
        end,
        IToNode
    ),

    NodeNumber = orddict:size(IToNode),

    %ct:pal("Nodes ~n~p~n", [IToNode]),
    %ct:pal("Nodes Spec ~n~p~n", [IToNodeSpec]),

    Graph = case Overlay of
        hyparview ->
            %% all nodes join the same node with I = 0
            [{I, [0]} || I <- lists:seq(1, NodeNumber - 1)];
        _ ->
            %% ensure symmetric views using `lsim_overlay'
            lsim_overlay:get(Overlay, NodeNumber)
    end,

    lists:foreach(
        fun({I, Peers}) ->
            Node = orddict:fetch(I, IToNode),
            %ct:pal("Node ~p~n~n", [Node]),

            lists:foreach(
                fun(Peer) ->
                    PeerSpec = orddict:fetch(Peer, IToNodeSpec),

                    %ct:pal("PeerSpec ~p~n~n", [PeerSpec]),

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
wait_for_completion(Nodes) ->
    ct:pal("Waiting for simulation to end"),

    NodeNumber = length(Nodes),

    Result = wait_until(
        fun() ->
            Ended = lists:foldl(
                fun(Node, Acc) ->
                    SimulationEnd = rpc:call(Node,
                                             lsim_config,
                                             get,
                                             [lsim_simulation_end,
                                              false]),

                    case SimulationEnd of
                        true ->
                            Acc + 1;
                        false ->
                            Acc
                    end
                end,
                0,
                Nodes
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
stop(IToNode) ->
    StopFun = fun({I, _Node}) ->
        Name = get_node_name(I),
        case ct_slave:stop(Name, [{stop_timeout, 10}]) of
            {ok, _} ->
                ok;
            Error ->
                ct:fail(Error)
        end
    end,
    lists:foreach(StopFun, IToNode).

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
    {Mega, Sec, Micro} = erlang:timestamp(),
    ME = 1000000000000000,
    SE = 1000000000,
    MiE = 1000,
    Mega * ME + Sec * SE + Micro * MiE.

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
