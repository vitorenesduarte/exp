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

-module(exp_rsg).
-author("Vitor Enes <vitorenesduarte@gmail.com").

-include("exp.hrl").

-behaviour(gen_server).

%% exp_rsg callbacks
-export([start_link/0,
         simulation_end/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {node_number :: non_neg_integer(),
                break_links_specs :: [node_spec()] | undefined,
                partisan_manager :: atom()}).

-define(BARRIER_PEER_SERVICE, exp_barrier_peer_service).
-define(INTERVAL, 3000).

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec simulation_end() -> ok.
simulation_end() ->
    gen_server:call(?MODULE, simulation_end, infinity).

%% gen_server callbacks
init([]) ->
    schedule_create_barrier(),

    NodeNumber = exp_config:get(exp_node_number),
    Manager = partisan_config:get(partisan_peer_service_manager),

    lager:info("exp_rsg initialized"),
    {ok, #state{node_number=NodeNumber,
                break_links_specs=undefined,
                partisan_manager=Manager}}.

handle_call(simulation_end, _From, State) ->
    tell({sim_done, ldb_config:id()}),
    {reply, ok, State};

handle_call(Msg, _From, State) ->
    lager:warning("Unhandled call message: ~p", [Msg]),
    {noreply, State}.

handle_cast(sim_go, State) ->
    lager:info("Received SIM GO. Starting simulation."),
    exp_simulation_runner:start_simulation(),
    {noreply, State};

handle_cast({break_links_info, Infos}, State) ->
    Specs = lists:map(
        fun({Name, Ip, ?BARRIER_PORT}) -> {Name, Ip, ?PORT} end,
        Infos
    ),
    {Names, _, _} = lists:unzip3(Specs),
    lager:info("Received BREAK LINKS INFO. ~p", [Names]),

    ldb_whisperer:update_metrics_members(Names),
    {noreply, State#state{break_links_specs=Specs}};

handle_cast(break_links, #state{break_links_specs=Specs,
                                partisan_manager=Manager}=State) ->
    lager:info("Received BREAK LINKS."),
    {_, Ips, _} = lists:unzip3(Specs),
    Manager:close_connections(Ips),
    {noreply, State};

handle_cast(heal_links, #state{break_links_specs=Specs,
                               partisan_manager=Manager}=State) ->
    lager:info("Received HEAL LINKS."),
    connect(Specs, Manager),
    {noreply, State};

handle_cast(metrics_go, State) ->
    lager:info("Received METRICS GO. Pushing metrics."),
    exp_simulations_support:push_ldb_metrics(),
    tell({metrics_done, ldb_config:id()}),
    {noreply, State};

handle_cast(Msg, State) ->
    lager:warning("Unhandled cast message: ~p", [Msg]),
    {noreply, State}.

handle_info(create_barrier, State) ->
    case exp_orchestration:get_task(rsg, ?BARRIER_PORT, true) of
        {ok, RSG} ->
            ok = connect([RSG], ?BARRIER_PEER_SERVICE),
            schedule_join_peers();
        {error, not_connected} ->
            schedule_create_barrier()
    end,

    {noreply, State};

handle_info(join_peers, #state{node_number=NodeNumber,
                               partisan_manager=Manager}=State) ->
    MyName = ldb_config:id(),
    Nodes = exp_orchestration:get_tasks(exp, ?PORT, true),
    Overlay = exp_config:get(exp_overlay),

    case length(Nodes) == NodeNumber of
        true ->
            %% if all nodes are connected
            {NumericalId, ToConnect} = exp_overlay:numerical_id_and_neighbors(MyName,
                                                                               Nodes,
                                                                               Overlay),
            %% set numerical id
            exp_config:set(exp_numerical_id, NumericalId),
            %% and connect to neighbors
            ok = connect(ToConnect, Manager),
            tell({connect_done, ldb_config:id()});
        _ ->
            schedule_join_peers()
    end,
    {noreply, State};

handle_info(Msg, State) ->
    lager:warning("Unhandled info message: ~p", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
schedule_create_barrier() ->
    timer:send_after(?INTERVAL, create_barrier).

%% @private
schedule_join_peers() ->
    timer:send_after(?INTERVAL, join_peers).

%% @private
connect([], _) ->
    ok;
connect([Node|Rest]=All, PeerService) ->
    case PeerService:join(Node) of
        ok ->
            connect(Rest, PeerService);
        Error ->
            lager:info("Couldn't connect to ~p. Reason ~p. Will try again in ~p ms",
                       [Node, Error, ?INTERVAL]),
            timer:sleep(?INTERVAL),
            connect(All, PeerService)
    end.

%% @private
tell(Msg) ->
    {ok, Members} = ?BARRIER_PEER_SERVICE:members(),
    lists:foreach(
        fun(Peer) ->
            ?BARRIER_PEER_SERVICE:forward_message(
               Peer,
               exp_rsg_master,
               Msg
            )
        end,
        without_me(Members)
     ).

%% @private
without_me(Members) ->
    Members -- [ldb_config:id()].
