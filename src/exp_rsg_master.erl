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

-module(exp_rsg_master).
-author("Vitor Enes <vitorenesduarte@gmail.com").

-include("exp.hrl").

-behaviour(gen_server).

%% exp_rsg_master callbacks
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {nodes :: undefined | list(node_spec()),
                connect_done:: ordsets:ordset(ldb_node_id()),
                sim_done :: ordsets:ordset(ldb_node_id()),
                metrics_done :: ordsets:ordset(ldb_node_id()),
                metrics_nodes :: undefined | list(ldb_node_id()),
                start_time :: undefined | timestamp()}).

-define(BARRIER_PEER_SERVICE, exp_barrier_peer_service).
-define(INTERVAL, 3000).

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server callbacks
init([]) ->
    schedule_create_barrier(),
    lager:info("exp_rsg_master initialized"),

    {ok, #state{nodes=undefined,
                connect_done=ordsets:new(),
                sim_done=ordsets:new(),
                metrics_done=ordsets:new(),
                metrics_nodes=undefined,
                start_time=undefined}}.

handle_call(Msg, _From, State) ->
    lager:warning("Unhandled call message: ~p", [Msg]),
    {noreply, State}.

handle_cast({connect_done, NodeName},
            #state{nodes=Nodes,
                   connect_done=ConnectDone0}=State) ->

    lager:info("Received CONNECT DONE from ~p", [NodeName]),

    ConnectDone1 = ordsets:add_element(NodeName, ConnectDone0),

    {T1, MetricsNodes1} = case ordsets:size(ConnectDone1) == node_number() of
        true ->
            lager:info("Everyone is CONNECT DONE. SIM GO!"),

            MetricsNodes0 = configure_break_links_metrics(Nodes),

            T0 = ldb_util:unix_timestamp(),
            tell(sim_go),
            {T0, MetricsNodes0};
        false ->
            {undefined, undefined}
    end,

    {noreply, State#state{connect_done=ConnectDone1,
                          metrics_nodes=MetricsNodes1,
                          start_time=T1}};

handle_cast({sim_done, NodeName},
            #state{sim_done=SimDone0,
                   metrics_nodes=MetricsNodes}=State) ->

    lager:info("Received SIM DONE from ~p", [NodeName]),

    SimDone1 = ordsets:add_element(NodeName, SimDone0),

    case ordsets:size(SimDone1) == node_number() of
        true ->
            lager:info("Everyone is SIM DONE. METRICS GO to ~p!", [MetricsNodes]),
            tell(metrics_go, MetricsNodes);
        false ->
            ok
    end,

    {noreply, State#state{sim_done=SimDone1}};

handle_cast({metrics_done, NodeName},
            #state{metrics_done=MetricsDone0,
                   metrics_nodes=MetricsNodes,
                   start_time=StartTime}=State) ->

    lager:info("Received METRICS DONE from ~p", [NodeName]),

    MetricsDone1 = ordsets:add_element(NodeName, MetricsDone0),

    case ordsets:size(MetricsDone1) == length(MetricsNodes) of
        true ->
            lager:info("Everyone is METRICS DONE. STOP!!!"),
            exp_simulations_support:push_exp_metrics(StartTime),
            exp_orchestration:stop_tasks([exp, rsg]);
        false ->
            ok
    end,

    {noreply, State#state{metrics_done=MetricsDone1}};

handle_cast(Msg, State) ->
    lager:warning("Unhandled cast message: ~p", [Msg]),
    {noreply, State}.

handle_info(create_barrier, State) ->
    Nodes = exp_orchestration:get_tasks(exp, ?BARRIER_PORT, true),

    case length(Nodes) == node_number() of
        true ->
            ok = connect(Nodes);
        false ->
            schedule_create_barrier()
    end,
    {noreply, State#state{nodes=Nodes}};

handle_info(break_links, #state{metrics_nodes=MetricsNodes}=State) ->
    lager:info("BREAK LINKS ~p", [MetricsNodes]),
    tell(break_links, MetricsNodes),
    schedule_heal_links(),
    {noreply, State};

handle_info(heal_links, #state{metrics_nodes=MetricsNodes}=State) ->
    lager:info("HEAL LINKS"),
    tell(heal_links, MetricsNodes),
    {noreply, State};

handle_info(Msg, State) ->
    lager:warning("Unhandled info message: ~p", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
configure_break_links_metrics(Nodes) ->
    %% list of nodes from which we want metrics
    %% - in case of break links, only the involved nodes
    %% - otherwise, all
    BreakLinks = exp_config:get(exp_break_links),
    Overlay = exp_config:get(exp_overlay),
    {Names, Links} = exp_overlay:break_links(BreakLinks, Nodes, Overlay),

    %% inform all nodes involved in break links
    lists:foreach(
        fun({Name, Specs}) ->
            tell({break_links_info, Specs}, [Name])
        end,
        Links
    ),

    %% schedule break links
    case BreakLinks of
        none -> ok;
        _ -> schedule_break_links()
    end,

    Names.

%% @private
node_number() ->
    exp_config:get(exp_node_number).

%% @private
schedule_create_barrier() ->
    timer:send_after(?INTERVAL, create_barrier).

%% @private
schedule_break_links() ->
    NodeEventNumber = exp_config:get(exp_node_event_number),
    %% wait ~50% of simulation time before breaking links
    Seconds = NodeEventNumber div 2,
    timer:send_after(Seconds * 1000, break_links).

%% @private
schedule_heal_links() ->
    NodeEventNumber = exp_config:get(exp_node_event_number),
    %% wait ~25% of simulation time before healing
    Seconds = NodeEventNumber div 4,
    timer:send_after(Seconds * 1000, heal_links).

%% @private
connect([]) ->
    ok;
connect([Node|Rest]=All) ->
    case ?BARRIER_PEER_SERVICE:join(Node) of
        ok ->
            connect(Rest);
        Error ->
            lager:info("Couldn't connect to ~p. Reason ~p. Will try again in ~p ms",
                       [Node, Error, ?INTERVAL]),
            timer:sleep(?INTERVAL),
            connect(All)
    end.

%% @private send to all
tell(Msg) ->
    tell(Msg, rsgs()).

%% @private send to some
tell(Msg, Peers) ->
    lists:foreach(
        fun(Peer) ->
            ?BARRIER_PEER_SERVICE:forward_message(
               Peer,
               exp_rsg,
               Msg
            )
        end,
        Peers
     ).

%% @private
rsgs() ->
    {ok, Members} = ?BARRIER_PEER_SERVICE:members(),
    without_me(Members).

%% @private
without_me(Members) ->
    Members -- [ldb_config:id()].
