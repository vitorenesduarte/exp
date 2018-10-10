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

-module(exp_simulation_runner).
-author("Vitor Enes <vitorenesduarte@gmail.com").

-include("exp.hrl").

-behaviour(gen_server).

%% exp_simulation_runner callbacks
-export([start_link/1,
         start_simulation/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {event_count :: non_neg_integer(),
                event_fun :: function(),
                total_events_fun :: function(),
                check_end_fun :: function(),
                node_number :: non_neg_integer(),
                node_event_number :: non_neg_integer(),
                event_interval :: non_neg_integer()}).

-define(SIMULATION_END_INTERVAL, 2000).

-spec start_link([function()]) ->
    {ok, pid()} | ignore | {error, term()}.
start_link(Funs) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Funs, []).

-spec start_simulation() -> ok.
start_simulation() ->
    gen_server:call(?MODULE, start_simulation, infinity).

%% gen_server callbacks
init([StartFun, EventFun, TotalEventsFun, CheckEndFun]) ->
    lager:info("exp_simulation_runner initialized"),

    %% start fun is called here,
    %% and start simulation schedules the first event
    StartFun(),

    %% get node number and node event number
    NodeNumber = exp_config:get(exp_node_number),
    NodeEventNumber = exp_config:get(exp_node_event_number),
    EventInterval = exp_config:get(exp_event_interval),

    {ok, #state{event_count=0,
                event_fun=EventFun,
                total_events_fun=TotalEventsFun,
                check_end_fun=CheckEndFun,
                node_number=NodeNumber,
                node_event_number=NodeEventNumber,
                event_interval=EventInterval}}.

handle_call(start_simulation, _From, #state{event_interval=EventInterval}=State) ->
    schedule_event(EventInterval),
    {reply, ok, State};

handle_call(Msg, _From, State) ->
    lager:warning("Unhandled call message: ~p", [Msg]),
    {noreply, State}.

handle_cast(Msg, State) ->
    lager:warning("Unhandled cast message: ~p", [Msg]),
    {noreply, State}.

handle_info(event, #state{event_count=Events0,
                          event_fun=EventFun,
                          total_events_fun=TotalEventsFun,
                          node_number=NodeNumber,
                          node_event_number=NodeEventNumber,
                          event_interval=EventInterval}=State) ->
    Events = Events0 + 1,
    EventFun(Events, NodeNumber, NodeEventNumber),
    TotalEvents = TotalEventsFun(),
    lager:info("Event ~p | Observed ~p | Node ~p", [Events, TotalEvents, ldb_config:id()]),

    case Events == NodeEventNumber of
        true ->
            %% If I did all the events I should do
            schedule_simulation_end();
        false ->
            schedule_event(EventInterval)
    end,

    {noreply, State#state{event_count=Events}};

handle_info(simulation_end, #state{total_events_fun=TotalEventsFun,
                                   check_end_fun=CheckEndFun,
                                   node_number=NodeNumber,
                                   node_event_number=NodeEventNumber}=State) ->
    TotalEvents = TotalEventsFun(),
    lager:info("Events observed ~p | Node ~p", [TotalEvents, ldb_config:id()]),

    case CheckEndFun(NodeNumber, NodeEventNumber) of
        true ->
            %% If everyone did all the events they should do
            lager:info("All events have been observed"),
            end_simulation();
        false ->
            schedule_simulation_end()
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
schedule_event(EventInterval) ->
    timer:send_after(EventInterval, event).

%% @private
schedule_simulation_end() ->
    timer:send_after(?SIMULATION_END_INTERVAL, simulation_end).

%% @private
end_simulation() ->
    case exp_config:get(exp_orchestration) of
        undefined ->
            exp_config:set(exp_simulation_end, true);
        _ ->
            exp_rsg:simulation_end()
    end.
