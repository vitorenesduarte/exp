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

-module(lsim_simulation_runner).
-author("Vitor Enes Duarte <vitorenesduarte@gmail.com").

-include("lsim.hrl").

-behaviour(gen_server).

%% lsim_simulation_runner callbacks
-export([start_link/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {event_count :: non_neg_integer(),
                event_fun :: function(),
                total_events_fun :: function()}).

-define(EVENT_NUMBER, 10).
-define(EVENT_INTERVAL, 5000).
-define(SIMULATION_END_INTERVAL, 10000).

-spec start_link(function(), function(), function()) ->
    {ok, pid()} | ignore | {error, term()}.
start_link(StartFun, EventFun, TotalEventsFun) ->
    gen_server:start_link({local, ?MODULE},
                          ?MODULE,
                          [StartFun, EventFun, TotalEventsFun],
                          []).

%% gen_server callbacks
init([StartFun, EventFun, TotalEventsFun]) ->
    StartFun(),
    schedule_event(),

    ldb_log:info("lsim_simulation_runner initialized"),
    {ok, #state{event_count=0,
                event_fun=EventFun,
                total_events_fun=TotalEventsFun}}.

handle_call(Msg, _From, State) ->
    ldb_log:warning("Unhandled call message: ~p", [Msg]),
    {noreply, State}.

handle_cast(Msg, State) ->
    ldb_log:warning("Unhandled cast message: ~p", [Msg]),
    {noreply, State}.

handle_info(event, #state{event_count=Events0,
                          event_fun=EventFun}=State) ->
    Events = case simulation_started() of
        true ->
            Events1 = Events0 + 1,
            EventFun(Events1),
            ldb_log:info("Event ~p | Node ~p", [Events1, node()]),

            case Events1 == ?EVENT_NUMBER of
                true ->
                    %% If I did all the events I should do
                    schedule_simulation_end();
                false ->
                    schedule_event()
            end,

            Events1;
        false ->
            schedule_event(),
            Events0
    end,

    {noreply, State#state{event_count=Events}};

handle_info(simulation_end, #state{total_events_fun=TotalEventsFun}=State) ->
    NodeNumber = lsim_config:get(lsim_node_number),
    TotalEvents = TotalEventsFun(),

    ldb_log:info("Events observed ~p | Node ~p", [TotalEvents, node()]),

    case TotalEvents == NodeNumber * ?EVENT_NUMBER of
        true ->
            %% If everyone did all the events they should do
            ldb_log:info("All events have been observed"),
            lsim_config:set(simulation_end, true);
        false ->
            schedule_simulation_end()
    end,

    {noreply, State};

handle_info(Msg, State) ->
    ldb_log:warning("Unhandled info message: ~p", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
simulation_started() ->
    %% @todo Fix this for DCOS runs
    true.

%% @private
schedule_event() ->
    timer:send_after(?EVENT_INTERVAL, event).

%% @private
schedule_simulation_end() ->
    timer:send_after(?SIMULATION_END_INTERVAL, simulation_end).
