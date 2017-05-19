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
-export([start_link/1,
         start/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {event_count :: non_neg_integer(),
                start_fun :: function(),
                event_fun :: function(),
                total_events_fun :: function(),
                check_end_fun :: function()}).

-define(DEFAULT_EVENT_INTERVAL, 1000).
-define(SIMULATION_END_INTERVAL, 10000).

-spec start_link([function()]) ->
    {ok, pid()} | ignore | {error, term()}.
start_link(Funs) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Funs, []).

-spec start() -> ok.
start() ->
    gen_server:call(?MODULE, start, infinity).

%% gen_server callbacks
init([StartFun, EventFun, TotalEventsFun, CheckEndFun]) ->
    ?LOG("lsim_simulation_runner initialized"),
    {ok, #state{event_count=0,
                start_fun=StartFun,
                event_fun=EventFun,
                total_events_fun=TotalEventsFun,
                check_end_fun=CheckEndFun}}.

handle_call(start, _From, #state{start_fun=StartFun}=State) ->
    StartFun(),
    schedule_event(),

    {reply, ok, State};

handle_call(Msg, _From, State) ->
    lager:warning("Unhandled call message: ~p", [Msg]),
    {noreply, State}.

handle_cast(Msg, State) ->
    lager:warning("Unhandled cast message: ~p", [Msg]),
    {noreply, State}.

handle_info(event, #state{event_count=Events0,
                          event_fun=EventFun,
                          total_events_fun=TotalEventsFun}=State) ->
    Events = Events0 + 1,
    EventFun(Events),
    TotalEvents = TotalEventsFun(),
    ?LOG("Event ~p | Observed ~p | Node ~p", [Events, TotalEvents, ldb_config:id()]),

    case Events == node_event_number() of
        true ->
            %% If I did all the events I should do
            schedule_simulation_end();
        false ->
            schedule_event()
    end,

    {noreply, State#state{event_count=Events}};

handle_info(simulation_end, #state{total_events_fun=TotalEventsFun,
                                   check_end_fun=CheckEndFun}=State) ->
    TotalEvents = TotalEventsFun(),
    ?LOG("Events observed ~p | Node ~p", [TotalEvents, ldb_config:id()]),

    case CheckEndFun(node_number(), node_event_number()) of
        true ->
            %% If everyone did all the events they should do
            ?LOG("All events have been observed"),
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
node_number() ->
    lsim_config:get(lsim_node_number).

%% @private
node_event_number() ->
    lsim_config:get(lsim_node_event_number).

%% @private
schedule_event() ->
    timer:send_after(?DEFAULT_EVENT_INTERVAL, event).

%% @private
schedule_simulation_end() ->
    timer:send_after(?SIMULATION_END_INTERVAL, simulation_end).

%% @private
end_simulation() ->
    case lsim_config:get(lsim_orchestration) of
        undefined ->
            lsim_config:set(lsim_simulation_end, true);
        _ ->
            lsim_rsg:simulation_end()
    end.
