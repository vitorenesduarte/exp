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

-module(exp_simulations).
-author("Vitor Enes <vitorenesduarte@gmail.com").

-include("exp.hrl").

-define(KEY, "events").
-define(GMAP_KEY_NUMBER, 1000).

%% exp_simulations callbacks
-export([get_specs/1]).

%% @doc
-spec get_specs(atom()) -> [term()].
get_specs(Simulation) ->
    Funs = case Simulation of
        undefined ->
            [];

        awset ->
            StartFun = fun() ->
                ldb:create(?KEY, awset)
            end,
            EventFun = fun(EventNumber, _NodeNumber, NodeEventNumber) ->
                Addition = EventNumber rem 4 /= 0,
                LastEvent = EventNumber == NodeEventNumber,

                %% if it's the last event,
                %% do an addition always,
                %% so that we have a way to
                %% detect when a node has
                %% observed all events
                case Addition orelse LastEvent of
                    true ->
                        Element = create_element(EventNumber),
                        ldb:update(?KEY, {add, Element});
                    false ->
                        %% remove an element added by me
                        {ok, Query} = ldb:query(?KEY),
                        ByMe = sets:to_list(
                            sets:filter(
                                fun(E) ->
                                    string:str(E, atom_to_list(ldb_config:id())) > 0
                                end,
                                Query
                            )
                        ),
                        Element = lists:nth(
                            rand:uniform(length(ByMe)),
                            ByMe
                        ),
                        ldb:update(?KEY, {rmv, Element})
                end
            end,
            TotalEventsFun = fun() ->
                {ok, Value} = ldb:query(?KEY),
                sets:size(Value)
            end,
            CheckEndFun = fun(NodeNumber, NodeEventNumber) ->
                {ok, Query} = ldb:query(?KEY),
                %% a node has observed all events
                %% if it has in the set
                %% `NodeNumber` elements ending in
                %% `NodeEventNumber`
                LastElements = sets:filter(
                    fun(E) ->
                        string:str(E, element_sufix(NodeEventNumber)) > 0
                    end,
                    Query
                ),
                sets:size(LastElements) == NodeNumber
            end,
            [StartFun,
             EventFun,
             TotalEventsFun,
             CheckEndFun];

        gcounter ->
            StartFun = fun() ->
                ldb:create(?KEY, gcounter)
            end,
            EventFun = fun(_EventNumber, _NodeNumber, _NodeEventNumber) ->
                ldb:update(?KEY, increment)
            end,
            TotalEventsFun = fun() ->
                {ok, Value} = ldb:query(?KEY),
                Value
            end,
            CheckEndFun = fun(NodeNumber, NodeEventNumber) ->
                TotalEventsFun() == NodeNumber * NodeEventNumber
            end,
            [StartFun,
             EventFun,
             TotalEventsFun,
             CheckEndFun];

        gset ->
            StartFun = fun() ->
                ldb:create(?KEY, gset)
            end,
            EventFun = fun(EventNumber, _NodeNumber, _NodeEventNumber) ->
                Element = create_element(EventNumber),
                ldb:update(?KEY, {add, Element})
            end,
            TotalEventsFun = fun() ->
                {ok, Value} = ldb:query(?KEY),
                sets:size(Value)
            end,
            CheckEndFun = fun(NodeNumber, NodeEventNumber) ->
                TotalEventsFun() == NodeNumber * NodeEventNumber
            end,
            [StartFun,
             EventFun,
             TotalEventsFun,
             CheckEndFun];

        gmap ->
            StartFun = fun() ->
                Type = {gmap, [max_int]},
                ldb:create(?KEY, Type)
            end,
            EventFun = fun(_EventNumber, NodeNumber, _NodeEventNumber) ->
                Percentage = exp_config:get(exp_gmap_simulation_key_percentage),
                KeysPerNode = round_up(?GMAP_KEY_NUMBER / NodeNumber),

                %% node with id i has keys in
                %% [i * KeysPerNode, ((i + 1) * KeysPerNode) - 1]
                NumericalId = exp_config:get(exp_numerical_id),
                Start = NumericalId * KeysPerNode + 1,
                End0 = ((NumericalId + 1) * KeysPerNode),
                %% since `End0' can be bigger than `?GMAP_KEY_NUMBER':
                End = min(?GMAP_KEY_NUMBER, End0),

                %% create my keys
                MyKeys0 = lists:seq(Start, End),

                %% shuffle keys
                MyKeys = exp_util:shuffle_list(MyKeys0),

                %% take the first `KeysPerIteration'
                KeysPerIteration = round_up((Percentage * KeysPerNode) / 100),
                Keys = lists:sublist(MyKeys, KeysPerIteration),

                Ops = lists:map(fun(Key) -> {Key, increment} end, Keys),
                %% TODO support rewriting of ops in ldb
                EventOp = {gmap_events, state_gcounter, increment},
                ldb:update(?KEY, {apply_all, [EventOp | Ops]})
            end,
            TotalEventsFun = fun() ->
                {ok, Query} = ldb:query(?KEY),
                case lists:keyfind(gmap_events, 1, Query) of
                    {gmap_events, V} -> V;
                    false -> 0
                end
            end,
            CheckEndFun = fun(NodeNumber, NodeEventNumber) ->
                TotalEventsFun() == NodeNumber * NodeEventNumber
            end,
            [StartFun,
             EventFun,
             TotalEventsFun,
             CheckEndFun]

    end,

    create_spec(Funs).

%% @private
create_spec(Funs) ->
    case Funs of
        [] ->
            [];
        _ ->
            [{exp_simulation_runner,
              {exp_simulation_runner, start_link, [Funs]},
              permanent, 5000, worker, [exp_simulation_runner]}]
    end.

%% @private Create an unique element to be added to the set.
create_element(EventNumber) ->
    MyName = ldb_config:id(),
    atom_to_list(MyName) ++ element_sufix(EventNumber).

%% @private Create elements suffix.
element_sufix(EventNumber) ->
    "#" ++ integer_to_list(EventNumber).

%% @private Round up.
round_up(A) ->
    trunc(A) + 1.
