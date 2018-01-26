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

-module(lsim_simulations).
-author("Vitor Enes Duarte <vitorenesduarte@gmail.com").

-include("lsim.hrl").

-define(KEY, "events").

%% lsim_simulations callbacks
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
            EventFun = fun(EventNumber, NodeEventNumber) ->
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
        awset_single_source ->
            StartFun = fun() ->
                ldb:create(?KEY, awset)
            end,
            EventFun = fun(EventNumber, NodeEventNumber) ->
                Addition = EventNumber rem 4 /= 0,
                LastEvent = EventNumber == NodeEventNumber,

                NumId = lsim_config:get(node_numerical_id, oops),
                MyName = ldb_config:id(),
                lager:info("Node Name:~p, NumId ~p",[MyName, NumId]),
                if
                    NumId == 0 ->
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
                        end;
                    true -> ok
                end
            end,
            TotalEventsFun = fun() ->
                {ok, Value} = ldb:query(?KEY),
                sets:size(Value)
            end,
            CheckEndFun = fun(_NodeNumber, NodeEventNumber) ->
                {ok, Query} = ldb:query(?KEY),
                %% a node has observed all events if it has one
                %% `NodeNumber` element ending in `NodeEventNumber`
                LastElements = sets:filter(
                    fun(E) ->
                        string:str(E, element_sufix(NodeEventNumber)) > 0
                    end,
                    Query
                ),
                sets:size(LastElements) == 1
            end,
            [StartFun,
             EventFun,
             TotalEventsFun,
             CheckEndFun];
        gcounter ->
            StartFun = fun() ->
                ldb:create(?KEY, gcounter)
            end,
            EventFun = fun(_EventNumber, _NodeEventNumber) ->
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
            EventFun = fun(EventNumber, _NodeEventNumber) ->
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
                Type = {gmap,
                        [{pair,
                          [gcounter, gcounter]}]},
                ldb:create(?KEY, Type)
            end,
            EventFun = fun(EventNumber, _NodeEventNumber) ->
                Component = case EventNumber rem 2 of
                    0 ->
                        %% if even, increment the first component
                        %% of the pair
                        fst;
                    1 ->
                        %% else, the second
                        snd
                end,
                Op = {apply, ?KEY, {Component, increment}},
                ldb:update(?KEY, Op)
            end,
            TotalEventsFun = fun() ->
                {ok, [{?KEY, {Fst, Snd}}]} = ldb:query(?KEY),
                Fst + Snd
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
            [{lsim_simulation_runner,
              {lsim_simulation_runner, start_link, [Funs]},
              permanent, 5000, worker, [lsim_simulation_runner]}]
    end.

%% @private Create an unique element to be added to the set.
create_element(EventNumber) ->
    MyName = ldb_config:id(),
    atom_to_list(MyName) ++ element_sufix(EventNumber).

%% @private Create elements suffix.
element_sufix(EventNumber) ->
    "#" ++ integer_to_list(EventNumber).
