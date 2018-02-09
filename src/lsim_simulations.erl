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
-define(GMAP_KEY_NUMBER, 1000).

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
                Type = {gmap, [lwwregister]},
                ldb:create(?KEY, Type)
            end,
            EventFun = fun(EventNumber, NodeNumber, NodeEventNumber) ->
                LastEvent = EventNumber == NodeEventNumber,

                Op = case LastEvent of
                    true ->
                        %% TODO ldb should support rewriting of ops
                        %% that contain crdt types as the case of this one
                        %% `state_gcounter' -> `gcounter'
                        {apply, done, state_gcounter, increment};
                    false ->
                        Percentage = lsim_config:get(lsim_gmap_simulation_key_percentage),
                        KeysPerNode = round(?GMAP_KEY_NUMBER / NodeNumber),
                        KeysPerIteration = round((Percentage * KeysPerNode) / 100),

                        %% node with id i has keys from [i * KeysPerNode, ((i + 1) * KeysPerNode) - 1]
                        NumericalId = lsim_config:get(lsim_numerical_id),
                        Start = NumericalId * KeysPerNode,
                        End0 = ((NumericalId + 1) * KeysPerNode) - 1,
                        %% since `End0' can be bigger than `?GMAP_KEY_NUMBER':
                        End = min(?GMAP_KEY_NUMBER, End0),

                        %% shuffle possible keys
                        %% and take the first `KeysPerIteration'
                        ShuffledKeys = lsim_util:shuffle_list(
                            lists:seq(Start, End)
                        ),
                        Keys = lists:sublist(ShuffledKeys, KeysPerIteration),

                        Ops = lists:map(
                            fun(Key) ->
                                Timestamp = erlang:system_time(microsecond),
                                {Key, Timestamp, Timestamp}
                            end,
                            Keys
                        ),

                        {apply_all, Ops}
                end,

                ldb:update(?KEY, Op)
            end,
            TotalEventsFun = fun() ->
                {ok, Query} = ldb:query(?KEY),
                orddict:size(Query)
            end,
            CheckEndFun = fun(NodeNumber, _NodeEventNumber) ->
                {ok, Query} = ldb:query(?KEY),
                %% a node has observed all events
                %% if key `done' counter value equals to node number.
                Ended = orddict_ext:fetch(done, Query, 0) == NodeNumber,

                case Ended of
                    true ->
                        %% assert the number of keys in the end of
                        %% the simulation is correct
                        true = orddict:size(Query) == ?GMAP_KEY_NUMBER + 1;
                    false ->
                        ok
                end,

                %% return
                Ended
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
