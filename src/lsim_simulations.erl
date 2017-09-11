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

        trcb ->
            trcb_simulation();

        awset ->
            simple_set_simulation(awset);

        gcounter ->
            StartFun = fun() ->
                ldb:create(?KEY, gcounter)
            end,
            EventFun = fun(_EventNumber) ->
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
            simple_set_simulation(gset);

        gmap ->
            StartFun = fun() ->
                Type = {gmap,
                        [{pair,
                          [gcounter, gcounter]}]},
                ldb:create(?KEY, Type)
            end,
            EventFun = fun(EventNumber) ->
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

%% @private
simple_set_simulation(Type) ->
    StartFun = fun() ->
        ldb:create(?KEY, Type)
    end,
    EventFun = fun(EventNumber) ->
        MyName = ldb_config:id(),
        Ratio = lsim_config:get(lsim_element_node_ratio),
        Element0 = lists:foldl(
            fun(_, Acc) ->
               Acc ++ atom_to_list(MyName) ++ "_"
            end,
            "",
            lists:seq(1, Ratio)
        ),

        Element1 = Element0 ++ integer_to_list(EventNumber),
        ldb:update(?KEY, {add, Element1})
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
     CheckEndFun].

%% @private
trcb_simulation() ->
    StartFun = fun() ->
      {ok, Members} = partisan_peer_service:members(),
      featherine:tcbfullmembership(Members),

      DelvFun = fun(Msg) ->
        lager:info("Message delivered: ~p", [Msg]),
        gen_server:cast(lsim_simulation_runner, delivery),
        ok
      end,
      featherine:tcbdelivery(DelvFun),

      StabFun = fun(Msg) ->
        lager:info("Message stabilized: ~p", [Msg]),
        gen_server:cast(lsim_simulation_runner, stability)
      end,
      featherine:tcbstability(StabFun),

      put(delivery, 0),
      put(stability, 0)
    end,

    EventFun = fun(_Arg) ->
        featherine:tcbcast(msg)
    end,

    TotalEventsFun = fun() ->
        {get(delivered), get(stability)}
    end,

    CheckEndFun = fun(NodeNumber, NodeEventNumber) ->
        Tot = NodeNumber * NodeEventNumber,
        TotalEventsFun() == {Tot, Tot}
    end,

    HandleCastFun = fun(Msg) ->
        put(Msg, get(Msg) + 1)
    end,

    [StartFun,
     EventFun,
     TotalEventsFun,
     CheckEndFun,
     HandleCastFun].
