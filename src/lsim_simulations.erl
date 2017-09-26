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
            trcb_simulation()

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
trcb_simulation() ->
    StartFun = fun() ->
      {ok, Members} = partisan_peer_service:members(),
        lager:info("partisan_peer_service:members(): ~p", [Members]),

      featherine:tcbfullmembership(Members),

      put(delivery, 0),
      put(stability, 0),

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
      featherine:tcbstability(StabFun)
    end,

    EventFun = fun(_Arg) ->
        featherine:tcbcast(msg)
    end,

    TotalEventsFun = fun() ->
        {get(delivery), get(stability)}
    end,

    CheckEndFun = fun(NodeNumber, NodeEventNumber) ->
        TheoTot = NodeNumber * NodeEventNumber,
        {PracTotDelv, PracTotStab} = TotalEventsFun(),
        PracTotDelv == TheoTot andalso PracTotStab >= (TheoTot - NodeNumber)
    end,

    HandleCastFun = fun(Msg) ->
        lager:info("Before Doing ~p value is: ~p", [Msg, get(Msg)]),
        put(Msg, get(Msg) + 1),
        lager:info("After Doing ~p value is: ~p", [Msg, get(Msg)])
    end,

    [StartFun,
     EventFun,
     TotalEventsFun,
     CheckEndFun,
     HandleCastFun].
