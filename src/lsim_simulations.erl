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
    case Simulation of
        undefined ->
            [];
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
            create_spec(StartFun,
                        EventFun,
                        TotalEventsFun);
        gset ->
            StartFun = fun() ->
                ldb:create(?KEY, gset)
            end,
            EventFun = fun(EventNumber) ->
                Element = atom_to_list(node()) ++
                          integer_to_list(EventNumber),
                ldb:update(?KEY, {add, Element})
            end,
            TotalEventsFun = fun() ->
                {ok, Value} = ldb:query(?KEY),
                sets:size(Value)
            end,
            create_spec(StartFun,
                        EventFun,
                        TotalEventsFun);
        group ->
            StartFun = fun() ->
                Groups = lsim_config:get(groups),

                lists:foreach(
                    fun(Group) ->
                        ldb:create(Group, gcounter)
                    end,
                    Groups
                )
            end,

            EventFun = fun(_EventNumber) ->
                [Group | _] = lsim_config:get(groups),
                ldb:update(Group, increment)
            end,

            TotalEventsFun = fun() ->
                {ok, V1} = ldb:query("g1"),
                {ok, V2} = ldb:query("g2"),
                V1 + V2
            end,

            create_spec(StartFun,
                        EventFun,
                        TotalEventsFun)
    end.

%% @private
create_spec(StartFun, EventFun, TotalEventsFun) ->
    [{lsim_simulation_runner,
      {lsim_simulation_runner, start_link,
       [StartFun,
        EventFun,
        TotalEventsFun]},
      permanent, 5000, worker, [lsim_simulation_runner]}].
