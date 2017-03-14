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

-module(lsim_overlay).
-author("Vitor Enes Duarte <vitorenesduarte@gmail.com").

-include("lsim.hrl").

-export([get/2,
         to_connect/3]).

%% @doc The first argument can be:
%%          - `hyparview'
%%          - `ring'
%%          - `line'
%%      The second argument is the number of nodes.
-spec get(atom(), pos_integer()) -> orddict:orddict().
get(_, 1) ->
    [];
get(hyparview, N) ->
    %% In HyParView, everyone connects to a single node.
    lists:foldl(
        fun(I, Acc) ->
            orddict:store(I, [0], Acc)
        end,
        [{0, []}],
        lists:seq(1, N - 1)
    );
get(ring, N) ->
    lists:foldl(
        fun(I, Acc) ->
            Peers = [
                previous(I, N),
                next(I, N)
            ],
            orddict:store(I, Peers, Acc)
        end,
        orddict:new(),
        lists:seq(0, N - 1)
    );
get(line, N) ->
    T0 = get(ring, N),
    First = 0,
    Last = N - 1,
    T1 = lists:keyreplace(
        First,
        1,
        T0,
        {First, [next(First, N)]}
    ),
    T2 = lists:keyreplace(
        Last,
        1,
        T1,
        {Last, [previous(Last, N)]}
    ),
    T2.

%% @doc The first argument is my node spec,
%%      the second argument is a list of node specs,
%%      and the third argument is the overlay.
-spec to_connect(ldb_node_id(), [node_spec()], atom()) ->
    [node_spec()].
to_connect(MyName, Nodes, Overlay) ->
    Map = list_to_map(Nodes),
    {IdToName, MyId} = map_to_ids(MyName, Map),
    NodeNumber = length(Nodes),
    Topology = get(Overlay, NodeNumber),
    find_peers(Map, IdToName, MyId, Topology).

%% @private
previous(I, N) ->
    First = 0,
    case I of
        First ->
            N - 1;
        _ ->
            I - 1
    end.

%% @private
next(I, N) ->
    Last = N - 1,
    case I of
        Last ->
            0;
        _ ->
            I + 1
    end.

%% @private
list_to_map(Nodes) ->
    lists:foldl(
        fun({Name, _, _}=Node, Acc) ->
            orddict:store(Name, Node, Acc)
        end,
        orddict:new(),
        Nodes
    ).

%% @private
map_to_ids(MyName, Map) ->
    {IdToName, MyId, _} = lists:foldl(
        fun({Name, _}, {IdToName0, MyId0, Counter0}) ->
            IdToName1 = orddict:store(Counter0, Name, IdToName0),
            MyId1 = case MyName == Name of
                true ->
                    Counter0;
                false ->
                    MyId0
            end,
            Counter1 = Counter0 + 1,
            {IdToName1, MyId1, Counter1}
        end,
        {orddict:new(), undefined, 0},
        Map
    ),

    {IdToName, MyId}.

%% @private
find_peers(Map, IdToName, MyId, Topology) ->
    IdsToConnect = orddict:fetch(MyId, Topology),

    lists:map(
        fun(PeerId) ->
            PeerName = orddict:fetch(PeerId, IdToName),
            orddict:fetch(PeerName, Map)
        end,
        IdsToConnect
    ).
