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
%%          - `line'
%%          - `ring'
%%          - `erdos_renyi'
%%      The second argument is the number of nodes.
-spec get(atom(), non_neg_integer()) -> orddict:orddict().
get(_, 1) ->
    [];
get(line, 3) ->
    [{0, [1]},
     {1, [0, 2]},
     {2, [1]}];
get(line, 5) ->
    [{0, [1]},
     {1, [0, 2]},
     {2, [1, 3]},
     {3, [2, 4]},
     {4, [3]}];
get(line, 7) ->
    [{0, [1]},
     {1, [0, 2]},
     {2, [1, 3]},
     {3, [2, 4]},
     {4, [3, 5]},
     {5, [4, 6]},
     {6, [5]}];
get(line, 13) ->
    [{0, [1]},
     {1, [0, 2]},
     {2, [1, 3]},
     {3, [2, 4]},
     {4, [3, 5]},
     {5, [4, 6]},
     {6, [5, 7]},
     {7, [6, 8]},
     {8, [7, 9]},
     {9, [8, 10]},
     {10, [9, 11]},
     {11, [10, 12]},
     {12, [11]}];
get(ring, 3) ->
    [{0, [2, 1]},
     {1, [0, 2]},
     {2, [1, 0]}];
get(ring, 5) ->
    [{0, [4, 1]},
     {1, [0, 2]},
     {2, [1, 3]},
     {3, [2, 4]},
     {4, [3, 0]}];
get(ring, 7) ->
    [{0, [6, 1]},
     {1, [0, 2]},
     {2, [1, 3]},
     {3, [2, 4]},
     {4, [3, 5]},
     {5, [4, 6]},
     {6, [5, 0]}];
get(ring, 13) ->
    [{0, [12, 1]},
     {1, [0, 2]},
     {2, [1, 3]},
     {3, [2, 4]},
     {4, [3, 5]},
     {5, [4, 6]},
     {6, [5, 7]},
     {7, [6, 8]},
     {8, [7, 9]},
     {9, [8, 10]},
     {10, [9, 11]},
     {11, [10, 12]},
     {12, [11, 0]}];
get(erdos_renyi, 13) ->
    [{0, [4, 6, 10]},
     {1, [2, 5, 8, 9, 12]},
     {2, [1, 3, 7, 9, 11, 12]},
     {3, [2, 8, 6]},
     {4, [0, 9]},
     {5, [1, 10, 11, 12]},
     {6, [0, 3, 11]},
     {7, [2, 9]},
     {8, [1, 3, 9]},
     {9, [1, 2, 4, 7, 8, 10, 11]},
     {10, [0, 5, 9]},
     {11, [2, 5, 6, 9]},
     {12, [1, 2, 5]}].

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
