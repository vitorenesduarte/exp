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
         numerical_id_and_neighbors/3,
         break_links/3]).

%% @doc The first argument can be:
%%          - `hyparview'
%%          - `ring'
%%          - `fullmesh'
%%          - `line'
%%      The second argument is the number of nodes.
-spec get(atom(), pos_integer()) -> orddict:orddict().
get(_, 1) ->
    [];
get(fullmesh, N) ->
    All = lists:seq(0, N - 1),
    lists:foldl(
        fun(I, Acc) ->
            orddict:store(I, All -- [I], Acc)
        end,
        orddict:new(),
        All
    );
get(tree, 14) ->
    %% automatically generated by topologies.py
    [{0, [6]}, {1, [5, 11, 10]}, {2, [7]}, {3, [7]}, {4, [6]}, {5, [1]}, {6, [0, 4, 11]}, {7, [3, 2, 12]}, {8, [9]}, {9, [12, 13, 8]}, {10, [1]}, {11, [1, 12, 6]}, {12, [9, 7, 11]}, {13, [9]}];
get(chord, 16) ->
    %% automatically generated by topologies.py
    [{0, [14, 15, 4, 12, 1, 2]}, {1, [3, 15, 2, 5, 13, 0]}, {2, [1, 6, 3, 14, 0, 4]}, {3, [15, 1, 7, 2, 5, 4]}, {4, [8, 0, 5, 6, 3, 2]}, {5, [6, 1, 4, 9, 3, 7]}, {6, [5, 7, 10, 2, 8, 4]}, {7, [11, 3, 6, 9, 8, 5]}, {8, [4, 9, 10, 6, 12, 7]}, {9, [8, 11, 7, 13, 5, 10]}, {10, [11, 6, 8, 12, 9, 14]}, {11, [10, 7, 9, 13, 12, 15]}, {12, [13, 14, 0, 10, 11, 8]}, {13, [12, 11, 9, 1, 15, 14]}, {14, [0, 15, 12, 2, 13, 10]}, {15, [3, 1, 0, 14, 13, 11]}];
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
-spec numerical_id_and_neighbors(ldb_node_id(), list(node_spec()), atom()) ->
    {non_neg_integer(), list(node_spec())}.
numerical_id_and_neighbors(MyName, Nodes, Overlay) ->
    NodeNumber = length(Nodes),
    Sorted = lists:sort(Nodes),

    NumericalId = numerical_id(MyName, Sorted),

    %% id -> [id]
    Topology = get(Overlay, NodeNumber),

    {NumericalId, [lists:nth(I + 1, Sorted) || I <- orddict:fetch(NumericalId, Topology)]}.

%% @doc Given break links configuration,
%%      a list of node specs and a overlay,
%%      returns a tuple:
%%      {names of interesting (we want metrics from) nodes, map from name to links to break}
-spec break_links(none | one | log | half,
                  [node_spec()],
                  atom()) ->
    {[ldb_node_id()], [{ldb_node_id(), [node_spec()]}]}.
break_links(none, Nodes, _Overlay) ->
    %% all nodes are interesting
    {Names, _, _} = lists:unzip3(Nodes),
    %% and no links to break
    Links = [],
    {Names, Links};
break_links(one, Nodes, Overlay) ->
    NodeNumber = length(Nodes),
    {AId, BId} = get_predefined_link(Overlay, NodeNumber),

    Sorted = lists:sort(Nodes),

    {AName, _, _}=A = lists:nth(AId + 1, Sorted),
    {BName, _, _}=B = lists:nth(BId + 1, Sorted),

    Names = [AName, BName],
    Links = [{AName, [B]},
             {BName, [A]}],

    {Names, Links}.

%% @private Get numerical id, given the name a list of sorted specs by name.
-spec numerical_id(ldb_node_id(), list(node_spec())) -> non_neg_integer().
numerical_id(MyName, Sorted) ->
    %% compute id
    lists:foldl(
        fun({Name, _, _}, Acc) ->
            case Name < MyName of
                true ->
                    Acc + 1;
                false ->
                    Acc
            end
        end,
        0,
        Sorted
    ).

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
%% automatically generated
get_predefined_link(fullmesh, 2) ->
    {0, 1};
get_predefined_link(tree, 14) ->
    {11, 12};
get_predefined_link(chord, 16) ->
    {10, 14}.

