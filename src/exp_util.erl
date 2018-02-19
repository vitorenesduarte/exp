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

-module(exp_util).
-author("Vitor Enes <vitorenesduarte@gmail.com").

-include("exp.hrl").

-export([generate_spec/2,
         shuffle_list/1]).

%% @doc Given an IP string and port string
%%      genenerate the node spec.
-spec generate_spec(list(), node_port()) -> node_spec().
generate_spec(IpStr, Port) ->
    NameStr = "exp-" ++ integer_to_list(?PORT) ++ "@" ++ IpStr,

    ParsedName = list_to_atom(NameStr),
    {ok, ParsedIp} = inet_parse:address(IpStr),

    {ParsedName, ParsedIp, Port}.

%% @doc Shuffle a list.
-spec shuffle_list(list()) -> list().
shuffle_list(L) ->
    rand:seed(exsplus, erlang:timestamp()),
    lists:map(
        fun({_, E}) -> E end,
        lists:sort(
            lists:map(
                fun(E) -> {rand:uniform(), E} end, L
            )
        )
    ).
