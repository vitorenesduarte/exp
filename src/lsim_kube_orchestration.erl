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

-module(lsim_kube_orchestration).
-author("Vitor Enes Duarte <vitorenesduarte@gmail.com").

-include("lsim.hrl").

-behaviour(lsim_orchestration).

-export([rsg/1,
         nodes/1,
         stop/0]).

-spec rsg(node_port()) ->
    {ok, node_spec()} | {error, not_connected}.
rsg(Port) ->
    Nodes = get_tasks(rsg, Port),

    case Nodes of
        [] ->
            {error, not_connected};
        [RSG|_] ->
            {ok, RSG}
    end.

-spec nodes(node_port()) ->
    [node_spec()].
nodes(Port) ->
    get_tasks(lsim, Port).

-spec stop() ->
    ok.
stop() ->
    ?LOG("WILL DELETE"),
    delete_tasks(lsim),
    ok = delete_tasks(rsg),
    ok.

%% @private
get_tasks(Tag, Port) ->
    URL = get_url(Tag),
    Nodes = case http(get, URL) of
        {ok, N} ->
            N;
        {error, invalid} ->
            []
    end,

    generate_nodes(Nodes, Port).

%% @private
delete_tasks(Tag) ->
    URL = delete_url(Tag),
    ?LOG("WILL DELETE ~p", [URL]),

    case http(delete, URL) of
        {ok, Body} ->
            ?LOG("delete ok ~p", [Body]);
        {error, invalid} ->
            ?LOG("Delete failed. Trying again in 1 second"),
            timer:sleep(1000),
            delete_tasks(Tag)
    end.

%% @private
http(Method, URL) ->
    Headers = headers(),
    Options = [{body_format, binary}],
    DecodeFun = fun(Body) -> jsx:decode(Body, [return_maps]) end,

    case httpc:request(Method, {URL, Headers}, [], Options) of
        {ok, {{_, 200, _}, _, Body}} ->
            {ok, DecodeFun(Body)};
        {error, Reason} ->
            lager:info("Couldn't process ~p request ~p. Reason ~p",
                       [Method, URL, Reason]),
            {error, invalid}
    end.

%% @private
headers() ->
    Token = lsim_config:get(lsim_token),
    [{"Authorization", "Bearer " ++ Token}].

%% @private
get_url(Tag) ->
    APIServer = lsim_config:get(lsim_api_server),
    Timestamp = lsim_config:get(lsim_timestamp),

    APIServer ++ "/api/v1/pods?labelSelector="
              ++ "timestamp%3D" ++ integer_to_list(Timestamp)
              ++ ",tag%3D" ++ atom_to_list(Tag).

%% @private
delete_url(Tag) ->
    APIServer = lsim_config:get(lsim_api_server),
    Timestamp = lsim_config:get(lsim_timestamp),

    APIServer ++ "/apis/extensions/v1beta1/namespaces/default/deployments/"
              ++ atom_to_list(Tag) ++ "-"
              ++ integer_to_list(Timestamp)
              ++ "?gracePeriodSeconds=0".

%% @private
generate_nodes(Map, Port) ->
    #{<<"items">> := Items} = Map,
    List = case Items of
        null ->
            [];
        _ ->
            Items
    end,

    generate_spec(List, Port).

%% @private
generate_spec(List, Port) ->
    lists:map(
        fun(E) ->
            IP = get_ip(E),
            lsim_util:generate_spec(IP, Port)
        end,
        List
    ).

%% @private
get_ip(E) ->
    #{<<"status">> := Status} = E,
    #{<<"podIP">> := IP} = Status,
    decode(IP).

%% @private
decode(Binary) ->
    binary_to_list(Binary).
