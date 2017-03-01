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

-module(lsim_kube_discovery).
-author("Vitor Enes Duarte <vitorenesduarte@gmail.com").

-include("lsim.hrl").

-behaviour(lsim_discovery).

-export([nodes/0]).

-spec nodes() -> [node_spec()].
nodes() ->
    Headers = headers(),
    URL = url(),
    lager:info("Headers ~p | URL ~p", [Headers, URL]),
    Options = [{body_format, binary}],
    DecodeFun = fun(Body) -> jsx:decode(Body, [return_maps]) end,

    Reply = case httpc:request(get, {URL, Headers}, [], Options) of
        {ok, {{_, 200, _}, _, Body}} ->
            {ok, DecodeFun(Body)};
        {error, Reason} ->
            lager:info("Couldn't get list of nodes. Reason ~p",
                       [Reason]),
            {error, invalid}
    end,

    lager:info("Reply ~p", [Reply]),

    generate_nodes(Reply).

%% @private
headers() ->
    Token = lsim_config:get(lsim_token),
    [{"Authorization", "Bearer " ++ Token}].

%% @private
url() ->
    APIServer = lsim_config:get(lsim_api_server),
    Timestamp = lsim_config:get(lsim_timestamp),

    lager:info("API Server ~p", [APIServer]),
    lager:info("Timestamp ~p", [Timestamp]),

    APIServer ++ "/api/v1/pods?labelSelector=timestamp%3D"
              ++ integer_to_list(Timestamp).

%% @private
generate_nodes(Reply) ->
    List = case Reply of
        {ok, Map} ->
            #{<<"items">> := Items} = Map,
            case Items of
                null ->
                    [];
                _ ->
                    Items
            end;
        _ ->
            []
    end,
    lager:info("List ~p", [List]),
    generate_spec(List).

%% @private
generate_spec(List) ->
    lists:map(
        fun(E) ->
            #{<<"spec">> := Spec} = E,
            IP = get_ip(Spec),
            Port = get_port(Spec),
            lager:info("Spec ~p", [Spec]),
            lager:info("IP ~p", [IP]),
            lager:info("Port ~p", [Port]),
            lsim_util:generate_spec(IP, Port)
        end,
        List
    ).

%% @private
get_ip(Spec) ->
    #{<<"status">> := Status} = Spec,
    #{<<"podIP">> := IP} = Status,
    decode(IP).

%% @private
get_port(Spec) ->
    #{<<"containers">> := [Container|_]} = Spec,
    #{<<"env">> := Envs} = Container,
    PortBinary = lists:foldl(
        fun(Env, Acc) ->
            #{<<"name">> := Name} = Env,
            #{<<"value">> := Value} = Env,
            case Name of
                "PEER_PORT" ->
                    Value;
                _ ->
                    Acc
            end
        end,
        undefined,
        Envs
    ),

    decode(PortBinary).

%% @private
decode(Binary) ->
    binary_to_list(Binary).
