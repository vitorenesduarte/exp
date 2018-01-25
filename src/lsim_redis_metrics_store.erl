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

-module(lsim_redis_metrics_store).
-author("Vitor Enes Duarte <vitorenesduarte@gmail.com").

-include("lsim.hrl").

-behaviour(gen_server).
-behaviour(lsim_metrics_store).

%% lsim_metrics_store callbacks
-export([start_link/0,
         put/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {redis}).

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec put(key(), value()) -> ok.
put(Key, Value) ->
    gen_server:call(?MODULE, {put, Key, Value}, infinity).

%% gen_server callbacks
init([]) ->
    {Host, Port} = get_redis_config(),
    {ok, Redis} = eredis:start_link(Host, Port),
    lager:info("lsim_redis_metrics_store initialized"),
    {ok, #state{redis=Redis}}.

handle_call({put, Key, Value}, _From, #state{redis=Redis}=State) ->
    {ok, <<"OK">>} = eredis:q(Redis, ["SET", Key, Value]),
    {reply, ok, State};

handle_call(Msg, _From, State) ->
    lager:warning("Unhandled call message: ~p", [Msg]),
    {noreply, State}.

handle_cast(Msg, State) ->
    lager:warning("Unhandled cast message: ~p", [Msg]),
    {noreply, State}.

handle_info(Msg, State) ->
    lager:warning("Unhandled info message: ~p", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
get_redis_config() ->
    case lsim_orchestration:get_task(redis, ?REDIS_PORT, false) of
        {ok, {_, IpAddress, Port}} ->
            Ip = inet_parse:ntoa(IpAddress),
            {Ip, Port};
        {error, not_connected} ->
            lager:info("Redis not connected. Trying again in 5 seconds."),
            timer:sleep(5000),
            get_redis_config()
    end.
