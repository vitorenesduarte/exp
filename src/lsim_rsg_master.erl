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

-module(lsim_rsg_master).
-author("Vitor Enes Duarte <vitorenesduarte@gmail.com").

-include("lsim.hrl").

-behaviour(gen_server).

%% lsim_simulation_runner callbacks
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).

-define(JOIN_INTERVAL, 2000).

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server callbacks
init([]) ->
    schedule_join(),
    ?LOG("lsim_rsg_master initialized"),
    {ok, #state{}}.

handle_call(Msg, _From, State) ->
    lager:warning("Unhandled call message: ~p", [Msg]),
    {noreply, State}.

handle_cast(Msg, State) ->
    lager:warning("Unhandled cast message: ~p", [Msg]),
    {noreply, State}.

handle_info(join, #state{}=State) ->
    MyName = ldb_config:id(),
    Nodes = lsim_discovery:nodes(),
    Overlay = lsim_config:get(lsim_overlay),

    case length(Nodes) == node_number() of
        true ->
            %% if all nodes are connected
            ToConnect = lsim_overlay:to_connect(MyName,
                                                Nodes,
                                                Overlay),
            ok = connect(ToConnect),
            %% @todo wait for everyone
            lsim_simulation_runner:start();
        false ->
            schedule_join()
    end,
    {noreply, State#state{}};
            
handle_info(Msg, State) ->
    lager:warning("Unhandled info message: ~p", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
node_number() ->
    lsim_config:get(lsim_node_number).

%% @private
schedule_join() ->
    timer:send_after(?JOIN_INTERVAL, join).

%% @private
connect([]) ->
    ok;
connect([Node|Rest]=All) ->
    case ldb_peer_service:join(Node) of
        ok ->
            connect(Rest);
        Error ->
            ?LOG("Couldn't connect to ~p. Reason ~p. Will try again in 5 seconds",
                 [Node, Error]),
                 timer:sleep(5000),
                 connect(All)
    end.
