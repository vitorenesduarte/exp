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

-module(lsim_resource).
-author("Vitor Enes Duarte <vitorenesduarte@gmail.com").

-include("lsim.hrl").

-behaviour(gen_server).

%% lsim_resource callbacks
-export([start_link/0,
         update_membership/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% mochiweb callbacks
-export([loop/1]).

-record(state, {members :: list(ldb_node_id())}).

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec update_membership(sets:set(ldb_node_id())) -> ok.
update_membership(Membership) ->
    gen_server:call(?MODULE,
                    {update_membership, Membership},
                    infinity).

%% gen_server callbacks
init([]) ->
    ?LOG("lsim_resource initialized"),

    Loop = fun(Req) ->
        ?MODULE:loop(Req)
    end,
    mochiweb_http:start([{loop, Loop} | ?WEB_CONFIG]),

    {ok, #state{members=[]}}.

handle_call({update_membership, Membership}, _From, _State) ->
    Members0 = [Name || {Name, _, _} <- sets:to_list(Membership)],
    Members = Members0 -- [ldb_config:id()],

    State = #state{members=Members},
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

%% mochiweb
loop(Req) ->
    lager:info(Req).
