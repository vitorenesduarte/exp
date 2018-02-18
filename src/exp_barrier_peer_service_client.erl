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

-module(exp_barrier_peer_service_client).
-author("Vitor Enes <vitorenesduarte@gmail.com").

-include("exp.hrl").

-behaviour(gen_server).

%% exp_barrier_peer_service_client callbacks
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {socket :: gen_tcp:socket()}).

-spec start_link(gen_tcp:socket()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Socket) ->
    gen_server:start_link(?MODULE, [Socket], []).

%% gen_server callbacks
init([Socket]) ->
    lager:info("exp_barrier_peer_service_client initialized! Node ~p listening to socket ~p",
               [ldb_config:id(), Socket]),
    {ok, #state{socket=Socket}}.

handle_call(Msg, _From, State) ->
    lager:warning("Unhandled call message: ~p", [Msg]),
    {noreply, State}.

handle_cast(Msg, State) ->
    lager:warning("Unhandled cast message: ~p", [Msg]),
    {noreply, State}.

handle_info({forward_message, _Handler, _Message}=M,
            #state{socket=Socket}=State) ->
    case gen_tcp:send(Socket, encode(M)) of
        ok ->
            ok;
        Error ->
            lager:info("Failed to send message: ~p", [Error])
    end,

    {noreply, State};

handle_info({tcp, _Socket, Data}, State) ->
    handle_message(decode(Data)),
    {noreply, State};

handle_info({tcp_closed, Socket}, State) ->
    lager:info("TCP closed ~p", [Socket]),
    {stop, normal, State};

handle_info(Msg, State) ->
    lager:warning("Unhandled info message: ~p", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
encode(Message) ->
    term_to_binary(Message).

%% @private
decode(Message) ->
    binary_to_term(Message).

%% @private
handle_message({forward_message, Handler, Message}) ->
    gen_server:cast(Handler, Message).
