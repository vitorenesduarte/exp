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

-module(lsim_iptables).
-author("Vitor Enes Duarte <vitorenesduarte@gmail.com").

-include("lsim.hrl").

-export([reject_ips/1,
         delete_rules/1]).

-define(BIN, "sudo iptables").
-define(CHAINS, ["INPUT", "OUTPUT"]).

-spec reject_ips(list(node_ip())) -> rules().
reject_ips(IPs) ->
    LastRule = lists:foldl(
        fun(IP, RuleAcc) ->
            IPStr = ip_to_str(IP),
            Rule = RuleAcc + 1,

            lists:foreach(
                fun(Chain) ->
                    CMD = ?BIN
                       %% insert in chain
                       ++ " --insert " ++ Chain
                       %% at this position
                       ++ " " ++ integer_to_list(Rule)
                       %% this rule
                       ++ " -s " ++ IPStr
                       ++ " -p tcp"
                       ++ " -j REJECT --reject-with tcp-reset",

                    exec(CMD)
                end,
                ?CHAINS
            ),

            Rule
        end,
        0,
        IPs
    ),

    lists:seq(1, LastRule).

-spec delete_rules(rules()) -> ok.
delete_rules(Rules) ->
    lists:foreach(
        fun(Rule) ->
            lists:foreach(
                fun(Chain) ->
                    CMD = ?BIN
                       %% delete in chain
                       ++ " --delete " ++ Chain
                       %% this position
                       ++ " " ++ integer_to_list(Rule),

                    exec(CMD)
                end,
                ?CHAINS
            )
        end,
        Rules
    ).

%% @private
ip_to_str({A, B, C, D}) ->
    integer_to_list(A) ++ "." ++
    integer_to_list(B) ++ "." ++
    integer_to_list(C) ++ "." ++
    integer_to_list(D).

%% @private
exec(CMD) ->
    Result = os:cmd(CMD),

    case Result /= "" of
        true ->
            ?LOG("Exec ~p, Result ~p", [CMD, Result]);
        false ->
            ok
    end.
