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

-export([configure_tcp_keepalive/2,
         reject_ips/1,
         delete_rules/1]).

%% @todo remove
-export([tcpkill/1, kill_tcpkill/0]).

-define(CHAINS, ["INPUT", "OUTPUT"]).

%% @doc If number of partitions to be created is greater than 1,
%%      and not an rsg master, change tcp_keepalive values to:
%%          - net.ipv4.tcp_keepalive_time = 10
%%          - net.ipv4.tcp_keepalive_intvl = 5
%%          - net.ipv4.tcp_keepalive_probes = 1
%%
%%      With this, we'll detect dead TCP connections after
%%      15 seconds:
%%          - 10 seconds of inactivity
%%          - 1 probe after 5 seconds
%%
%% Source:
%% http://www.ehowstuff.com/configure-linux-tcp-keepalive-setting/
%%
-spec configure_tcp_keepalive(non_neg_integer(), boolean()) -> ok.
configure_tcp_keepalive(PartitionNumber, RSG) ->
    lager:info("P ~p, R ~p\n\n", [PartitionNumber, RSG]),
    ShouldConfigure = PartitionNumber > 1 andalso not RSG,

    case ShouldConfigure of
        true ->
            % change the defaults
            File = "/etc/sysctl.conf",
            Configs = [
                "net.ipv4.tcp_keepalive_time = 10",
                "net.ipv4.tcp_keepalive_intvl = 5",
                "net.ipv4.tcp_keepalive_probes = 1"
            ],

            lists:foreach(
                fun(Config) ->
                    CMD = "echo \"" ++ Config ++ "\""
                       ++ " | sudo tee -a " ++ File,

                    lager:info("CMD ~p", [CMD]),

                    exec(CMD)
                end,
                Configs
            ),

            %% reload settings
            exec("sudo sysctl -p");
        false ->
            ok
    end.

%% @doc Rejects a list of ips and
%%      returns the number of rules created.
-spec reject_ips(list(node_ip())) -> non_neg_integer().
reject_ips(IPs) ->
    partisan_static_peer_service_manager:close_connections(IPs),

    LastRule = lists:foldl(
        fun(IP, RuleAcc) ->


            IPStr = ip_to_str(IP),
            %tcpkill(IPStr),

            Rule = RuleAcc + 1,

            lists:foreach(
                fun(Chain) ->
                    CMD = "sudo iptables"
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

    LastRule.

-spec delete_rules(non_neg_integer()) -> ok.
delete_rules(LastRule) ->
    %kill_tcpkill(),
    lists:foreach(
        fun(_) ->
            lists:foreach(
                fun(Chain) ->
                    CMD = "sudo iptables"
                       %% delete in chain
                       ++ " --delete " ++ Chain
                       %% the first position
                       %% (since deleting one position
                       %%  results in all the other rules
                       %%  below being shifted up)
                       ++ " 1",

                    exec(CMD)
                end,
                ?CHAINS
            )
        end,
        lists:seq(1, LastRule)
    ).

%% @private
ip_to_str({A, B, C, D}) ->
    integer_to_list(A) ++ "." ++
    integer_to_list(B) ++ "." ++
    integer_to_list(C) ++ "." ++
    integer_to_list(D).

%% @private
tcpkill(IP) ->
    exec("tcpkill host " ++ IP ++ " &").

%% @private
kill_tcpkill() ->
    CMD = "netstat -anp | "
       ++ "grep tcpkill | "
       ++ "awk '{print $7}' |"
       ++ "cut -d'/' -f1 | "
       ++ "xargs kill",

    exec(CMD).



%% @private
exec(CMD) ->
    Result = os:cmd(CMD),

    case Result /= "" of
        true ->
            ?LOG("Exec ~p, Result ~p", [CMD, Result]);
        false ->
            ok
    end.
