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

-module(lsim_util).
-author("Vitor Enes Duarte <vitorenesduarte@gmail.com").

-include("lsim.hrl").

%% lsim_util callbacks
-export([wait_until/3,
         read_lines/1]).

%% @todo add spec
%% @doc Wait until `Fun' returns true or `Retry' reaches 0.
%%      The sleep time between retries is `Delay'.
wait_until(_Fun, 0, _Delay) -> fail;
wait_until(Fun, Retry, Delay) when Retry > 0 ->
    case Fun() of
        true ->
            ok;
        _ ->
            timer:sleep(Delay),
            wait_until(Fun, Retry - 1, Delay)
    end.

%% @doc
-spec read_lines(string()) -> [string()].
read_lines(FilePath) ->
    {ok, FileDescriptor} = file:open(FilePath, [read]),
    Lines = get_lines(FilePath, FileDescriptor),
    Lines.

%% @private
get_lines(FilePath, FileDescriptor) ->
    case io:get_line(FileDescriptor, '') of
        eof ->
            [];
        {error, Error} ->
            lager:warning("Error while reading line from file ~p. Error: ~p", [FilePath, Error]),
            [];
        Line ->
            [Line | get_lines(FilePath, FileDescriptor)]
    end.
