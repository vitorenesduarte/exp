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

-module(exp_simulations).
-author("Vitor Enes <vitorenesduarte@gmail.com").

-include("exp.hrl").

-define(KEY, "events").
-define(GMAP_KEY_NUMBER, 1000).

%% exp_simulations callbacks
-export([get_specs/1]).

%% @doc
-spec get_specs(atom()) -> [term()].
get_specs(Simulation) ->
    Funs = case Simulation of
        undefined ->
            [];

        awset ->
            StartFun = fun() ->
                ldb:create(?KEY, awset)
            end,
            EventFun = fun(EventNumber, _NodeNumber, NodeEventNumber, _) ->
                Addition = EventNumber rem 4 /= 0,
                LastEvent = EventNumber == NodeEventNumber,

                %% if it's the last event,
                %% do an addition always,
                %% so that we have a way to
                %% detect when a node has
                %% observed all events
                case Addition orelse LastEvent of
                    true ->
                        Element = create_element(EventNumber),
                        ldb:update(?KEY, {add, Element});
                    false ->
                        %% remove an element added by me
                        {ok, Query} = ldb:query(?KEY),
                        ByMe = sets:to_list(
                            sets:filter(
                                fun(E) ->
                                    string:str(E, atom_to_list(ldb_config:id())) > 0
                                end,
                                Query
                            )
                        ),
                        Element = lists:nth(
                            rand:uniform(length(ByMe)),
                            ByMe
                        ),
                        ldb:update(?KEY, {rmv, Element})
                end
            end,
            TotalEventsFun = fun() ->
                {ok, Value} = ldb:query(?KEY),
                sets:size(Value)
            end,
            CheckEndFun = fun(NodeNumber, NodeEventNumber) ->
                {ok, Query} = ldb:query(?KEY),
                %% a node has observed all events
                %% if it has in the set
                %% `NodeNumber` elements ending in
                %% `NodeEventNumber`
                LastElements = sets:filter(
                    fun(E) ->
                        string:str(E, element_sufix(NodeEventNumber)) > 0
                    end,
                    Query
                ),
                sets:size(LastElements) == NodeNumber
            end,
            [StartFun,
             EventFun,
             TotalEventsFun,
             CheckEndFun];

        gcounter ->
            StartFun = fun() ->
                ldb:create(?KEY, gcounter)
            end,
            EventFun = fun(_EventNumber, _NodeNumber, _NodeEventNumber, _) ->
                ldb:update(?KEY, increment)
            end,
            TotalEventsFun = fun() ->
                {ok, Value} = ldb:query(?KEY),
                Value
            end,
            CheckEndFun = fun(NodeNumber, NodeEventNumber) ->
                TotalEventsFun() == NodeNumber * NodeEventNumber
            end,
            [StartFun,
             EventFun,
             TotalEventsFun,
             CheckEndFun];

        gset ->
            StartFun = fun() ->
                ldb:create(?KEY, gset)
            end,
            EventFun = fun(EventNumber, _NodeNumber, _NodeEventNumber, _) ->
                Element = create_element(EventNumber),
                ldb:update(?KEY, {add, Element})
            end,
            TotalEventsFun = fun() ->
                {ok, Value} = ldb:query(?KEY),
                sets:size(Value)
            end,
            CheckEndFun = fun(NodeNumber, NodeEventNumber) ->
                TotalEventsFun() == NodeNumber * NodeEventNumber
            end,
            [StartFun,
             EventFun,
             TotalEventsFun,
             CheckEndFun];

        gmap ->
            StartFun = fun() ->
                ldb:create(?KEY, lwwmap),
                ldb:create("gmap_events", gcounter),
                ldb_forward:update_ignore_keys(sets:from_list(["gmap_events"]))
            end,
            EventFun = fun(_EventNumber, NodeNumber, _NodeEventNumber, _) ->
                Percentage = exp_config:get(exp_gmap_simulation_key_percentage),
                KeysPerNode = round_up(?GMAP_KEY_NUMBER / NodeNumber),

                %% node with id i has keys in
                %% [i * KeysPerNode, ((i + 1) * KeysPerNode) - 1]
                NumericalId = exp_config:get(exp_numerical_id),
                Start = NumericalId * KeysPerNode + 1,
                End0 = ((NumericalId + 1) * KeysPerNode),
                %% since `End0' can be bigger than `?GMAP_KEY_NUMBER':
                End = min(?GMAP_KEY_NUMBER, End0),

                %% create my keys
                MyKeys0 = lists:seq(Start, End),

                %% shuffle keys
                MyKeys = exp_util:shuffle_list(MyKeys0),

                %% take the first `KeysPerIteration'
                KeysPerIteration = round_up((Percentage * KeysPerNode) / 100),
                Keys = lists:sublist(MyKeys, KeysPerIteration),

                Ops = lists:map(
                    fun(Key) ->
                        Timestamp = erlang:system_time(nanosecond),
                        Value = <<>>,
                        {set, Key, Timestamp, Value}
                    end,
                    Keys
                ),
                ldb:update(?KEY, Ops),
                ldb:update("gmap_events", increment)
            end,
            TotalEventsFun = fun() ->
                {ok, Value} = ldb:query("gmap_events"),
                Value
            end,
            CheckEndFun = fun(NodeNumber, NodeEventNumber) ->
                TotalEventsFun() == NodeNumber * NodeEventNumber
            end,
            [StartFun,
             EventFun,
             TotalEventsFun,
             CheckEndFun];

        retwis ->
            StartFun = fun() ->
                retwis_init(),
                ldb:create("retwis_events", gcounter),
                ldb_forward:update_ignore_keys(sets:from_list(["retwis_events"])),
                %% st for this experiment:
                ldb_metrics:new()
            end,
            EventFun = fun(_EventNumber, _NodeNumber, _NodeEventNumber, St0) ->
                St = retwis_event(St0),
                ldb:update("retwis_events", increment),
                St
            end,
            TotalEventsFun = fun() ->
                {ok, Value} = ldb:query("retwis_events"),
                Value
            end,
            CheckEndFun = fun(NodeNumber, NodeEventNumber) ->
                TotalEventsFun() == NodeNumber * NodeEventNumber
            end,
            [StartFun,
             EventFun,
             TotalEventsFun,
             CheckEndFun]

    end,

    create_spec(Funs).

%% @private
create_spec(Funs) ->
    case Funs of
        [] ->
            [];
        _ ->
            [{exp_simulation_runner,
              {exp_simulation_runner, start_link, [Funs]},
              permanent, 5000, worker, [exp_simulation_runner]}]
    end.

%% @private Create an unique element to be added to the set.
create_element(EventNumber) ->
    MyName = ldb_config:id(),
    atom_to_list(MyName) ++ element_sufix(EventNumber).

%% @private Create elements suffix.
element_sufix(EventNumber) ->
    "#" ++ integer_to_list(EventNumber).

%% @private Round up.
round_up(A) ->
    trunc(A) + 1.

-define(USER_NUMBER,    10000).
-define(POST_SIZE,      100).
-define(POST_ID_SIZE,   32).
-define(TIMELINE_POSTS, 10).

%% @private
%%  Sizes:
%%  - Post: 100 bytes
%%  - *Id:  32 bytes
%%
%%  Data structures:
%%  - UserId_followers: GSet<UserId>
%%  - UserId_posts:     LWWMap<PostId, Post>
%%  - UserId_timeline:  LWWMap<Timestamp, PostId>
%%
%%  Initial numbers:
%%  - ?USER_NUMBER users
%%
retwis_init() ->
    %% create all the keys that will ever exist
    lists:foreach(
        fun(UserId) ->
            ldb:create(followers_key(UserId), gset),
            ldb:create(posts_key(UserId),     lwwmap),
            ldb:create(timeline_key(UserId),  lwwmap)
        end,
        all_users()
    ),
    generate_experiment_ids().

%% @private
-spec generate_experiment_ids() -> list(integer()).
generate_experiment_ids() ->
    Zipf = exp_config:get(retwis_zipf),
    NodeEventNumber = exp_config:get(exp_node_event_number),
    %% each follow (15%), requires 2 users
    %% each post (35%), requires 1 user
    Per100 = 15 * 2 + 35,
    ImpossibleCase = Per100 * 2,
    IdsRequiredEstimation = round_up(NodeEventNumber * ImpossibleCase / 100),

    case Zipf of
        0 ->
            %% uniform
            [random_user() || _ <- lists:seq(1, IdsRequiredEstimation)];
        _ ->
            CMD = "python3 bin/zipf.py "
               ++ integer_to_list(?USER_NUMBER) ++ " "
               ++ float_to_list(Zipf / 100, [{decimals, 1}]) ++ " "
               ++ integer_to_list(IdsRequiredEstimation),
            Result = os:cmd(CMD),
            [list_to_integer(Id) || Id <- string:lexemes(Result, "\n")]
    end.

%% @private
%%  for timeline, ?TIMELINE_POSTS are read
%%  TODO does it matter this number?
-spec retwis_event({ldb_metrics:st(), list(integer())}) -> ldb_metrics:st().
retwis_event({MetricsSt0, NextIds0}) ->

    %% the following code does not look good on purpose
    %% - to avoid the pattern matching, we repeat code,
    %%   in principle, providing a more accurate measure
    case event_type() of
        follow ->
            {MicroSeconds, NextIds1} = timer:tc(fun retwis_follow/1, [NextIds0]),
            MetricsSt1 = ldb_metrics:record_latency(follow, MicroSeconds, MetricsSt0),
            {MetricsSt1, NextIds1};
        post ->
            {MicroSeconds, NextIds1} = timer:tc(fun retwis_post/1, [NextIds0]),
            MetricsSt1 = ldb_metrics:record_latency(post, MicroSeconds, MetricsSt0),
            {MetricsSt1, NextIds1};
        timeline ->
            {MicroSeconds, _} = timer:tc(fun retwis_timeline/0, []),
            MetricsSt1 = ldb_metrics:record_latency(timeline, MicroSeconds, MetricsSt0),
            {MetricsSt1, NextIds0}
    end.

%% @private
retwis_follow([User,NewFollowee | NextIds1]) ->
    ldb:update(followers_key(NewFollowee), {add, User}),
    NextIds1.

%% @private
retwis_post([User | NextIds1]) ->
    %% post data
    Post = create_post(),
    PostId = create_post_id(),
    Timestamp = erlang:system_time(nanosecond),

    %% create post
    ldb:update(posts_key(User), {set, PostId, Timestamp, Post}),

    %% get followers
    {ok, Followers} = ldb:query(followers_key(User)),

    %% add post to each follower timeline
    Op = {set, Timestamp, Timestamp, PostId},
    sets:fold(
        fun(Follower, _) -> ldb:update(timeline_key(Follower), Op) end,
        undefined,
        Followers
    ),
    NextIds1.

%% @private
retwis_timeline() ->
    User = random_user(),
    %% read 10 posts from timeline
    {ok, _} = ldb:query(timeline_key(User), [?TIMELINE_POSTS]).

%% @private
%%  Events:
%%  - follow user:   15%
%%  - post tweet:    35%
%%  - load timeline: 50%
-spec event_type() -> follow | post | timeline.
event_type() ->
    Random = rand:uniform(100),
    case Random =< 15 of
        true -> follow;
        false ->
            case Random =< 50 of
                true -> post;
                false -> timeline
            end
    end.

%% @private
-spec timeline_key(non_neg_integer()) -> string().
timeline_key(UserId) ->
    append_to_id(UserId, "_timeline").

%% @private
-spec followers_key(non_neg_integer()) -> string().
followers_key(UserId) ->
    append_to_id(UserId, "_followers").

%% @private
-spec posts_key(non_neg_integer()) -> string().
posts_key(UserId) ->
    append_to_id(UserId, "_posts").

%% @private
-spec append_to_id(non_neg_integer(), string()) -> string().
append_to_id(UserId, End) ->
    integer_to_list(UserId) ++ End.

-spec all_users() -> list(non_neg_integer()).
all_users() ->
    lists:seq(0, ?USER_NUMBER + 1).

-spec random_user() -> non_neg_integer().
random_user() ->
    rand:uniform(?USER_NUMBER).

-spec create_post() -> binary().
create_post() ->
    random_bytes(?POST_SIZE).

-spec create_post_id() -> binary().
create_post_id() ->
    random_bytes(?POST_ID_SIZE).

%% @doc Generate a random byte array.
-spec random_bytes(non_neg_integer()) -> binary().
random_bytes(Length) ->
    crypto:strong_rand_bytes(Length).
