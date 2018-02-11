#!/usr/bin/env escript

%%! -pa _build/default/lib/eredis/ebin/

main(_) ->
    Redis = redis_connection(),

    %% get all the redis keys
    {ok, Keys} = eredis:q(Redis, ["KEYS", "*"]),

    %% clear metrics dir
    %os:cmd("rm -rf " ++ metrics_dir()),

    io:format("Keys found ~p~n", [Keys]),
    KeyNumber = length(Keys),

    lists:foreach(
        fun(Index) ->
            Filename = lists:nth(Index, Keys),
            io:format("(~p of ~p) Fetching key ~p~n",
                      [Index, KeyNumber, Filename]),
            %% for all the keys (files), save them in the metrics dir
            {ok, File} = eredis:q(Redis,
                                  ["GET", Filename],
                                  infinity),
            save(Filename, File)
        end,
        lists:seq(1, KeyNumber)
    ),

    ok.

%% @private
redis_connection() ->
    {ok, Redis} = eredis:start_link(),
    Redis.

%% @private
metrics_dir() ->
    os:getenv("METRICS_DIR").

%% @private
save(Filename, File) ->
    Path = get_path(Filename),
    ok = filelib:ensure_dir(Path),
    ok = file:write_file(Path, File).

%% @private
get_path(Filename) ->
    metrics_dir() ++ "/" ++ binary_to_list(Filename).
