#!/usr/bin/env escript

%%! -pa _build/default/lib/eredis/ebin/

main(_) ->
    Redis = redis_connection(),

    %% get all the redis keys
    {ok, Keys} = eredis:q(Redis, ["KEYS", "*"]),

    %% clear metrics dir
    os:cmd("rm -rf " ++ metrics_dir()),

    lists:foreach(
        fun(Filename) ->
            %% for all the keys (files), save them in the metrics dir
            {ok, File} = eredis:q(Redis, ["GET", Filename]),
            save(Filename, File)
        end,
        Keys
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
