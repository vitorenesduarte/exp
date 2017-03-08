#!/usr/bin/env escript

%%! -pa _build/default/lib/eredis/ebin/

main(_) ->
    Redis = redis_connection(),

    %% get all the redis keys
    {ok, Keys} = eredis:q(Redis, ["KEYS", "*"]),

    %% clear log folder
    os:cmd("rm -rf " ++ log_folder()),

    lists:foreach(
        fun(Filename) ->
            %% for all the keys (files), save them in the log folder
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
log_folder() ->
    "/tmp/logs".

%% @private
save(Filename, File) ->
    Path = get_path(Filename),
    ok = filelib:ensure_dir(Path),
    ok = file:write_file(Path, File).

%% @private
get_path(Filename) ->
    log_folder() ++ "/" ++ binary_to_list(Filename).
