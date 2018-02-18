#!/usr/bin/env escript

%%! -pa _build/default/lib/eredis/ebin/

main(_) ->
    %% connect to redis
    redis(connect),

    %% get all the redis keys
    Keys = redis(fetch_keys),
    io:format("Found ~p keys!~n", [length(Keys)]),

    %% get all non-existing keys
    NonExisting = [Key || Key <- Keys, not file(exists, Key)],
    KeyNumber = length(NonExisting),
    io:format("Non-existing keys: ~p~n", [NonExisting]),

    lists:foreach(
        fun({Index, Key}) ->
            io:format("(~p of ~p)~n", [Index, KeyNumber]),
            File = redis(fetch_key, Key),
            file(save, Key, File)
        end,
        lists:zip(lists:seq(1, KeyNumber), NonExisting)
    ),

    ok.

%% @private
redis(connect) ->
    {ok, Redis} = eredis:start_link(),
    put(redis, Redis);
redis(fetch_keys) ->
    {ok, Keys} = eredis:q(get(redis), ["KEYS", "*"]),
    lists:map(fun(Key) -> binary_to_list(Key) end, Keys).
redis(fetch_key, Key) ->
    {ok, File} = eredis:q(get(redis), ["GET", Key], infinity),
    File.

%% @private
file(path, Key) ->
    filename:join(
        os:getenv("METRICS_DIR"),
        Key
    );
file(exists, Key) ->
    Path = file(path, Key),
    filelib:is_file(Path).
file(save, Key, File) ->
    Path = file(path, Key),
    ok = filelib:ensure_dir(Path),
    ok = file:write_file(Path, File).
