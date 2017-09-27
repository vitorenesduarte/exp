-define(APP, lsim).
-type error() :: {error, atom()}.

%% peer service
-type lsim_node_id() :: node().
-type node_ip() :: inet:ip_address().
-type node_port() :: non_neg_integer().
-type node_spec() :: {lsim_node_id(), node_ip(), node_port()}.
-type handler() :: term(). %% module
-type message() :: term().
-type timestamp() :: non_neg_integer().

%% defaults
-define(DEFAULT_OVERLAY, trcb).
-define(DEFAULT_MODE, trcb).

%% logging
-define(LOGGING, list_to_atom("true")). %% dialyzer
-define(LOG(S),
        ?LOG(S, [])
       ).
-define(LOG(S, Args),
        case ?LOGGING of
            true ->
                lager:info(S, Args);
            false ->
                ok
        end
       ).

%% barrier
-define(PORT, 6866).
-define(BARRIER_PORT, 6867).
-define(REDIS_PORT, 6379).
-define(TCP_OPTIONS, [binary, {active, true}, {packet, 4}, {keepalive, true}]).

%% web config
-define(WEB_IP, "0.0.0.0").
-define(WEB_PORT, 8080).
-define(WEB_CONFIG, [{ip, ?WEB_IP},
                     {port, ?WEB_PORT}]).

%% logs
-type key() :: list().
-type value() :: binary().
