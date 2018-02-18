-define(APP, lsim).
-type error() :: {error, atom()}.

%% peer service
-type ldb_node_id() :: node().
-type node_ip() :: inet:ip_address().
-type node_port() :: non_neg_integer().
-type node_spec() :: {ldb_node_id(), node_ip(), node_port()}.
-type handler() :: term(). %% module
-type message() :: term().
-type timestamp() :: non_neg_integer().

%% defaults
-define(DEFAULT_OVERLAY, hyparview).
-define(DEFAULT_MODE, state_based).

%% logging
-ifdef(debug).
-define(DEBUG(M), lager:info(M)).
-define(DEBUG(M, A), lager:info(M, A)).
-else.
-define(DEBUG(_M), ok).
-define(DEBUG(_M, _A), ok).
-endif.

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
