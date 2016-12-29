-define(APP, lsim).
-type error() :: {error, atom()}.

%% peer service
-type ldb_node_id() :: node().
-type node_ip() :: inet:ip_address().
-type node_port() :: non_neg_integer().
-type node_spec() :: {ldb_node_id(), node_ip(), node_port()}.
-type handler() :: term(). %% module
-type message() :: term().
-define(TCP_OPTIONS, [binary, {active, true}, {packet, 4}, {keepalive, true}]).

%% defaults
-define(DEFAULT_OVERLAY, hyparview).
-define(DEFAULT_MODE, state_based).
