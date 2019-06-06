-module(kds).
-behaviour(supervisor).
-behaviour(application).

%% API
-export([start/0]).
-export([stop /0]).

%% Supervisor callbacks
-export([init/1]).

%% Application callbacks
-export([start/2]).
-export([stop /1]).

%%
%% API
%%
-spec start() ->
    {ok, _}.
start() ->
    application:ensure_all_started(kds).

-spec stop() ->
    ok.
stop() ->
    application:stop(kds).


%%
%% Supervisor callbacks
%%

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {ok, IP} = inet:parse_address(application:get_env(kds, ip, "::")),
    HealthCheckers = genlib_app:env(?MODULE, health_checkers, []),
    Service = woody_server:child_spec(
        kds_thrift_service_sup,
        #{
            handlers => [
                kds_thrift_services:handler_spec(keyring_v2)
            ],
            event_handler     => scoper_woody_event_handler,
            ip                => IP,
            port              => genlib_app:env(?MODULE, port, 8022),
            transport_opts    => genlib_app:env(?MODULE, transport_opts, #{}),
            protocol_opts     => genlib_app:env(?MODULE, protocol_opts, #{}),
            shutdown_timeout  => genlib_app:env(?MODULE, shutdown_timeout, 0),
            additional_routes => [erl_health_handle:get_route(HealthCheckers)]
        }
    ),
    KeyringSupervisor = #{
        id => kds_keyring_sup,
        start => {kds_keyring_sup, start_link, []}
    },
    Procs = [
        Service,
        KeyringSupervisor
    ],
    {ok, {{one_for_one, 1, 5}, Procs}}.


%%
%% Application callbacks
%%
-spec start(normal, any()) ->
    {ok, pid()} | {error, any()}.
start(normal, _StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec stop(any()) ->
    ok.
stop(_State) ->
    ok.
