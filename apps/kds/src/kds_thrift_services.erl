-module(kds_thrift_services).

-export([handler_spec/1]).
-export([path/1]).
-export([thrift/1]).
-export([handler_module/1]).

%%
%% Types
%%

-type service_code() :: keyring_v2.

-export_type([service_code/0]).

%% Internal types

-type path() :: woody:path().
-type thrift_service() :: woody:service().
-type hadler_spec() :: woody:handler(list()).

-type service_hadler_spec() :: {path(), {thrift_service(), hadler_spec()}}.

%%
%% API
%%

-spec handler_spec(service_code()) -> service_hadler_spec().
handler_spec(Code) ->
    {path(Code), {thrift(Code), handler_module(Code)}}.

-spec path(service_code()) -> path().
path(keyring_v2) ->
    "/v2/keyring".

-spec thrift(service_code()) -> thrift_service().
thrift(keyring_v2) ->
    {cds_proto_keyring_thrift, 'Keyring'}.

-spec handler_module(service_code()) -> hadler_spec().
handler_module(keyring_v2) ->
    {kds_keyring_v2_thrift_handler, []}.
