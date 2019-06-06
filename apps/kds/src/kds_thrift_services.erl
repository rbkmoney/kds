-module(kds_thrift_services).

-export([handler_spec/1]).
-export([service_path/1]).
-export([thrift_service/1]).
-export([handler_module/1]).

%%
%% Types
%%

-type storage_code() :: keyring_v2.

-export_type([storage_code/0]).

%% Internal types

-type path() :: woody:path().
-type thrift_service() :: woody:service().
-type hadler_spec() :: woody:handler(list()).

-type service_hadler_spec() :: {path(), {thrift_service(), hadler_spec()}}.

%%
%% API
%%

-spec handler_spec(storage_code()) -> service_hadler_spec().
handler_spec(Code) ->
    {service_path(Code), {thrift_service(Code), handler_module(Code)}}.

-spec service_path(storage_code()) -> path().
service_path(keyring_v2) ->
    "/v2/keyring".

-spec thrift_service(storage_code()) -> thrift_service().
thrift_service(keyring_v2) ->
    {cds_proto_keyring_thrift, 'Keyring'}.

-spec handler_module(storage_code()) -> hadler_spec().
handler_module(keyring_v2) ->
    {kds_keyring_v2_thrift_handler, []}.
