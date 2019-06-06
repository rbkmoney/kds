-module(kds_keyring_storage_env).
-behaviour(kds_keyring_storage).

-export([create/1]).
-export([read/0]).
-export([update/1]).
-export([delete/0]).

-define(ENV_KEY, kds_keyring_storage_env).

-spec create(binary()) -> ok | {error, already_exists}.
create(Keyring) ->
    case application:get_env(kds, ?ENV_KEY) of
        undefined ->
            ok = application:set_env(kds, ?ENV_KEY, Keyring);
        {ok, _Something} ->
            {error, already_exists}
    end.

-spec read() -> {ok, binary()} | {error, not_found}.
read() ->
    case application:get_env(kds, ?ENV_KEY) of
        {ok, Keyring} ->
            {ok, Keyring};
        undefined ->
            {error, not_found}
    end.

-spec update(binary()) -> ok.
update(Keyring) ->
    ok = application:set_env(kds, ?ENV_KEY, Keyring).

-spec delete() -> ok.
delete() ->
    ok = application:unset_env(kds, ?ENV_KEY).
