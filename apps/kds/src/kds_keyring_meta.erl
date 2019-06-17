-module(kds_keyring_meta).

%% API
-export([get_keyring_meta_from_keyring_data/1]).

-spec get_keyring_meta_from_keyring_data(kds_keyring:keyring_data()) ->
    kds_keyring:keyring_meta(kds_keyring:key_id()).
get_keyring_meta_from_keyring_data(KeyringData) ->
    Keys = maps:get(keys, KeyringData),
    KeysMeta = maps:map(fun (_KeyId, _Key) -> #{retired => false} end, Keys),
    #{keys => KeysMeta}.