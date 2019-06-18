-module(kds_keyring_meta).

%% API
-export([get_keyring_meta_from_keyring_data/1]).
-export([update_meta/2]).
-export([validate_meta/1]).

-type keyring_meta() :: kds_keyring:keyring_meta(kds_keyring:key_id()).

-spec get_keyring_meta_from_keyring_data(kds_keyring:keyring_data()) -> keyring_meta().
get_keyring_meta_from_keyring_data(KeyringData) ->
    Keys = maps:get(keys, KeyringData),
    KeysMeta = maps:map(fun (_KeyId, _Key) -> #{retired => false} end, Keys),
    #{keys => KeysMeta}.

-spec update_meta(keyring_meta(), keyring_meta()) -> keyring_meta().
update_meta(#{keys := OldKeysMeta} = OldMeta, UpdateMeta) ->
    KeysMeta = maps:get(keys, UpdateMeta, #{}),
    NewKeysMeta = maps:fold(
        fun (K, V, Acc) ->
            UpdateKeyMeta = maps:get(K, KeysMeta, #{}),
            Acc#{K => maps:merge(V, UpdateKeyMeta)}
        end,
        #{}, OldKeysMeta),
    OldMeta#{keys => NewKeysMeta}.

-spec validate_meta(keyring_meta()) -> boolean().
validate_meta(#{keys := KeysMeta}) ->
    lists:all(
        fun (KeyMeta) ->
            case KeyMeta of
                #{
                    retired := Retired
                } when is_boolean(Retired) ->
                    true;
                _InvalidKeyMeta ->
                    false
            end
        end,
        maps:values(KeysMeta));
validate_meta(_InvalidMeta) ->
    false.