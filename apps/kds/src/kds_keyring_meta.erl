-module(kds_keyring_meta).

%% API
-export([get_keyring_meta_from_keyring_data/1]).
-export([get_changes/2]).
-export([update_meta/2]).
-export([update_add_meta/2]).
-export([validate_meta/1]).

-type keyring_meta() :: kds_keyring:keyring_meta(kds_keyring:key_id()).

-spec get_keyring_meta_from_keyring_data(kds_keyring:keyring_data()) -> keyring_meta().
get_keyring_meta_from_keyring_data(KeyringData) ->
    Keys = maps:get(keys, KeyringData),
    KeysMeta = maps:map(fun (_KeyId, _Key) -> #{retired => false} end, Keys),
    #{keys => KeysMeta}.

-spec get_changes(keyring_meta(), keyring_meta()) -> kds_keyring:keyring_meta_diff().
get_changes(#{keys := OldKeysMeta}, #{keys := NewKeysMeta}) ->
    UpdateKeysMeta = maps:fold(
        fun (K, V, Acc) ->
            case maps:get(K, OldKeysMeta, #{}) of
                V ->
                    Acc;
                _DifferentKeyMeta ->
                    Acc#{K => V}
            end
        end,
        #{}, NewKeysMeta
    ),
    case maps:size(UpdateKeysMeta) of
        0 ->
            #{};
        _UpdatedKeys ->
            #{keys => UpdateKeysMeta}
    end.

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

-spec update_add_meta(keyring_meta(), keyring_meta()) -> keyring_meta().
update_add_meta(#{keys := OldKeysMeta} = OldMeta, UpdateMeta) ->
    UpdateKeysMeta = maps:get(keys, UpdateMeta, #{}),
    NewKeysMeta = maps:fold(
        fun (K, V, Acc) ->
            OldKeyMeta = maps:get(K, OldKeysMeta, #{}),
            Acc#{K => maps:merge(OldKeyMeta, V)}
        end,
        OldKeysMeta, UpdateKeysMeta),
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