-module(kds_keyring_meta).

-include_lib("cds_proto/include/cds_proto_keyring_thrift.hrl").

%% API
-export([get_default_keyring_meta/1]).
-export([get_changes/2]).
-export([update_meta/2]).
-export([update_add_meta/2]).
-export([validate_meta/1]).
-export([changes_will_be_made/2]).
-export([decode_keyring_meta/1]).
-export([encode_keyring_meta/1]).

-type keyring_meta() :: kds_keyring:keyring_meta(kds_keyring:key_id()).
-type encoded_keyring_meta() :: #'KeyringMeta'{}.

-spec get_default_keyring_meta(kds_keyring:keyring_data()) -> keyring_meta().
get_default_keyring_meta(KeyringData) ->
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

-spec validate_meta(keyring_meta()) -> ok.
validate_meta(#{keys := KeysMeta}) ->
    lists:foreach(fun validate_key_meta/1, maps:values(KeysMeta)).

validate_key_meta(KeyMeta) ->
    ok = validate_retired(KeyMeta),
    ok.

validate_retired(#{retired := Retired}) when is_boolean(Retired) ->
    ok;
validate_retired(#{retired := _Retired}) ->
    throw({validation_failed, <<"field \'retired\' isn't boolean">>}).

-spec changes_will_be_made(keyring_meta(), keyring_meta()) -> ok.
changes_will_be_made(OldKeyringMeta, UpdateMeta) ->
    case update_meta(OldKeyringMeta, UpdateMeta) of
        OldKeyringMeta ->
            throw(no_changes);
        _NewKeyringMeta ->
            ok
    end.

-spec decode_keyring_meta(encoded_keyring_meta()) -> keyring_meta().
decode_keyring_meta(#'KeyringMeta'{
    keys_meta = KeysMeta
}) ->
    DecodedKeysMeta = maps:fold(
        fun (K, #'KeyMeta'{retired = Retired}, Acc) ->
            Acc#{K => #{retired => Retired}}
        end,
        #{},
        KeysMeta),
    #{keys => DecodedKeysMeta}.

-spec encode_keyring_meta(keyring_meta() | undefined) -> encoded_keyring_meta().
encode_keyring_meta(undefined) ->
    #'KeyringMeta'{keys_meta = #{}};
encode_keyring_meta(#{
    keys := KeysMeta
}) ->
    EncodedKeysMeta = maps:fold(
        fun (K, #{retired := Retired}, Acc) ->
            Acc#{K => #'KeyMeta'{retired = Retired}}
        end,
        #{},
        KeysMeta
    ),
    #'KeyringMeta'{keys_meta = EncodedKeysMeta}.