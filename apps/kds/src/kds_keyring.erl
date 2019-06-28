-module(kds_keyring).

-export([new/0]).
-export([rotate/1]).
-export([incr_version/1]).
-export([get_key/2]).
-export([get_keys/1]).
-export([get_current_key/1]).

-export([get_changes/2]).
-export([apply_changes/2]).

-export([encrypt/2]).
-export([decrypt/2]).
-export([marshall/1]).
-export([unmarshall/1]).

-export([validate_masterkey/3]).
-export([validate_masterkey/2]).

-export_type([key/0]).
-export_type([key_id/0]).
-export_type([keyring/0]).
-export_type([keyring_diff/0]).
-export_type([encrypted_keyring_diff/0]).
-export_type([keyring_data/0]).
-export_type([keyring_meta/0]).
-export_type([keyring_meta_diff/0]).
-export_type([encrypted_keyring/0]).

-type masterkey() :: kds_keysharing:masterkey().
-type key() :: binary().
-type key_id() :: byte().
-type encrypted_keyring() :: #{
    data := binary(),
    meta := keyring_meta() | undefined
}.

-type keyring_diff() :: #{
    data => keyring_data(),
    meta => keyring_meta_diff()
}.

-type encrypted_keyring_diff() :: #{
    data => binary(),
    meta => keyring_meta_diff()
}.

-type key_meta() :: #{
    retired := boolean()
}.
-type keyring_meta() :: #{
    version := pos_integer(),
    keys := #{
        key_id() => key_meta()
    }
}.
-type keyring_meta_diff() :: #{
    version => pos_integer(),
    keys => #{
        key_id() => key_meta()
    }
}.

-type keyring() :: #{
    data := keyring_data(),
    meta := keyring_meta()
}.

-type keyring_data() :: #{
    current_key := key_id(),
    keys := #{key_id() => key()}
}.

-define(KEY_BYTESIZE, 32).

%%

-spec new() -> keyring().
new() ->
    #{
        data => #{
            current_key => 0,
            keys => #{0 => kds_crypto:key()}
        },
        meta => #{
            version => 1,
            keys => #{
                0 => #{
                    retired => false
                }
            }
        }
    }.

-spec rotate(keyring()) -> keyring().
rotate(#{data := #{current_key := CurrentKeyId, keys := Keys}, meta := #{version := Version, keys := KeysMeta}}) ->
    <<NewCurrentKeyId>> = <<(CurrentKeyId + 1)>>,
    case maps:is_key(NewCurrentKeyId, Keys) of
        false ->
            #{
                data => #{
                    current_key => NewCurrentKeyId,
                    keys => Keys#{NewCurrentKeyId => kds_crypto:key()}
                },
                meta => #{
                    version => Version,
                    keys => KeysMeta#{NewCurrentKeyId => #{retired => false}}
                }
            };
        true ->
            throw(keyring_full)
    end.

-spec incr_version(keyring()) -> keyring();
    (encrypted_keyring()) -> encrypted_keyring().
incr_version(#{meta := #{version := Version} = KeyringMeta} = Keyring) ->
    Keyring#{
        meta => KeyringMeta#{
            version => Version + 1
        }
    }.

-spec get_key(key_id(), keyring()) -> {ok, {key_id(), key()}} | {error, not_found}.
get_key(KeyId, #{data := #{keys := Keys}}) ->
    case maps:find(KeyId, Keys) of
        {ok, Key} ->
            {ok, {KeyId, Key}};
        error ->
            {error, not_found}
    end.

-spec get_keys(keyring()) -> [{key_id(), key()}].
get_keys(#{data := #{keys := Keys}}) ->
    maps:to_list(Keys).

-spec get_current_key(keyring()) -> {key_id(), key()}.
get_current_key(#{data := #{current_key := CurrentKeyId, keys := Keys}}) ->
    CurrentKey = maps:get(CurrentKeyId, Keys),
    {CurrentKeyId, CurrentKey}.

-spec get_changes(keyring(), keyring()) -> keyring_diff();
    (encrypted_keyring(), encrypted_keyring()) -> encrypted_keyring_diff().
get_changes(#{meta := OldMeta}, #{data := NewData, meta := NewMeta}) ->
    #{
        data => NewData,
        meta => kds_keyring_meta:get_changes(OldMeta, NewMeta)
    }.

-spec apply_changes(keyring(), keyring_diff()) -> keyring();
    (encrypted_keyring(), encrypted_keyring_diff()) -> encrypted_keyring().
apply_changes(#{data := OldData, meta := OldMeta}, DiffKeyring) ->
    DiffMeta = maps:get(meta, DiffKeyring, #{}),
    #{
        data => maps:get(data, DiffKeyring, OldData),
        meta => kds_keyring_meta:update_add_meta(OldMeta, DiffMeta)
    }.

%%

-spec encrypt(key(), keyring()) -> encrypted_keyring().
encrypt(MasterKey, #{data := KeyringData, meta := KeyringMeta}) ->
    #{
        data => base64:encode(kds_crypto:encrypt(MasterKey, marshall(KeyringData))),
        meta => KeyringMeta
    }.

-spec decrypt(key(), encrypted_keyring()) -> {ok, keyring()} | {error, decryption_failed}.
decrypt(MasterKey, #{data := EncryptedKeyringData, meta := KeyringMeta}) ->
    try unmarshall(kds_crypto:decrypt(MasterKey, base64:decode(EncryptedKeyringData))) of
        KeyringData ->
            case KeyringMeta of
                undefined ->
                    {ok, #{
                        data => KeyringData,
                        meta => kds_keyring_meta:get_default_keyring_meta(KeyringData)
                    }};
                _ ->
                    {ok, #{data => KeyringData, meta => KeyringMeta}}
            end
    catch
        decryption_failed ->
            {error, decryption_failed}
    end.

-spec marshall(keyring_data()) -> binary().
marshall(#{current_key := CurrentKey, keys := Keys}) ->
    <<CurrentKey, (maps:fold(fun marshall_keys/3, <<>>, Keys))/binary>>.

-spec unmarshall(binary()) -> keyring_data().
unmarshall(<<CurrentKey, Keys/binary>>) ->
    #{current_key => CurrentKey, keys => unmarshall_keys(Keys, #{})}.

-spec marshall_keys(key_id(), key(), binary()) -> binary().
marshall_keys(KeyId, Key, Acc) ->
    <<Acc/binary, KeyId, Key:?KEY_BYTESIZE/binary>>.

-spec unmarshall_keys(binary(), map()) -> map().
unmarshall_keys(<<>>, Acc) ->
    Acc;
unmarshall_keys(<<KeyId, Key:?KEY_BYTESIZE/binary, Rest/binary>>, Acc) ->
    unmarshall_keys(Rest, Acc#{KeyId => Key}).

-spec validate_masterkey(masterkey(), keyring(), encrypted_keyring()) ->
    {ok, keyring()} | {error, wrong_masterkey}.
validate_masterkey(MasterKey, Keyring, EncryptedOldKeyring) ->
    case decrypt(MasterKey, EncryptedOldKeyring) of
        {ok, Keyring} ->
            {ok, Keyring};
        {ok, _NotMatchingKeyring} ->
            {error, wrong_masterkey};
        {error, decryption_failed} ->
            {error, wrong_masterkey}
    end.

-spec validate_masterkey(masterkey(), encrypted_keyring()) ->
    {ok, keyring()} | {error, wrong_masterkey}.
validate_masterkey(MasterKey, EncryptedOldKeyring) ->
    case decrypt(MasterKey, EncryptedOldKeyring) of
        {ok, Keyring} ->
            {ok, Keyring};
        {error, decryption_failed} ->
            {error, wrong_masterkey}
    end.
