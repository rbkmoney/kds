-module(kds_keyring_meta).

-include_lib("cds_proto/include/cds_proto_keyring_thrift.hrl").

%% API
-export([get_default_keyring_meta/1]).
-export([update_meta/2]).
-export([decode_keyring_meta/1]).
-export([encode_keyring_meta/1]).

-export_type([keyring_meta/0]).
-export_type([keyring_meta_diff/0]).

-type keyring_meta() :: #{
    version := pos_integer(),
    keys := #{
        key_id() => key_meta()
    }
}.
-type keyring_meta_diff() :: #{
    keys => #{
        key_id() => key_meta()
    }
}.
-type key_meta() :: #{
    retired := boolean()
}.
-type key_id() :: kds_keyring:key_id().
-type encoded_keyring_meta() :: #'KeyringMeta'{}.

-spec get_default_keyring_meta(kds_keyring:keyring_data()) -> keyring_meta().
get_default_keyring_meta(KeyringData) ->
    Keys = maps:get(keys, KeyringData),
    KeysMeta = maps:map(fun (_KeyId, _Key) -> #{retired => false} end, Keys),
    #{version => 1, keys => KeysMeta}.

-spec update_meta(keyring_meta(), keyring_meta_diff()) -> keyring_meta().
update_meta(#{version := Version, keys := OldKeysMeta} = OldMeta, UpdateMeta) ->
    KeysMeta = maps:get(keys, UpdateMeta, #{}),
    NewKeysMeta = maps:fold(
        fun (K, V, Acc) ->
            UpdateKeyMeta = maps:get(K, KeysMeta, #{}),
            Acc#{K => maps:merge(V, UpdateKeyMeta)}
        end,
        #{}, OldKeysMeta),
    case OldMeta#{keys => NewKeysMeta} of
        OldMeta ->
            OldMeta;
        NewMeta ->
            NewMeta#{version => Version + 1}
    end.

-spec decode_keyring_meta(encoded_keyring_meta()) -> keyring_meta_diff().
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