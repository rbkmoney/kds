-module(kds_woody_event_handler).

-behaviour(woody_event_handler).

-include_lib("cds_proto/include/cds_proto_keyring_thrift.hrl").

%% woody_event_handler behaviour callbacks
-export([handle_event/4]).

%%
%% woody_event_handler behaviour callbacks
%%
-spec handle_event(Event, RpcId, Meta, Opts) ->
    ok
    when
    Event :: woody_event_handler:event(),
    RpcId :: woody:rpc_id() | undefined,
    Meta  :: woody_event_handler:event_meta(),
    Opts  :: woody:options().

handle_event(Event, RpcID, RawMeta, Opts) ->
    FilteredMeta = filter_meta(RawMeta),
    scoper_woody_event_handler:handle_event(Event, RpcID, FilteredMeta, Opts).

filter_meta(RawMeta) ->
    case RawMeta of
        #{result := {ok, [#'EncryptedMasterKeyShare'{} | _Rest] = EncryptedMasterKeyShares}} ->
            filter_encrypted_master_key_shares(EncryptedMasterKeyShares);
        #{result := [#'EncryptedMasterKeyShare'{} | _Rest] = EncryptedMasterKeyShares} ->
            filter_encrypted_master_key_shares(EncryptedMasterKeyShares);
        #{args := [ShareholderId, SignedShare]} when is_bitstring(SignedShare) ->
            RawMeta#{args => [ShareholderId, filter_jose(SignedShare)]};
        _ ->
            RawMeta
    end.

filter_encrypted_master_key_shares(EncryptedMasterKeyShares) ->
    lists:map(
        fun (EncryptedMasterKeyShare) ->
            EncryptedMasterKeyShare#'EncryptedMasterKeyShare'{encrypted_share = <<"***">>}
        end,
        EncryptedMasterKeyShares).

filter_jose(Str) ->
    re:replace(Str, "^eyJ([a-zA-Z0-9_-]*.?){4,6}", <<"***">>).
