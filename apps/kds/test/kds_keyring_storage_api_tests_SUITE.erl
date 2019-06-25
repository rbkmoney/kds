-module(kds_keyring_storage_api_tests_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("shamir/include/shamir.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_group/2]).
-export([end_per_group/2]).

-export([init_check_keyring/1]).
-export([locked_unlocked_check_keyring/1]).

-type config() :: term().

-spec test() -> _.

-spec all() -> [{group, atom()}].

all() ->
    [
        {group, basic_lifecycle}
    ].

-spec groups() -> [{atom(), list(), [atom()]}].

groups() ->
    [
        {basic_lifecycle, [sequence], [
            init_check_keyring,
            locked_unlocked_check_keyring
        ]}
    ].

%%
%% starting/stopping
%%

-spec init_per_group(atom(), config()) -> config().

init_per_group(_, C) ->
    C1 = kds_ct_utils:start_stash(C),
    C2 = kds_ct_utils:start_clear(C1),
    {ok, EncPrivateKey1} = file:read_file(filename:join(config(data_dir, C2), "enc.1.priv.json")),
    {ok, EncPrivateKey2} = file:read_file(filename:join(config(data_dir, C2), "enc.2.priv.json")),
    {ok, EncPrivateKey3} = file:read_file(filename:join(config(data_dir, C2), "enc.3.priv.json")),
    EncPrivateKeys = #{<<"1">> => EncPrivateKey1, <<"2">> => EncPrivateKey2, <<"3">> => EncPrivateKey3},
    {ok, SigPrivateKey1} = file:read_file(filename:join(config(data_dir, C2), "sig.1.priv.json")),
    {ok, SigPrivateKey2} = file:read_file(filename:join(config(data_dir, C2), "sig.2.priv.json")),
    {ok, SigPrivateKey3} = file:read_file(filename:join(config(data_dir, C2), "sig.3.priv.json")),
    SigPrivateKeys = #{<<"1">> => SigPrivateKey1, <<"2">> => SigPrivateKey2, <<"3">> => SigPrivateKey3},
    [
        {enc_private_keys, EncPrivateKeys},
        {sig_private_keys, SigPrivateKeys}
    ] ++ C2.

-spec end_per_group(atom(), config()) -> _.

end_per_group(_, C) ->
    kds_ct_utils:stop_clear(C).

-spec init_check_keyring(config()) -> _.

init_check_keyring(C) ->
    SSLOpts = [{cacertfile, cacertfile(C)}, {certfile, clientcertfile(C)}],
    _ = ?assertEqual(
        {error, {invalid_status, not_initialized}},
        kds_keyring_client:get_keyring(root_url(C), SSLOpts)
    ),
    _ = kds_ct_keyring:init(C),
    _ = ?assertMatch(
        #{
            meta := #{keys := #{0 := #{retired := false}}},
            data := #{current_key := 0, keys := #{0 := _Key0}}
        },
        kds_keyring_client:get_keyring(root_url(C), SSLOpts)
    ).

-spec locked_unlocked_check_keyring(config()) -> _.

locked_unlocked_check_keyring(C) ->
    SSLOpts = [{cacertfile, cacertfile(C)}, {certfile, clientcertfile(C)}],
    _ = ?assertMatch(
        #{
            meta := #{keys := #{0 := #{retired := false}}},
            data := #{current_key := 0, keys := #{0 := _Key0}}
        },
        kds_keyring_client:get_keyring(root_url(C), SSLOpts)
    ),
    _ = kds_ct_keyring:lock(C),
    _ = ?assertEqual(
        {error, {invalid_status, locked}},
        kds_keyring_client:get_keyring(root_url(C), SSLOpts)
    ),
    _ = kds_ct_keyring:unlock(C),
    _ = ?assertMatch(
        #{
            meta := #{keys := #{0 := #{retired := false}}},
            data := #{current_key := 0, keys := #{0 := _Key0}}
        },
        kds_keyring_client:get_keyring(root_url(C), SSLOpts)
    ).

%%
%% internal
%%

config(Key, Config) ->
    config(Key, Config, undefined).

config(Key, Config, Default) ->
    case lists:keysearch(Key, 1, Config) of
        {value, {Key, Val}} ->
            Val;
        _ ->
            Default
    end.

root_url(C) ->
    config(storage_root_url, C).

cacertfile(C) ->
    config(cacertfile, C).

clientcertfile(C) ->
    config(clientcertfile, C).
