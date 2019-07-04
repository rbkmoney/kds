-module(kds_keyring_storage_thrift_handler).
-behaviour(woody_server_thrift_handler).

-include_lib("cds_proto/include/cds_proto_keyring_thrift.hrl").

%% woody_server_thrift_handler callbacks
-export([handle_function/4]).

%%
%% woody_server_thrift_handler callbacks
%%

-spec handle_function(woody:func(), woody:args(), woody_context:ctx(), woody:options()) ->
    {ok, woody:result()} | no_return().

handle_function(OperationID, Args, Context, Opts) ->
    scoper:scope(
        keyring_storage,
        fun() -> handle_function_(OperationID, Args, Context, Opts) end
    ).

handle_function_('GetKeyring', [], _Context, _Opts) ->
    try kds_keyring_manager:get_keyring() of
        Keyring ->
            {ok, encode_keyring(Keyring)}
    catch
        {invalid_status, Status} ->
            raise(#'InvalidStatus'{status = Status})
    end.

encode_keyring(#{
    data := #{
        keys := Keys
    },
    meta := #{
        current_key_id := CurrentKeyId,
        version := Version,
        keys := KeysMeta
    }
}) ->
    #'Keyring'{
        version = Version,
        current_key_id = CurrentKeyId,
        keys = encode_keys(Keys, KeysMeta)
    }.

encode_keys(Keys, KeysMeta) ->
    maps:fold(
        fun(K, V, Acc) ->
            #{
                retired := Retired,
                security_parameters := #{
                    scrypt_opts := #{
                        n := ScryptN,
                        r := ScryptR,
                        p := ScryptP
                    }
                }
            } = maps:get(K, KeysMeta),
            Acc#{K => #'Key'{
                data = V,
                meta = #'KeyMeta'{
                    retired = Retired,
                    security_parameters = #'SecurityParameters'{
                        deduplication_hash_opts = #'ScryptOptions'{
                            n = ScryptN,
                            r = ScryptR,
                            p = ScryptP
                        }
                    }
                }
            }}
        end,
        #{}, Keys).

-spec raise(_) -> no_return().
raise(Exception) ->
    woody_error:raise(business, Exception).
