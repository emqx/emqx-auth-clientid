%% Copyright (c) 2018 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(emqx_auth_clientid).

-behaviour(emqx_auth_mod).

-include_lib("emqx/include/emqx.hrl").

-export([add_clientid/2, lookup_clientid/1, remove_clientid/1, all_clientids/0]).

%% emqx_auth_mod callbacks
-export([init/1, check/3, description/0]).

-define(TAB, ?MODULE).
-record(?TAB, {client_id, password}).
-record(state, {hash_type}).

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

%% @doc Add clientid with password
-spec(add_clientid(binary(), binary()) -> {atomic, ok} | {aborted, any()}).
add_clientid(ClientId, Password) ->
    R = #?TAB{client_id = ClientId, password = encrypted_data(Password)},
    mnesia:transaction(fun mnesia:write/1, [R]).

%% @doc Lookup clientid
-spec(lookup_clientid(binary()) -> list(#?TAB{})).
lookup_clientid(ClientId) ->
    mnesia:dirty_read(?TAB, ClientId).

%% @doc Lookup all clientids
-spec(all_clientids() -> list(binary())).
all_clientids() ->
    mnesia:dirty_all_keys(?TAB).

%% @doc Remove clientid
-spec(remove_clientid(binary()) -> {atomic, ok} | {aborted, term()}).
remove_clientid(ClientId) ->
    ret(mnesia:transaction(fun mnesia:delete/1, [{?TAB, ClientId}])).

ret({atomic, ok})     -> ok;
ret({aborted, Error}) -> {error, Error}.
%%------------------------------------------------------------------------------
%% emqx_auth_mod callbacks
%%------------------------------------------------------------------------------

init({ClientList, HashType}) ->
    ok = ekka_mnesia:create_table(?TAB, [
            {disc_copies, [node()]},
            {attributes, record_info(fields, ?TAB)}]),
    ok = ekka_mnesia:copy_table(?TAB, disc_copies),
    State = #state{hash_type = HashType},
    Clients = [r(ClientId, Password, HashType) || {ClientId, Password} <- ClientList],
    mnesia:transaction(fun() -> [mnesia:write(C) || C <- Clients] end),
    {ok, State}.

r(ClientId, Password, HashType) ->
    #?TAB{client_id = iolist_to_binary(ClientId),
          password  = hash(iolist_to_binary(Password), HashType)}.

check(#{client_id := undefined}, _Password, _) ->
    {error, clientid_undefined};
check(_Credentials, undefined, _) ->
    {error, password_undefined};
check(#{client_id := ClientId}, Password, #state{hash_type = HashType}) ->
    case mnesia:dirty_read(?TAB, ClientId) of
        [] -> ignore;
        [#?TAB{password = Password1}] ->

            case Password1 =:= hash(Password, HashType) of
                true -> ok;
                false -> {error, password_error}
            end
        end.

description() ->
    "ClientId Authentication Module".

encrypted_data(Password) ->
    HashOpt = get_passwordhash_config(),
    HashType = proplists:get_value(hash_type, HashOpt, md5),
    hash(Password, HashType).
    
get_passwordhash_config() ->
    application:get_env(emqx_auth_clientid, password_hash, []).

hash(Password, HashType) ->
    emqx_passwd:hash(HashType, Password).
