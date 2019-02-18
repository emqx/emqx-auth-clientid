%% Copyright (c) 2013-2019 EMQ Technologies Co., Ltd. All Rights Reserved.
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

% CLI callbacks
-export([cli/1]).
-export([is_enabled/0]).
-export([add_clientid/2, update_clientid/2, lookup_clientid/1, remove_clientid/1, all_clientids/0]).

%% emqx_auth_mod callbacks
-export([init/1, check/3, description/0]).

-define(TAB, ?MODULE).
-record(?TAB, {client_id, password}).
-record(state, {hash_type}).

%-----------------------------------------------------------------------------
% CLI
%-----------------------------------------------------------------------------

cli(["list"]) ->
    if_enabled(fun() ->
        ClientIds = mnesia:dirty_all_keys(?TAB),
        [emqx_cli:print("~s~n", [ClientId]) || ClientId <- ClientIds]
    end);

cli(["add", ClientId, Password]) ->
    if_enabled(fun() ->
        Ok = add_clientid(iolist_to_binary(ClientId), iolist_to_binary(Password)),
        emqx_cli:print("~p~n", [Ok])
    end);

cli(["update", ClientId, NewPassword]) ->
    if_enabled(fun() ->
        Ok = update_clientid(iolist_to_binary(ClientId), iolist_to_binary(NewPassword)),
        emqx_cli:print("~p~n", [Ok])
    end);

cli(["del", ClientId]) ->
    if_enabled(fun() ->
        emqx_cli:print("~p~n", [remove_clientid(iolist_to_binary(ClientId))])
    end);

cli(_) ->
    emqx_cli:usage([{"clientid list", "List ClientId"},
                    {"clientid add <ClientId> <Password>", "Add ClientId"},
                    {"clientid update <Clientid> <NewPassword>", "Update Clientid"},
                    {"clientid del <ClientId>", "Delete ClientId"}]).

if_enabled(Fun) ->
    case is_enabled() of true -> Fun(); false -> hint() end.

hint() ->
    emqx_cli:print("Please './bin/emqx_ctl plugins load emqx_auth_clientid' first.~n").

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

is_enabled() ->
    lists:member(?TAB, mnesia:system_info(tables)).

%% @doc Add clientid with password
-spec(add_clientid(binary(), binary()) -> {atomic, ok} | {aborted, any()}).
add_clientid(ClientId, Password) ->
    Client = #?TAB{client_id = ClientId, password = encrypted_data(Password)},
    ret(mnesia:transaction(fun do_add_clientid/1, [Client])).

do_add_clientid(Client = #?TAB{client_id = ClientId}) ->
    case mnesia:read(?TAB, ClientId) of
        [] -> mnesia:write(Client);
        [_|_] -> mnesia:abort(exitsted)
    end.

%% @doc Update clientid with newpassword
-spec(update_clientid(binary(), binary()) -> {atomic, ok} | {aborted, any()}).
update_clientid(ClientId, NewPassword) ->
    Client = #?TAB{client_id = ClientId, password = encrypted_data(NewPassword)},
    ret(mnesia:transaction(fun do_update_clientid/1, [Client])).

do_update_clientid(Client = #?TAB{client_id = ClientId}) -> 
    case mnesia:read(?TAB, ClientId) of
        [_|_] -> mnesia:write(Client);
        [] -> mnesia:abort(noexitsted)
    end.

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
    Clients = [r(ClientId, Password) || {ClientId, Password} <- ClientList],
    mnesia:transaction(fun() -> [mnesia:write(C) || C <- Clients] end),
    {ok, State}.

r(ClientId, Password) ->
    #?TAB{client_id = iolist_to_binary(ClientId),
          password  = encrypted_data(iolist_to_binary(Password))}.

check(#{client_id := undefined}, _Password, _) ->
    {error, clientid_undefined};
check(_Credentials, undefined, _) ->
    {error, password_undefined};
check(#{client_id := Client}, Password, #state{hash_type = HashType}) ->
    case mnesia:dirty_read(?TAB, Client) of
        [] -> ignore;
        [#?TAB{password = <<Salt:4/binary, Hash/binary>>}] ->
            case Hash =:= hash(Password, Salt, HashType) of
                true -> ok;
                false -> {error, password_error}
            end
    end.

description() ->
    "ClientId Authentication Module".

encrypted_data(Password) ->
    HashType = application:get_env(emqx_auth_clientid, password_hash, sha256),
    SaltBin = salt(),
    <<SaltBin/binary, (hash(Password, SaltBin, HashType))/binary>>.

hash(Password, SaltBin, HashType) ->
    emqx_passwd:hash(HashType, <<SaltBin/binary, Password/binary>>).

salt() ->
    rand:seed(exsplus, erlang:timestamp()),
    Salt = rand:uniform(16#ffffffff), <<Salt:32>>.
