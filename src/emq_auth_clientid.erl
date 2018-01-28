%%--------------------------------------------------------------------
%% Copyright (c) 2013-2018 EMQ Enterprise, Inc. (http://emqtt.io)
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
%%--------------------------------------------------------------------

-module(emq_auth_clientid).

-behaviour(emqttd_auth_mod).

-include_lib("emqttd/include/emqttd.hrl").

-export([add_clientid/2, lookup_clientid/1, remove_clientid/1, all_clientids/0]).

%% emqttd_auth_mod callbacks
-export([init/1, check/3, description/0]).

-define(AUTH_CLIENTID_TAB, mqtt_auth_clientid).

-record(?AUTH_CLIENTID_TAB, {client_id, password}).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

%% @doc Add clientid with password
-spec(add_clientid(binary(), binary()) -> {atomic, ok} | {aborted, any()}).
add_clientid(ClientId, Password) ->
    R = #mqtt_auth_clientid{client_id = ClientId, password = Password},
    mnesia:transaction(fun mnesia:write/1, [R]).

%% @doc Lookup clientid
-spec(lookup_clientid(binary()) -> list(#mqtt_auth_clientid{})).
lookup_clientid(ClientId) ->
    mnesia:dirty_read(?AUTH_CLIENTID_TAB, ClientId).

%% @doc Lookup all clientids
-spec(all_clientids() -> list(binary())).
all_clientids() -> mnesia:dirty_all_keys(?AUTH_CLIENTID_TAB).

%% @doc Remove clientid
-spec(remove_clientid(binary()) -> {atomic, ok} | {aborted, any()}).
remove_clientid(ClientId) ->
    mnesia:transaction(fun mnesia:delete/1, [{?AUTH_CLIENTID_TAB, ClientId}]).

%%--------------------------------------------------------------------
%% emqttd_auth_mod callbacks
%%--------------------------------------------------------------------

init(ClientList) ->
    ok = ekka_mnesia:create_table(?AUTH_CLIENTID_TAB, [
            {disc_copies, [node()]},
            {attributes, record_info(fields, ?AUTH_CLIENTID_TAB)}]),
    ok = ekka_mnesia:copy_table(?AUTH_CLIENTID_TAB, disc_copies),
    Clients = [r(ClientId, Password) || {ClientId, Password} <- ClientList],
    mnesia:transaction(fun() -> [mnesia:write(C) || C <- Clients] end),
    {ok, []}.

r(ClientId, Password) ->
    #mqtt_auth_clientid{client_id = iolist_to_binary(ClientId),
                        password  = iolist_to_binary(Password)}.

check(#mqtt_client{client_id = undefined}, _Password, _) ->
    {error, clientid_undefined};
check(_Client, undefined, _) ->
    {error, password_undefined};
check(#mqtt_client{client_id = ClientId}, Password, _) ->
    case mnesia:dirty_read(?AUTH_CLIENTID_TAB, ClientId) of
        [] -> ignore;
        [#?AUTH_CLIENTID_TAB{password = Password}]  -> ok; %% TODO: plaintext??
        _ -> {error, password_error}
    end.

description() -> "ClientId authentication module".

