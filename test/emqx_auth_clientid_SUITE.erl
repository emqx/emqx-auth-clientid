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

-module(emqx_auth_clientid_SUITE).

-compile(export_all).

-include_lib("emqx/include/emqx.hrl").

-include_lib("common_test/include/ct.hrl").

-import(emqx_ct_http, [ request_api/3
                      , request_api/4
                      , request_api/5
                      , get_http_data/1
                      , create_default_app/0
                      , default_auth_header/0]).

-import(emqx_ct_helpers, [ start_apps/1
                         , stop_apps/1
                         , deps_path/2
                         ]).

-define(HOST, "http://127.0.0.1:8080/").

-define(API_VERSION, "v3").

-define(BASE_PATH, "api").

all() ->
    [{group, emqx_auth_clientid}].

groups() ->
    [{emqx_auth_clientid, [sequence], [emqx_auth_clientid_api, cli, t_http_api]}].

init_per_suite(Config) ->
    start_apps([emqx_auth_clientid, emqx_management]),
    application:set_env(emqx, allow_anonymous, false),
    application:set_env(emqx, enable_acl_cache, false),
    ekka_mnesia:start(),
    emqx_mgmt_auth:mnesia(boot),
    create_default_app(),
    Config.

end_per_suite(_Config) ->
    stop_apps([emqx_auth_clientid, emqx_management, emqx]),
    ekka_mnesia:ensure_stopped().

emqx_auth_clientid_api(_Config) ->
    ok = emqx_auth_clientid:add_clientid(<<"emq_auth_clientid">>, <<"password">>),
    User = #{client_id => <<"emq_auth_clientid">>,
             username => <<"user">>,
             password => <<"password">>},
    [{emqx_auth_clientid,<<"emq_auth_clientid">>, _}] =
    emqx_auth_clientid:lookup_clientid(<<"emq_auth_clientid">>),
    {ok, _} = emqx_access_control:authenticate(User),
    ok = emqx_auth_clientid:remove_clientid(<<"emq_auth_clientid">>),
    {error, _} = emqx_access_control:authenticate(User).

cli(_Config) ->
    [mnesia:dirty_delete({emqx_auth_clientid, ClientId}) ||  ClientId <- mnesia:dirty_all_keys(emqx_auth_clientid)],
    emqx_auth_clientid:cli(["add", "clientid", "password"]),
    [{emqx_auth_clientid, <<"clientid">>, <<Salt:4/binary, Hash/binary>>}] =
        emqx_auth_clientid:lookup_clientid(<<"clientid">>),
    HashType = application:get_env(emqx_auth_clientid, password_hash, sha256), 
    case Hash =:= emqx_passwd:hash(HashType, <<Salt/binary, <<"password">>/binary>>) of
        true -> ok;
        false -> ct:fail("password error")
    end,
    emqx_auth_clientid:cli(["update", "clientid", "newpassword"]),
    [{emqx_auth_clientid, <<"clientid">>, <<Salt1:4/binary, Hash1/binary>>}] =
        emqx_auth_clientid:lookup_clientid(<<"clientid">>),
    case Hash1 =:= emqx_passwd:hash(HashType, <<Salt1/binary, <<"newpassword">>/binary>>) of
        true -> ok;
        false -> ct:fail("password error")
    end,
    emqx_auth_clientid:cli(["del", "clientid"]),
    [] = emqx_auth_clientid:lookup_clientid(<<"clientid">>),
    emqx_auth_clientid:cli(["add", "user1", "pass1"]),
    emqx_auth_clientid:cli(["add", "user2", "pass2"]),
    UserList = emqx_auth_clientid:cli(["list"]),
    2 = length(UserList),
    emqx_auth_clientid:cli(usage).

t_http_api(_Config) ->
    [mnesia:dirty_delete({emqx_auth_clientid, ClientId}) ||  ClientId <- mnesia:dirty_all_keys(emqx_auth_clientid)],
    {ok, Result} = request_api(get, api_path(["auth_clientid"]), default_auth_header()),
    [] = get_http_data(Result),
    {ok, _} = request_api(post, api_path(["auth_clientid"]), [], default_auth_header(), [{<<"clientid">>, <<"clientid">>},
                                                                                         {<<"password">>, <<"password">>}]),
    {ok, Result1} = request_api(get, api_path(["auth_clientid", "clientid"]), default_auth_header()),
    [_, {<<"password">>, Hash}] = get_http_data(Result1),
    [{emqx_auth_clientid, <<"clientid">>, <<Salt:4/binary, _Hash/binary>>}] =
        emqx_auth_clientid:lookup_clientid(<<"clientid">>),
    HashType = application:get_env(emqx_auth_clientid, password_hash, sha256), 
    case Hash =:= emqx_passwd:hash(HashType, <<Salt/binary, <<"password">>/binary>>) of
        true -> ok;
        false -> ct:fail("password error")
    end,
    {ok, _} = request_api(put, api_path(["auth_clientid", "clientid"]), [], default_auth_header(), [{<<"password">>, <<"newpassword">>}]),
    [{emqx_auth_clientid, <<"clientid">>, <<Salt1:4/binary, Hash1/binary>>}] =
        emqx_auth_clientid:lookup_clientid(<<"clientid">>),
    HashType1 = application:get_env(emqx_auth_clientid, password_hash, sha256), 
    case Hash1 =:= emqx_passwd:hash(HashType1, <<Salt1/binary, <<"newpassword">>/binary>>) of
        true -> ok;
        false -> ct:fail("password error")
    end,
    {ok, _} = request_api(delete, api_path(["auth_clientid", "clientid"]), default_auth_header()),
    [] = emqx_auth_clientid:lookup_clientid(<<"clientid">>),
    ok.

api_path(Parts) ->
    ?HOST ++ filename:join([?BASE_PATH, ?API_VERSION] ++ Parts).
