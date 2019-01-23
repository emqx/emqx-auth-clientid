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

-module(emqx_auth_clientid_SUITE).

-compile(export_all).

-include_lib("emqx/include/emqx.hrl").

-include_lib("common_test/include/ct.hrl").

all() ->
    [{group, emqx_auth_clientid}].

groups() ->
    [{emqx_auth_clientid, [sequence], [emqx_auth_clientid_api, change_config]}].

init_per_suite(Config) ->
    [start_apps(App, {SchemaFile, ConfigFile}) ||
        {App, SchemaFile, ConfigFile}
            <- [{emqx, deps_path(emqx, "priv/emqx.schema"),
                       deps_path(emqx, "etc/emqx.conf")},
                {emqx_management, deps_path(emqx_management, "priv/emqx_management.schema"),
                                  deps_path(emqx_management, "etc/emqx_management.conf")},
                {emqx_auth_clientid, local_path("priv/emqx_auth_clientid.schema"),
                                     local_path("etc/emqx_auth_clientid.conf")}]],
    Config.

end_per_suite(_Config) ->
    application:stop(emqx_auth_clientid),
    application:stop((emqx_management)),
    application:stop(emqx).

% get_base_dir() ->
%     {file, Here} = code:is_loaded(?MODULE),
%     filename:dirname(filename:dirname(Here)).

deps_path(App, RelativePath) ->
    %% Note: not lib_dir because etc dir is not sym-link-ed to _build dir
    %% but priv dir is
    Path0 = code:priv_dir(App),
    Path = case file:read_link(Path0) of
               {ok, Resolved} -> Resolved;
               {error, _} -> Path0
           end,
    filename:join([Path, "..", RelativePath]).

local_path(RelativePath) ->
    % filename:join([get_base_dir(), RelativePath]).
    deps_path(emqx_auth_clientid, RelativePath).

start_apps(App, {SchemaFile, ConfigFile}) ->
    read_schema_configs(App, {SchemaFile, ConfigFile}),
    set_special_configs(App),
    application:ensure_all_started(App).

read_schema_configs(App, {SchemaFile, ConfigFile}) ->
    ct:pal("Read configs - SchemaFile: ~p, ConfigFile: ~p", [SchemaFile, ConfigFile]),
    Schema = cuttlefish_schema:files([SchemaFile]),
    Conf = conf_parse:file(ConfigFile),
    NewConfig = cuttlefish_generator:map(Schema, Conf),
    Vals = proplists:get_value(App, NewConfig, []),
    [application:set_env(App, Par, Value) || {Par, Value} <- Vals].

set_special_configs(emqx) ->
    application:set_env(emqx, allow_anonymous, false),
    application:set_env(emqx, enable_acl_cache, false),
    application:set_env(emqx, plugins_loaded_file,
                        deps_path(emqx, "test/emqx_SUITE_data/loaded_plugins"));
set_special_configs(_App) ->
    ok.

emqx_auth_clientid_api(_Config) ->
    {atomic, ok} = emqx_auth_clientid:add_clientid(<<"emq_auth_clientid">>, <<"password">>),
    User1 = #{client_id => <<"emq_auth_clientid">>},
    [{emqx_auth_clientid,<<"emq_auth_clientid">>, _}] =
    emqx_auth_clientid:lookup_clientid(<<"emq_auth_clientid">>),
    ok = emqx_access_control:authenticate(User1, <<"password">>),
    emqx_access_control:authenticate(User1, <<"password">>),
    ok = emqx_auth_clientid:remove_clientid(<<"emq_auth_clientid">>),
    {error, _} = emqx_access_control:authenticate(User1, <<"password">>).

change_config(_Config) ->
    application:stop(emqx_auth_clientid),
    application:set_env(emqx_auth_clientid, client_list,
                        [{"id1", "password1"}, {"dev:devid", "passwd2"}]),
    ok = application:start(emqx_auth_clientid),
    User1 = #{client_id => <<"id1">>},
    User2 = #{client_id => <<"dev:devid">>},
    ok = emqx_access_control:authenticate(User1, <<"password1">>),
    {error, password_error} = emqx_access_control:authenticate(User1, <<"password00">>),
    ok = emqx_access_control:authenticate(User2, <<"passwd2">>),
    %% clean data
    ok = emqx_auth_clientid:remove_clientid(<<"id1">>),
    ok = emqx_auth_clientid:remove_clientid(<<"dev:devid">>).
