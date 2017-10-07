%%--------------------------------------------------------------------
%% Copyright (c) 2013-2017 EMQ Enterprise, Inc. (http://emqtt.io)
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

-module(emqx_auth_clientid_SUITE).

-compile(export_all).

-include_lib("emqx/include/emqx.hrl").

-include_lib("common_test/include/ct.hrl").

-record(mqtt_auth_clientid, {client_id, password}).

all() ->
    [{group, emqx_auth_clientid}].

groups() -> 
    [{emqx_auth_clientid, [sequence], 
      [emqx_auth_clientid_api, change_config]}].

init_per_suite(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    [start_apps(App, DataDir) || App <- [emqx, emqx_auth_clientid]],
    Config.

end_per_suite(_Config) ->
    application:stop(emqx_auth_clientid),
    application:stop(emqx).


emqx_auth_clientid_api(_Config) ->
    {atomic, ok} = emqx_auth_clientid:add_clientid(<<"emq_auth_clientid">>, <<"password">>),
    User1 = #mqtt_client{client_id = <<"emq_auth_clientid">>},
    [{mqtt_auth_clientid,<<"emq_auth_clientid">>,<<"password">>}] = 
    emqx_auth_clientid:lookup_clientid(<<"emq_auth_clientid">>),
    ok = emqx_access_control:auth(User1, <<"password">>),
    {atomic, ok} = emqx_auth_clientid:remove_clientid(<<"emq_auth_clientid">>),
    {error, clientid_not_found} = emqx_access_control:auth(User1, <<"password">>).

change_config(_Config) ->
    application:stop(emqx_auth_clientid),
    application:set_env(emqx_auth_clientid, client_list, [{"id", "password"}, {"dev:devid", "passwd2"}]),
    application:start(emqx_auth_clientid),
    User1 = #mqtt_client{client_id = <<"id">>},
    User2 = #mqtt_client{client_id = <<"dev:devid">>},
    ok = emqx_access_control:auth(User1, <<"password">>),
    ok = emqx_access_control:auth(User2, <<"passwd2">>).

start_apps(App, DataDir) ->
    Schema = cuttlefish_schema:files([filename:join([DataDir, atom_to_list(App) ++ ".schema"])]),
    Conf = conf_parse:file(filename:join([DataDir, atom_to_list(App) ++ ".conf"])),
    NewConfig = cuttlefish_generator:map(Schema, Conf),
    Vals = proplists:get_value(App, NewConfig, []),
    [application:set_env(App, Par, Value) || {Par, Value} <- Vals],
    application:ensure_all_started(App).

