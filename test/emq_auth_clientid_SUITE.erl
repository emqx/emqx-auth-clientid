-module(emq_auth_clientid_SUITE).

-compile(export_all).

-include_lib("emqttd/include/emqttd.hrl").

-include_lib("common_test/include/ct.hrl").

-record(mqtt_auth_clientid, {client_id, password}).

all() ->
    [{group, emq_auth_clientid}].

groups() -> 
    [{emq_auth_clientid, [sequence], 
      [emq_auth_clientid_api, change_config]}].

init_per_suite(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    [start_apps(App, DataDir) || App <- [emqttd, emq_auth_clientid]],
    Config.

end_per_suite(_Config) ->
    application:stop(emq_auth_clientid),
    application:stop(emqttd).


emq_auth_clientid_api(_Config) ->
    {atomic, ok} = emq_auth_clientid:add_clientid(<<"emq_auth_clientid">>, <<"password">>),
    User1 = #mqtt_client{client_id = <<"emq_auth_clientid">>},
    [{mqtt_auth_clientid,<<"emq_auth_clientid">>,<<"password">>}] = 
    emq_auth_clientid:lookup_clientid(<<"emq_auth_clientid">>),
    ok = emqttd_access_control:auth(User1, <<"password">>),
    {atomic, ok} = emq_auth_clientid:remove_clientid(<<"emq_auth_clientid">>),
    ok = emqttd_access_control:auth(User1, <<"password">>).

change_config(_Config) ->
    application:stop(emq_auth_clientid),
    application:set_env(emq_auth_clientid, client_list, [{"id", "password"}, {"dev:devid", "passwd2"}]),
    application:start(emq_auth_clientid),
    User1 = #mqtt_client{client_id = <<"id">>},
    User2 = #mqtt_client{client_id = <<"dev:devid">>},
    ok = emqttd_access_control:auth(User1, <<"password">>),
    ok = emqttd_access_control:auth(User2, <<"passwd2">>).

start_apps(App, DataDir) ->
    Schema = cuttlefish_schema:files([filename:join([DataDir, atom_to_list(App) ++ ".schema"])]),
    Conf = conf_parse:file(filename:join([DataDir, atom_to_list(App) ++ ".conf"])),
    NewConfig = cuttlefish_generator:map(Schema, Conf),
    Vals = proplists:get_value(App, NewConfig, []),
    [application:set_env(App, Par, Value) || {Par, Value} <- Vals],
    application:ensure_all_started(App).

