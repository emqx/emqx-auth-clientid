PROJECT = emq_auth_clientid
PROJECT_DESCRIPTION = Authentication with ClientId/Password
PROJECT_VERSION = 2.1.1

BUILD_DEPS = emqttd cuttlefish
dep_emqttd = git https://github.com/emqtt/emqttd develop
dep_cuttlefish = git https://github.com/emqtt/cuttlefish

NO_AUTOPATCH = cuttlefish

ERLC_OPTS += +'{parse_transform, lager_transform}'

COVER = true

include erlang.mk

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emq_auth_clientid.conf -i priv/emq_auth_clientid.schema -d data

