PROJECT = emq_auth_clientid
PROJECT_DESCRIPTION = Authentication with ClientId/Password
PROJECT_VERSION = 2.0.7

BUILD_DEPS = emqttd cuttlefish
dep_emqttd = git https://github.com/emqtt/emqttd master
dep_cuttlefish = git https://github.com/emqtt/cuttlefish

ERLC_OPTS += +'{parse_transform, lager_transform}'

COVER = true

include erlang.mk

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emq_auth_clientid.conf -i priv/emq_auth_clientid.schema -d data

