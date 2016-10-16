PROJECT = emq_auth_clientid
PROJECT_DESCRIPTION = Authentication with ClientId/Password
PROJECT_VERSION = 2.0

BUILD_DEPS = emqttd
dep_emqttd = git https://github.com/emqtt/emqttd emq20

TEST_DEPS = cuttlefish
dep_cuttlefish = git https://github.com/emqtt/cuttlefish

ERLC_OPTS += +'{parse_transform, lager_transform}'

COVER = true

include erlang.mk

app.config::
	cuttlefish -l info -e etc/ -c etc/emq_auth_clientid.conf -i priv/emq_auth_clientid.schema -d data

